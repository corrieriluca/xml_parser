/// Represents a result from a parser function. It is a [`Result`] that
/// has the tuple (`next_input: &str`, `OutputElement`) as Ok value and a string
/// representing the erroneous output as Err value.
pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

/// Represents a Parser function.
pub trait Parser<'a, Output> {
    /// Executes this parser against on given input.
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    /// Map the result of this parser with the given `map_fn` function.
    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    /// Reject as an Err the [`ParseResult`] of this parser if it does not
    /// match the given predicate `pred_fn`.
    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    /// Builds a parser that runs this parser, pass its result to a second
    /// parser, and runs this second parser. The final output of this parser
    /// is the ouptput of the second parser.
    ///
    /// It differs from `pair` because it does not return a tuple of the two
    /// results of the two parsers but just one.
    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

/// Implements the parser trait for every function that takes a string as input
/// and returns a ParseResult.
/// It allow to define your own parsers (like building blocks of a greater
/// parser) while still being able to use parser combinators later on them.
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// A [`BoxedParser`] is just a [`Parser`] in a [`Box`].
pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

// Combinators

/// Takes a parser and builds a parser that runs the parser and transforms
/// its result with a `map_fn` function.
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next, result)| (next, map_fn(result)))
    }
}

/// Takes a parser and builds a parser that runs the parser and reject or accept
/// its result (if it's an Ok result) based on a given predicate.
pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

/// Builds a parser that runs the given parser, pass its result to a second
/// parser, and runs this second parser. The final output of this parser
/// is the ouptput of the second parser.
///
/// It differs from `pair` because it does not return a tuple of the two
/// results of the two parsers but just one.
pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

/// Takes two parsers and returns a parser that runs the two parsers
/// sequentially, and returns a tuple of the results of the two parsers).
pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

/// Takes two parsers, runs them sequentially but returns only the result
/// of the first parser (the left one).
pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

/// Takes two parsers, runs them sequentially but returns only the result
/// of the second parser (the right one).
pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

/// Takes two parsers, runs the first one, if succeeded returns its result,
/// otherwise returns the result of the second one.
pub fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

/// Takes a parser and executes it zero or more times until it returns an Err
/// result. Returns a parser that returns a vector of results of the given
/// parser.
///
/// Similar as `*` in regular expressions.
pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

/// Takes a parser and executes it one or more times until it returns an Err
/// result. Returns a parser that returns a vector of results of the given
/// parser.
///
/// Similar as `?` in regular expressions.
pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

/// Builds a parser that accept an given string and consumes it.
pub fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// A parser that consumes one character, if there's still one left.
pub fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

// Whitespace characters utilities

/// Builds a parser that consumes one whitespace character, if there is one.
pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

/// Builds a parser that consume zero or more whitespace character.
pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

/// Builds a parser that consume one or more whitespace character.
pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

/// Builds a parser that discards the leading and the trailing whitespaces
/// of its input and parse that input with the given parser.
pub fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}
