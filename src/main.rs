use combinators::*;

#[cfg(test)]
mod tests;

mod combinators;

/// Represents a XML element.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// Parse a identifier (i.e. the name of a XML element).
///
/// It consists in a letter followed by any number of letters, digits or `-`
/// (can be seen as `[a-zA-Z][a-zA-Z0-9-]*`)
fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

/// Parse a string, containing any character but quotes, contained between
/// a pair of quotes.
fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

/// Parse a attribute pair `key="value"`, returns a tuple.
fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

/// Parse the list of attributes `key="value"`, returns a vector of tuples.
fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

/// For a XML element, parse the identifier and the list of attributes of
/// that element.
fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

/// Parse a XML "single" element (cannot have children).
///
/// Example:
/// ```xml
/// <single-element/>
/// ```
fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

/// Parse the opening element of a XML "parent" element.
///
/// Example: it is `<div>` in `<div></div>`.
fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

/// Parse the closing element of a XML "parent" element.
///
/// Example: it is `</div>` in `<div></div>`.
fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

/// Parse a XML "parent" element (can have children).
///
/// Example:
/// ```xml
/// <div>
///     <first-element/>
///     <second-element/>
/// </div>
/// ```
fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        left(zero_or_more(element()), close_element(el.name.clone())).map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

/// Parse a XML element (can be an entire document).
///
/// It can be a single element like this:
/// ```xml
/// <single-element/>
/// ```
///
/// Or a more complex one (with nested elements):
/// ```xml
/// <div>
///     <first-element/>
///     <second-element/>
/// </div>
/// ```
fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
}

/// Demonstration of the XML parser
fn main() {
    let doc = r#"
<top label="Top">
    <semi-bottom label="Bottom"/>
    <middle>
        <bottom label="Another bottom"/>
    </middle>
</top>"#;

    println!("{:#?}", element().parse(doc));
}
