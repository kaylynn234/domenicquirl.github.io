+++
title = "Where to Start Hand-Writing a Parser (in Rust)"
date = 2021-04-26
+++

I'm in [the Rust community Discord server](https://www.rust-lang.org/community).
Particularly, I hang around in their language development channel (regularly called `#lang-dev`, but its name is ever changing).
The folks in the server come with varying experience in Rust, and `#lang-dev` is frequented by Rustaceans of vastly different skill levels and knowledge backgrounds when it comes to actually working on a programming language.
In my experience, the Rust community is incredibly kind and usually glad to help out with questions.
Some questions however, especially beginner ones, keep coming up again and again.

Regarding parsing, there are a number of great resources already.
To mention a few, together with some guesses on why they might leave some of `#lang-dev`'s visitors' questions open (if you _are_ a beginner and some of these words don't mean anything to you yet, don't worry. We'll get there.):
 - [Crafting Interpreters](https://craftinginterpreters.com/) is an _absolutely fantastic_ read. Code examples are in Java and C however (also, the C compiler they develop is of very explicitly single-pass, which often makes it difficult for people to use it as guidance for their languages if the language design does not fit that model well. But that's less related to parsing). 
 - [Writing a Simple Parser in Rust](https://adriann.github.io/rust_parser.html) is very beginner-friendly, as it describes the author's own experience coming up with their parser. The parsing result is a homogenous syntax tree, which I personally like a lot, but, based on Rust Discord conversations, a strongly typed AST seems to be easier to conceptualize for many beginners because it gives you concrete _things_ from your language to talk about (like a `Function` or a `Variable`). More generally, the post is focused on parsing arithmetic expressions. 

   There is more to most languages than those, though, and dealing with precedence and associativity is often the source of a lot of confusion. The lexer implementation also vastly simplifies in this context, and is done by matching individual characters. That is by all means sufficient for the use case, but does not help beginners with lexing identifiers, escaped string literals, or even just floating point numbers (optionally with scientific notation), even less with handling conflicts between different classes of tokens (such as keywords which look like identifiers).
 - [Make a Language](https://arzg.github.io/lang/) is very detailed, but skips from lexer-less string-based parsing directly to using the [`logos`](https://crates.io/crates/logos) crate.

While I'm on the topic of other resources, [the `rust-langdev` repository](https://github.com/Kixiron/rust-langdev) is a collection of language development-related crates organized by category, also featuring a "Resources" section with further links on a bunch of topics. Go check it out!

## This Post is
 - An introduction to programming language parsing in which we hand-write a parser and run it on some real input.
 - A starting point, leaving lots of room for own experiments and further reading.
 - Intended as a collection of partial answers I have given in the Discord over time, to have a more comprehensive example and explanation to refer people to.
 - Accompanied by a public repository containing the full implementation.

## This Post is not
 - Conclusive. Several problems we are tackling here can be solved in multiple ways and I present only one place to start. 
 - A tutorial on writing production parsers, or an in-depth tutorial on any of the areas it covers really.
 - About parser generators. We will do all of the parsing by hand.

---
## Blog Repository

The result of this article is publicly available at [https://github.com/<wbr>domenicquirl/<wbr>blog/<wbr>parsing-<wbr>basics](https://github.com/domenicquirl/blog/parsing-basics).
The repository contains the final result of our parser implementation, including tests and benches.

---

## A High-Level View
By "parsing", we mean the process of transforming some input text, for example a source file of code in your language, into a _syntax tree_.

**Why transform the text?** Because working with individual characters gets really tedious very quickly, and also introducing additional _structure_ to, e.g., the input to a compiler for your language, is very useful to said compiler (or language server, or whatever you want to build) because it can operate on a _higher level of abstraction_.

**Why a tree?** Programs in most languages are already organized _hierarchically_.
Think about Rust: You have _crates_ (libraries or binaries), which can house multiple _modules_.
Each module can define an arbitrary number of _items_ such as _structs_, _traits_, _functions_ or _constants_.
A function is a sequence of multiple _statements_ like _variable assignments_, _loops_, _conditionals_ (`if`), etc[^stmt-expr]<span id="fn-stmt-expr"></span>.

Statements are subdivided further into their components, until at some point we reach some kind of "basic building blocks" of our language and can go no further.
For example, a variable assignment in Rust consists of the keyword `let`, a variable name, an equals sign `=`, an _expression_ that represents the new value of the variable, and a closing semicolon `;`.
The expression could be a _function call_, a combined expression like an addition of two numbers, or just a _literal_ (a literal is when you write an explicit value of some data type, like `3`, `"Hello World!"` or `Foo { bar: 2 }`).

Inside of a struct are its fields, but there is even more hierarchy that can be hidden in a struct definition.
Consider a generic `struct Foo<T, U>`.
The _type definition_ of this struct includes the struct's name (`Foo`), as well as a list of generic parameters.
If you place some restrictions on `T` or `U` with a `where` bound, that bound becomes part of the struct definition too!

Trees are exactly the structure to represent how a program in your language is built up from its basic blocks layer by layer.
You need somewhere to start, like a crate in Rust, but we will just start out with a single file which can contain multiple functions.
This starting point becomes the _root_ of the syntax tree.
When parsing a program, it's all about piecing together more and more parts of the tree, branching out every time a part of the program is made up of multiple smaller parts (so, always).
Below is an illustration of a syntax tree for a file that contains a function with a variable assignment:

{{ include_image(path="blog/parsing-basics/tree.png", class="horizontal") }}

### "Excuse me, there's a Lexer in your Parser"

The first point of confusion that commonly arises is that what is colloquially referred to as "parsing" quite often includes not one, but **two** components: a **lexer** and a **parser**.

{{ include_image(path="blog/parsing-basics/lexparse.png", class="vertical") }}

A lexer looks at the input string character by character and tries to group those characters together into something that at least has a meaning in your language.
We'll call such a group of characters a _token_.
Sometimes, a token will just be a single character.
A semicolon or an equals sign already mean something to you when you program, while the individual letters `"l"`, `"e"` and `"t"` probably don't in most contexts.
The lexer will recognize that sequence of characters as the `let` keyword and will put them in a group together as a single token.
Similarly, the lexer will produce a single "floating point number" token for the input `27.423e-12`.

The parser's job is then to take the meaningful tokens kindly created by the lexer and figure out their hierarchical structure to turn them into a syntax tree.
I've described most of the general idea above already, so let's finally _make_ one!

## Implementing our Lexer and Parser

We'll set up a new crate for our parsing experiments:<br>
`> cargo new --lib parsing-basics`<br><br>
Since we need tokens for the parser, we start with the lexer and make a `lexer` module, which in turn has a `token` module.
In there, we make an `enum` of the kinds of tokens we will have:

```rust
// In token.rs

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenKind {
    // Single characters
    Plus,
    Minus,
    Times,
    Slash,
    Pow,
    Eq,
    Dot,
    Comma,
    Underscore,
    Bang,
    Ampersand,
    Bar,
    Colon,
    SemiColon,
    // Brackets
    LAngle,
    RAngle,
    LSquare,
    RSquare,
    LBrace,
    RBrace,
    LParen,
    RParen,
    // Multiple characters
    String,
    Comment,
    Int,
    Float,
    Identifier,
    KeywordLet,
    KeywordFn,
    KeywordStruct,
    KeywordIf,
    KeywordElse,
    // Operators
    And,
    Or,
    Eqq,
    Neq,
    Geq,
    Leq,
    // Misc,
    Error,
    Whitespace,
    Eof,
}
```
You can see we have a lot of single-character tokens in there (including left and right brackets of all types), and then the groupings of strings and comments, numbers, and identifiers and keywords, as well as kinds to group things like `&&` or `!=` together.
We also have the `Error` kind in case we see a character we don't understand, and the `Eof` kind, which stands for "end of file" and will be the last token produced by the lexer.
Next, we will do something that will come in handy a lot during our implementation: we will define a macro for referencing token kinds:
```rust
#[macro_export]
macro_rules! T {
    [+] => {
        $crate::lexer::TokenKind::Plus
    };
    [-] => {
        $crate::lexer::TokenKind::Minus
    };
    [*] => {
        $crate::lexer::TokenKind::Times
    };
    [/] => {
        $crate::lexer::TokenKind::Slash
    };
    [^] => {
        $crate::lexer::TokenKind::Pow
    };
    [=] => {
        $crate::lexer::TokenKind::Eq
    };
    [.] => {
        $crate::lexer::TokenKind::Dot
    };
    [,] => {
        $crate::lexer::TokenKind::Comma
    };
    [_] => {
        $crate::lexer::TokenKind::Underscore
    };
    [!] => {
        $crate::lexer::TokenKind::Bang
    };
    [&] => {
        $crate::lexer::TokenKind::Ampersand
    };
    [|] => {
        $crate::lexer::TokenKind::Bar
    };
    [:] => {
        $crate::lexer::TokenKind::Colon
    };
    [;] => {
        $crate::lexer::TokenKind::SemiColon
    };
    [<] => {
        $crate::lexer::TokenKind::LAngle
    };
    [>] => {
        $crate::lexer::TokenKind::RAngle
    };
    ['['] => {
        $crate::lexer::TokenKind::LSquare
    };
    [']'] => {
        $crate::lexer::TokenKind::RSquare
    };
    ['{'] => {
        $crate::lexer::TokenKind::LBrace
    };
    ['}'] => {
        $crate::lexer::TokenKind::RBrace
    };
    ['('] => {
        $crate::lexer::TokenKind::LParen
    };
    [')'] => {
        $crate::lexer::TokenKind::RParen
    };
    [string] => {
        $crate::lexer::TokenKind::String
    };
    [comment] => {
        $crate::lexer::TokenKind::Comment
    };
    [int] => {
        $crate::lexer::TokenKind::Int
    };
    [float] => {
        $crate::lexer::TokenKind::Float
    };
    [ident] => {
        $crate::lexer::TokenKind::Identifier
    };
    [let] => {
        $crate::lexer::TokenKind::KeywordLet
    };
    [fn] => {
        $crate::lexer::TokenKind::KeywordFn
    };
    [struct] => {
        $crate::lexer::TokenKind::KeywordStruct
    };
    [if] => {
        $crate::lexer::TokenKind::KeywordIf
    };
    [else] => {
        $crate::lexer::TokenKind::KeywordElse
    };
    [&&] => {
        $crate::lexer::TokenKind::And
    };
    [||] => {
        $crate::lexer::TokenKind::Or
    };
    [==] => {
        $crate::lexer::TokenKind::Eqq
    };
    [!=] => {
        $crate::lexer::TokenKind::Neq
    };
    [>=] => {
        $crate::lexer::TokenKind::Geq
    };
    [<=] => {
        $crate::lexer::TokenKind::Leq
    };
    [error] => {
        $crate::lexer::TokenKind::Error
    };
    [ws] => {
        $crate::lexer::TokenKind::Whitespace
    };
    [EOF] => {
        $crate::lexer::TokenKind::Eof
    };
}
```
This is a long list, but from now on we can for example refer to the "less or equal comparison" token kind as `T![<=]`.
Not only does that save a lot of typing, in my opinion it is also a lot to read.
The first use of our new macro will be implementing a nice `Display` representation for `TokenKind`:
```rust
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Single characters
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T![^] => "^",
                T![=] => "=",
                T![.] => ".",
                T![,] => ",",
                T![_] => "_",
                T![!] => "!",
                T![&] => "&",
                T![|] => "|",
                T![:] => ":",
                T![;] => ";",
                // Brackets
                T![<] => "<",
                T![>] => ">",
                T!['['] => "[",
                T![']'] => "]",
                T!['{'] => "{",
                T!['}'] => "}",
                T!['('] => "(",
                T![')'] => ")",
                // Multiple characters
                T![string] => "String",
                T![comment] => "// Comment",
                T![int] => "Int",
                T![float] => "Float",
                T![ident] => "Identifier",
                T![let] => "let",
                T![fn] => "fn",
                T![struct] => "struct",
                T![if] => "if",
                T![else] => "else",
                // Operators
                T![&&] => "&&",
                T![||] => "||",
                T![==] => "==",
                T![!=] => "!=",
                T![>=] => ">=",
                T![<=] => "<=",
                // Misc 
                T![error] => "<?>",
                T![ws] => "<WS>",
                T![EOF] => "<EOF>",
            }
        )
    }
}
```
Again we've got ourselves a small wall of text, but we can now add a small test to check everything works as it should so far:
```rust 
#[cfg(test)]
mod tests {
    #[test]
    fn token_kind_display() {
        assert_eq!(T![+].to_string(), "+");
        assert_eq!(T![<=].to_string(), "<=");
        assert_eq!(T![let].to_string(), "let");
        assert_eq!(T![error].to_string(), "<?>");
        assert_eq!(T![comment].to_string(), "// Comment");
    }
}
```

### Tokens

We can now go to define our tokens.
They will store the kind of token, of course, as one of the `TokenKind`s we just defined.
The other thing we'll want to know from our tokens is, well, _what_ they are.
For example, if in the parser we see a token that we did not expect, we want to produce an error, which benefits a lot from including in the error message what the user actually typed.

One option would be to use Rust's amazing enums and include things like the name of an identifier or numbers in their respective variants.
I personally don't like this approach as much, because it makes tokens be all over the place - some have just their kind, some have an additional string, some have a number, ...
What about just including the the string of a token in _all_ of the tokens?
This would make the tokens equal again, but also we'd have to take all of these tiny strings out of the string we already have - the input string.
Strings mean allocations, and with strings our tokens will not be `Copy`.

Instead, we will make use of _spans_.
A span is simply two positions representing the start and the end of the token in the input string.
Rust has a type like this in its standard library: `Range`.
However, `Range` has some quirks that make it less nice to work with than I would like (in particular, for reasons that have no place in this post, it is also not `Copy` even if you make only a `Range<usize>`).
Let's thus make our own small `Span` type that can be converted to and from `Range<usize>`:
```rust
// In token.rs

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end:   u32,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end:   range.end as u32,
        }
    }
}
```
We can also add the ability to directly index strings with our spans:
```rust
impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}
```
Our tokens will then have a `TokenKind` and a `Span` and, given the input string, will be able to return the text they represent via the span:
```rust
#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}
```
We'll make `Token`'s `Display` forward to its `kind`, but let its `Debug` also show the span:
```rust
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} - <{}, {}>", self.kind, self.span.start, self.span.end)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
```

### Lexer Rules

Now, let's talk about how to compute the next token for some input.
For the single-character tokens, that seems quite easy - we see a `+`, we make a plus token.
But what about longer and more complicated tokens, like floating point numbers?
And what the next few characters could be _multiple_ kinds of token?
Seeing a `=` character, we would classify that as an "equals sign" token _except_ there might be another `=` following it, in which case maybe it's a comparison operator (`T![==]`)?
Or maybe it's just two equals signs next to each other? 
When we see `let`, how do we know it's a keyword and not the name of a variable?

If we look at some common lexer and parser generators to see how they have you write down parsing rules (look, just because we're not using one, doesn't mean we can't take a peek, eh?), we find a large variety of regular expressions.
Now, I may be fine with using regular expressions for the more complex tokens, but for something as simple as `+` they do seem a bit overkill.
Also, these generators have the advantage that they can optimize the regular expressions of all tokens together, which I will not do by hand in this blog post (or probably ever).
Let start with the simple cases and work our way up.
In our `lexer` mod, we create a (for now fairly uninteresting) `Lexer` struct and give it a method to lex a single token:
```rust
// In lexer/mod.rs

pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Self {}
    }

    /// Returns `None` if the lexer cannot find a token at the start of `input`.
    fn valid_token(&self, input: &str) -> Option<Token> {
        let next = input.chars().next().unwrap();
        let (len, kind) = if let Some(kind) = unambiguous_single_char(next) {
            (1, kind)
        } else {
            return None;
        };

        Some(Token {
            kind,
            // We will fix this later
            span: Span { start: 0, end: len },
        })
    }
}
```
We will put all the lexer rules in a separate file, so we'll implement `unambiguous_single_char` in a new `lexer` module `rules`:
```rust
// In lexer/rules.rs

/// If the given character is a character that _only_ 
/// represents a token of length 1,
/// this method returns the corresponding `TokenKind`.
/// Note that this method will return `None` for characters 
/// like `=` that may also occur at the first position 
/// of longer tokens (here `==`).
pub(crate) const fn unambiguous_single_char(c: char) -> Option<TokenKind> {
    Some(match c {
        '+' => T![+],
        '-' => T![-],
        '*' => T![*],
        '^' => T![^],
        '.' => T![.],
        ',' => T![,],
        '[' => T!['['],
        ']' => T![']'],
        '{' => T!['{'],
        '}' => T!['}'],
        '(' => T!['('],
        ')' => T![')'],
        ':' => T![:],
        ';' => T![;],
        _ => return None,
    })
}
```
The method is essentially the revers of the `Display` implementation, but only for tokens that are one character long _and cannot be the start of anything else_.
So it includes `+` and most of the brackets, but, for example, it does not include `=`, because of the possible `==`, and `/`, because that can also be the start of a comment.
Angle brackets are absent because they can also be the start of `<=` and `>=`[^shift-ops]<span id="fn-shift-ops"></span>.

We can also start thinking about what to do when the user inputs something we don't know (yet).
If we can't make a token at the start of the input, we'll look ahead until we can and emit an `Error` token for the characters we've had to skip over:
```rust
pub fn next_token(&self, input: &str) -> Token {
    self.valid_token(input).unwrap_or_else(|| self.invalid_token(input))
}

/// Always "succeeds", because it creates an error `Token`.
fn invalid_token(&self, input: &str) -> Token {
    let len = input
        .char_indices()
        .find(|(pos, _)| self.valid_token(&input[*pos..]).is_some())
        .map(|(pos, _)| pos)
        .unwrap_or_else(|| input.len());
    debug_assert!(len <= input.len());
    Token {
        kind: T![error],
        span: Span {
            start: 0,
            end:   len as u32,
        },
    }
}
```
At long last, we can write a function that works through an entire input string and converts it into tokens:
```rust
pub fn tokenize(&self, input: &str) -> Vec<Token> {
    let mut ret = Vec::new();
    let mut suffix = input;
    while !suffix.is_empty() {
        let token = self.next_token(suffix);
        ret.push(token);
        suffix = &suffix[token.len()..];
    }
    ret.push(Token {
        kind: T![EOF],
        span: Span {
            start: input.len() as u32,
            end:   input.len() as u32,
        },
    });
    ret
}
```
Let's create a small integration test for the tokens that should work already:
```rust
// In tests/it.rs

use parsing_basics::{lexer::*, T};

/// walks `$tokens` and compares them to the given kinds.
macro_rules! assert_tokens {
    ($tokens:ident, [$($kind:expr,)*]) => {
        {
            let mut it = $tokens.iter();
            $(
                let token = it.next().expect("not enough tokens");
                assert_eq!(token.kind, $kind);
            )*
        }
    };
}

#[test]
fn single_char_tokens() {
    let lexer = Lexer::new();
    let input = "+-(.):";
    let tokens = lexer.tokenize(input);
    assert_tokens!(tokens, [T![+], T![-], 
        T!['('], T![.], T![')'], T![:], T![EOF],]);
}

#[test]
fn unknown_input() {
    let lexer = Lexer::new();
    let input = "{$$$$$$$+";
    let tokens = lexer.tokenize(input);
    assert_tokens!(tokens, [T!['{'], T![error], T![+], T![EOF],]);
}
```

#### Making our Lexer an Iterator
While our lexer produces the correct _kinds_ of tokens, currently all tokens are created with the span `0..1`.
To fix that, we'll have to keep track of where the lexer is currently positioned in the input string.
We'll take this opportunity to have the lexer take a reference to the input string.
This means that it will now need to have a lifetime, but also has advantages - it lets us resolve tokens to their text through the lexer, and we can make the lexer an iterator:
```rust
// In lexer/mod.rs

pub struct Lexer<'input> {
    input:    &'input str,
    position: u32,
    eof:      bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { input, position: 0, eof: false }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }

    // ... unchanged
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position as usize >= self.input.len() {
            if self.eof {
                return None;
            }
            self.eof = true;
            Some(Token {
                kind: T![EOF],
                span: Span {
                    start: self.position,
                    end:   self.position,
                },
            })
        } else {
            Some(self.next_token(&self.input[self.position as usize..]))
        }
    }
}
```
At this point, we should be able to adapt our tests to pass the input to the lexer and they should pass as before.
Note that all of our `Lexer` methods will now take `&mut self`, because we have to update our `position`.
You'll have to make the `lexer` variables `mut` in the tests so everything keeps working.
```rust
// In tests/it.rs

#[test]
fn single_char_tokens() {
    let input = "+-(.):";
    let mut lexer = Lexer::new(input); // <- new
    let tokens = lexer.tokenize(); // <- removed `input`
    assert_tokens!(tokens, [T![+], T![-], 
        T!['('], T![.], T![')'], T![:], T![EOF],]);
}

#[test]
fn unknown_input() {
    let input = "{$$$$$$$+";
    let mut lexer = Lexer::new(input); // <- new
    let tokens = lexer.tokenize(); // <- removed `input`
    assert_tokens!(tokens, [T!['{'], T![error], T![+], T![EOF],]);
}
```
Let's actually fix the spans now:
```rust
// In lexer/mod.rs

/// Returns `None` if the lexer cannot find a token at the start of `input`.
    fn valid_token(&mut self, input: &str) -> Option<Token> {
        let next = input.chars().next().unwrap();
        let (len, kind) = if let Some(kind) = unambiguous_single_char(next) {
            (1, kind)
        } else {
            return None;
        };

        // NEW!
        let start = self.position;
        self.position += len;
        Some(Token {
            kind,
            span: Span {
                start,
                end: start + len,
            },
        })
    }

    /// Always "succeeds", because it creates an error `Token`.
    fn invalid_token(&mut self, input: &str) -> Token {
        let start = self.position; // <- NEW!
        let len = input
            .char_indices()
            .find(|(pos, _)| self.valid_token(&input[*pos..]).is_some())
            .map(|(pos, _)| pos)
            .unwrap_or_else(|| input.len());
        debug_assert!(len <= input.len());

        // NEW!
        // Because `valid_token` advances our position, 
        // we need to reset it to after the errornous token.
        let len = len as u32;
        self.position = start + len;
        Token {
            kind: T![error],
            span: Span {
                start,
                end: start + len,
            },
        }
    }
```
We'll also add a small test for token spans:
```rust
// In tests/it.rs

#[test]
fn token_spans() {
    {
        let input = "+-(.):";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let dot = tokens[3];
        assert_eq!(dot.kind, T![.]);
        assert_eq!(dot.span, (3..4).into())
    }
    {
        let input = "{$$$$$$$+";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let error = tokens[1];
        assert_eq!(error.kind, T![error]);
        assert_eq!(error.span, (1..8).into())
    }
}
```

#### Whitespace

Whitespace is special enough for us to handle on its own, mainly because there will probably be a lot of it and it is also a class which can never conflict with anything else - either a character is whitespace, or it is not.
Successive whitespace characters are also grouped together into a single token:
```rust
// In lexer/mod.rs

/// Returns `None` if the lexer cannot find a token at the start of `input`.
fn valid_token(&mut self, input: &str) -> Option<Token> {
    let next = input.chars().next().unwrap();
    let (len, kind) = if next.is_whitespace() {
        (
            input
                .char_indices()
                .take_while(|(_, c)| c.is_whitespace())
                .last()
                .unwrap() // we know there is at least one whitespace character
                .0 as u32
                + 1,
            T![ws],
        )
    } else if let Some(kind) = unambiguous_single_char(next) {
        (1, kind)
    } else {
        return None;
    };

    // create the token, unchanged
}
```
We can copy one of our basic tests and add some whitespace to see this works:
```rust
// In tests/it.rs

#[test]
fn single_char_tokens_with_whitespace() {
    let input = "   + -  (.): ";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let leading_space = &tokens[0];
    assert_eq!(leading_space.kind, T![ws]);
    assert_eq!(leading_space.len(), 3);

    let space_after_minus = &tokens[4];
    assert_eq!(space_after_minus.kind, T![ws]);
    assert_eq!(space_after_minus.len(), 2);

    let trailing_space = &tokens[9];
    assert_eq!(trailing_space.kind, T![ws]);
    assert_eq!(trailing_space.len(), 1);

    let tokens: Vec<_> = tokens
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [T![+], T![-], T!['('], T![.], T![')'], T![:], T![EOF],]
    );
}
```

#### Other Rules

We've seen before that for the remaining tokens we need a general mechanism to determine which class they belong to.
We'll say that a general lexer rule is a function which returns if and how many input characters it could match to the token kind it is for and start with the remaining one- and two-character tokens and the keywords:
```rust
// In lexer/rules.rs

pub(crate) struct Rule {
    pub kind:    TokenKind,
    pub matches: fn(&str) -> Option<u32>,
}

fn match_single_char(input: &str, c: char) -> Option<u32> {
    input.chars().next()
        .and_then(|ch| if ch == c { Some(1) } else { None })
}

fn match_two_chars(input: &str, first: char, second: char) -> Option<u32> {
    if input.len() >= 2 {
        match_single_char(input, first)
            .and_then(|_| {
                match_single_char(&input[1..], second)
                    .map(|_| 2)
            })
    } else {
        None
    }
}

fn match_keyword(input: &str, keyword: &str) -> Option<u32> {
    input.starts_with(keyword)
        .then(|| keyword.len() as u32)
}

pub(crate) fn get_rules() -> Vec<Rule> {
    vec![
        Rule {
            kind:    T![!],
            matches: |input| match_single_char(input, '!'),
        },
        Rule {
            kind:    T![=],
            matches: |input| match_single_char(input, '='),
        },
        Rule {
            kind:    T![/],
            matches: |input| match_single_char(input, '/'),
        },
        Rule {
            kind:    T![_],
            matches: |input| match_single_char(input, '_'),
        },
        Rule {
            kind:    T![<],
            matches: |input| match_single_char(input, '<'),
        },
        Rule {
            kind:    T![>],
            matches: |input| match_single_char(input, '>'),
        },
        Rule {
            kind:    T![==],
            matches: |input| match_two_chars(input, '=', '='),
        },
        Rule {
            kind:    T![!=],
            matches: |input| match_two_chars(input, '!', '='),
        },
        Rule {
            kind:    T![&&],
            matches: |input| match_two_chars(input, '&', '&'),
        },
        Rule {
            kind:    T![||],
            matches: |input| match_two_chars(input, '|', '|'),
        },
        Rule {
            kind:    T![<=],
            matches: |input| match_two_chars(input, '<', '='),
        },
        Rule {
            kind:    T![>=],
            matches: |input| match_two_chars(input, '>', '='),
        },
        Rule {
            kind:    T![let],
            matches: |input| match_keyword(input, "let"),
        },
        Rule {
            kind:    T![fn],
            matches: |input| match_keyword(input, "fn"),
        },
        Rule {
            kind:    T![struct],
            matches: |input| match_keyword(input, "struct"),
        },
        Rule {
            kind:    T![if],
            matches: |input| match_keyword(input, "if"),
        },
        Rule {
            kind:    T![else],
            matches: |input| match_keyword(input, "else"),
        },
    ]
}
```
In the lexer, we plug in the new rules where the input is neither whitespace nor clearly a single character:
```rust
// In lexer/mod.rs

pub struct Lexer<'input> {
    input:    &'input str,
    position: u32,
    eof:      bool,
    rules:    Vec<Rule>, // <- NEW!
}


impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            position: 0,
            eof: false,
            rules: rules::get_rules(), // <- NEW!
        }
    }

    /// Returns `None` if the lexer cannot find a token at the start of `input`.
    fn valid_token(&mut self, input: &str) -> Option<Token> {
        let next = input.chars().next().unwrap();
        let (len, kind) = if next.is_whitespace() {
            // snip
        } else if let Some(kind) = unambiguous_single_char(next) {
            (1, kind)
        } else {
            self.rules
                .iter()
                // `max_by_key` returns the last element if multiple
                // rules match, but we want earlier rules to "win" 
                // against later ones
                .rev()
                .filter_map(|rule| Some(((rule.matches)(input)?, rule.kind)))
                .max_by_key(|&(len, _)| len)?
        };

        // create the token, unchanged
    }

    // ...remaining functions unchanged
}
```
If the simpler cases don't trigger, we iterate over all our rules and, for each `rule`, check if it `matches` the `input`.
We then select _the rule that matches the longest piece of the input_, that is, the most input characters.
This choice is commonly known as [the "maximal munch" principle](https://en.m.wikipedia.org/wiki/Maximal_munch) and makes it so two successive `=` become `==`[^max-munch]<span id="fn-max-munch"></span>.
Moreover, it is consistent with grouping a sequence of digits all together as an `Int`, or letters as an `Identifier` (which we'll do next).
Note also that we decide to resolve conflicts between tokens of _the same length_ by choosing the rule that was written first.
We will write the rules from least to most general, so things like identifiers will be plugged in at the back.

Speaking of identifiers, we'll make some quick tests for our new rules and then we'll finally handle them.
Here are the new tests:
```rust
// In tests/it.rs

#[test]
fn maybe_multiple_char_tokens() {
    let input = "&&=<=_!=||";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(tokens, [
        T![&&], T![=], T![<=], T![_], T![!=], T![||], T![EOF],
    ]);


#[test]
fn keywords() {
    let input = "if let = struct else fn";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize().into_iter()
        .filter(|t| t.kind != T![ws]).collect();
    assert_tokens!(tokens, [
        T![if], T![let], T![=], T![struct], T![else], T![fn], T![EOF],
    ]);
}
```

Let's get to the big boys.
For this post, we _will_ use regular expressions here.
This means we need to add the `regex` crate to the project.
Since we'll need to put our regexes somewhere, we also add the `lazy_static` crate.
While we're at it, we add `unindent` as a `dev-dependency` as well.
`unindent` is a small utility to, well, unindent text, to help us write a full test for the lexer after we've added the missing rules.
Because users of our lexer don't run the tests, they don't need to have `unindent`, which is why it doesn't go into the regular `dependencies`:
```toml
[dependencies]
regex = "1"
lazy_static = "1"

[dev-dependencies]
unindent = "0.1"
```
We add a matching function for regex-based rules which queries the regex (conveniently, `Regex::find` returns an `Option`) and the regexes themselves.
We then make new rules that use `match_regex` and extend our ruleset with them.
I skipped `T![int]` integer literals with the regexes and instead gave them their own little rule which works the same way we handle whitespace (it's still important to have a rule for this though, because integers and floats can conflict):
```rust
// In lexer/rules.rs

fn match_regex(input: &str, r: &Regex) -> Option<u32> {
    r.find(input).map(|regex_match| regex_match.end() as u32)
}

lazy_static! {
    static ref STRING_REGEX: Regex = 
        Regex::new(r#"^"((\\"|\\\\)|[^\\"])*""#).unwrap();
    static ref COMMENT_REGEX: Regex = 
        Regex::new(r#"^//[^\n]*\n"#).unwrap();
    static ref FLOAT_REGEX: Regex = 
        Regex::new(r#"^((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#).unwrap();
    static ref IDENTIFIER_REGEX: Regex = 
        Regex::new(r##"^([A-Za-z]|_)([A-Za-z]|_|\d)*"##).unwrap();
}

pub(crate) fn get_rules() -> Vec<Rule> {
    vec![
        // ...rules from before
        Rule {
            kind:    T![string],
            matches: move |input| match_regex(input, &STRING_REGEX),
        },
        Rule {
            kind:    T![comment],
            matches: move |input| match_regex(input, &COMMENT_REGEX),
        },
        Rule {
            kind:    T![int],
            matches: |input| {
                input
                    .char_indices()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .last()
                    .map(|(pos, _)| pos as u32 + 1)
            },
        },
        Rule {
            kind:    T![float],
            matches: |input| match_regex(input, &FLOAT_REGEX),
        },
        Rule {
            kind:    T![ident],
            matches: |input| match_regex(input, &IDENTIFIER_REGEX),
        },
    ]
}
```
Have a look at [the `regex` crate's documentation](https://docs.rs/regex/1.5.3/regex/#syntax) to learn how the regular expressions are specified.
I write them in _raw string literals_, which go from `r#"` to `"#`.
All regular expressions are _anchored_ with the starting `^`, which forces them to match the input from the start and excludes matches anywhere else later in the input.
Then, a string is a sequence of characters in quotation marks (`"`), optionally including an escaped quotation mark (`\"`) or backslash (`\\`).
A line comment starts with `//` and ends at the end of the line.
A floating point number is some digits, maybe followed by a period and more digits, or alternatively it may also start with the period.
It may be followed by `E` or `e`, an optional sign and more digits to allow scientific notation.
An identifier is any variable name, for which we require to start with a letter or underscore, and then also allow digits for the characters after the first.

Time to try it out!
We'll add two tests, a function and a struct definition:
```rust
// In tests/it.rs

#[test]
fn function() {
    let input = r#"
        // tests stuff
        fn test(var: Type, var2_: bool) {
            let x = "String content \" test" + 7 / 27.3e-2^4;
            let chars = x.chars();
            if let Some(c) = chars.next() {
                x = x + c;
            } else if !var2_ {
                x = x + ",";
            }
        }
    "#;
    let input = unindent(input);
    let mut lexer = Lexer::new(input.as_str());
    let tokens: Vec<_> = lexer.tokenize().into_iter()
        .filter(|t| t.kind != T![ws]).collect();
    assert_tokens!(tokens, [
        // comment
        T![comment], 
        // function signature
        T![fn], T![ident], T!['('], 
            T![ident], T![:], T![ident], T![,], 
            T![ident], T![:], T![ident], 
        T![')'], T!['{'], 
            // `x` assignment
            T![let], T![ident], T![=], T![string], T![+], T![int], 
                T![/], T![float], T![^], T![int], T![;], 
            // `chars` assignment
            T![let], T![ident], T![=], T![ident], 
                T![.], T![ident], T!['('], T![')'], T![;],
            // if
            T![if], T![let], T![ident], T!['('], T![ident], T![')'], T![=], 
                T![ident], T![.], T![ident], T!['('], T![')'], 
            T!['{'], 
                // `x` re-assignment
                T![ident], T![=], T![ident], T![+], T![ident], T![;],
            // else if
            T!['}'], T![else], T![if], T![!], T![ident], T!['{'], 
                // `x` re-assignment
                T![ident], T![=], T![ident], T![+], T![string], T![;], 
            T!['}'], // end if
        T!['}'], // end fn
        T![EOF],
    ]);
}

#[test]
fn struct_def() {
    let input = r#"
        struct Foo<T> {
            bar: Bar<T>,
        }
    "#;
    let input = unindent(input);
    let input = input.as_str();
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize().into_iter().filter(|t| t.kind != T![ws]).collect();
    assert_tokens!(tokens, [
        // struct definition/type
        T![struct], T![ident], T![<], T![ident], T![>], T!['{'], 
            // member `bar` of type `Bar<T>`
            T![ident], T![:], T![ident], T![<], T![ident], T![>],T![,], 
        T!['}'], // end struct
        T![EOF],
    ]);
    let bar = tokens[6];
    assert_eq!(bar.span, (20..23).into()); // unindented span
}
```
Note that the comment before the `fn test` is correctly recognized as a comment, while in the assignment to `x` there is a single `/` for division.
This is because the comment is longer than a single slash, so it wins against the single character rule.
The same happens for the floating point number in the same assignment.
Our keywords are also recognized correctly.
They do match as identifiers as well, but their rules are declared earlier than the identifier rule, so our lexer gives them precedence.

#### Some Source Text

We'll extend that last text with a few checks for identifiers to illustrate how to get back at the input string from a token:
```rust
#[test]
fn struct_def() {
    // `input`, `lexer` and `tokens`, unchanged

    let bar = tokens[6];
    assert_eq!(bar.span, (20..23).into()); 
    assert_eq!(bar.text(input), "bar"); // <- NEW!

    let foo = tokens[1];
    assert_eq!(foo.text(input), "Foo"); // <- NEW!
}
```

One lexer, done.<br><br>

### The Parser

Our next big task is to re-arrange the lexer tokens into a nice tree that represents our input program.
We'll need a little bit of setup, starting with defining the AST we want to parse into.
All of our parser-related stuff will go into a new `parser` module with submodules, like `ast`:
```rust
// In parser/ast.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    FnCall { fn_name: String, args: Vec<Expr> },
    PrefixOp { op: TokenKind, expr: Box<Expr> },
    InfixOp { op: TokenKind, lhs: Box<Expr>, rhs: Box<Expr> },
    PostfixOp { op: TokenKind, expr: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(usize),
    Float(f64),
    Str(String),
}
```
We have 3 kinds of literal, corresponding to `T![int]`, `T![float]` and `T![string]`.
Identifiers store their name, and function call expressions store the name of the function and what was passed as its arguments.
The arguments are themselves expressions, because you can call functions like `sin(x + max(y, bar.z))`.


Expressions with operators are categorized into three classes:
 - **Prefix** operators are unary operators that come before an expression, like in `-2.4` or `!this_is_a_bool`. 
 - **Postfix** operators are also unary operators, but those that come _after_ an expression. For us, this will only be the faculty operator `4!`.
 - **Infix** operators are the binary operators like `a + b` or `c ^ d`, where `a` and `c` would be the left-hand sides `lhs`, and `b` and `d` are the right-hand sides `rhs`.
This is not the only way to structure your AST.
Some people prefer having an explicit `Expr` variant for each operator, so you'd have `Expr::Add`, `Expr::Sub`, `Expr::Not` and so on.
If you use those then of course you don't have to store the kind of operator in the AST, but I'm going with the more generic approach here, both because I personally prefer it and because it means less copy-pasting and smaller code blocks for this article.

Note that the AST _discards_ some information: 
When calling a function `bar(x, 2)`, I have to write not only the name and arguments, but also parentheses and commas.
Whitespace is also nowhere to be found, and I can tell you already that we will not have any methods that handle comments (well, apart from figuring out how we don't have to deal with them).
This is a major reason why the AST is called the **abstract** syntax tree; we keep input data only if it matters to us.
For other application, like an IDE, those things that we throw away here may matter a great deal, e.g., to format a file or show documentation.
In such cases, other representations are often used, such as **concrete** syntax trees (CSTs) that retain a lot more information.

#### Parser Input

I can tell you from personal experience that having to manually skip over whitespace and comments everywhere in a parser is not fun.
When we made the lexer tests in the last section, we filtered out all such tokens from the lexer output before comparing them to what we expected, except where we specifically wanted to test the whitespace handling.
In principle, our parser will have a `Lexer` iterator inside and query it for new tokens when it needs them.
The `Iterator::filter` adapter that we used in the tests has the annoying property, however, that it is really hard to name, because its predicate (the function that decides what to filter out) is part of its type.
We will thus build our own small iterator around the lexer, which will filter the tokens for us:
```rust
// In parser/mod.rs

pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { lexer: Lexer::new(input) }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next_token = self.lexer.next()?;
            if !matches!(next_token.kind, T![ws] | T![comment]) {
                return Some(next_token);
            } // else continue
        }
    }
}
```
I don't think we've used the `matches!` macro before.
It is basically just a short form of writing
```rust
match next_token.kind {
    T![ws] | T![comment] => true,
    _ => false,
}
```
though I admit `!matches!` always looks kinda funny.
Our parser can then be built like this:
```rust
// In parser/mod.rs

pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input:  &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
}
```
The downside of the "lexer as iterator" approach is shining through a bit here, since we now have to carry around the `'input` lifetime and the `where` bound (and also we had to make `TokenIter` in the first place).
However, we'll mostly be able to forget about it for the actual parsing methods.

`Peekable` is an iterator adapter from the standard library, which fortunately can be named more easily than `Filter`.
It wraps an iterator and allows us to `peek()` inside.
This lets us look ahead one token to see what's coming, without removing the token from the iterator.
We will use this _a lot_.

Let's start with the basic methods of our parser:
```rust

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Get the source text of a token.
    pub fn text(&self, token: Token) -> &'input str {
        token.text(&self.input)
    }

    /// Look-ahead one token and see what kind of token it is.
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens.peek().map(|token| token.kind).unwrap_or(T![EOF])
    }

    /// Check if the next token is some `kind` of token.
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Get the next token.
    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Move forward one token in the input and check 
    /// that we pass the kind of token we expect.
    pub(crate) fn consume(&mut self, expected: TokenKind) {
        let token = self.next().expect(&format!(
            "Expected to consume `{}`, but there was no next token",
            expected
        ));
        assert_eq!(
            token.kind, expected,
            "Expected to consume `{}`, but found `{}`",
            expected, token.kind
        );
    }
}
```

To start building our parse tree, the first step will be expressions.
They'll end up occupying quite a bit of space, so they also go in their own module:
```rust
// In parser/expressions.rs

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expression(&mut self) -> ast::Expr {
        match self.peek() {
            _ => todo!()   
        }
    }
}
```
As you can see, we'll be using `peek()` to figure out what expression is coming our way.
We start with the most basic expression of them all: literals
```rust
lit @ T![int] | lit @ T![float] | lit @ T![string] => {
    let literal_text = {
        // the calls on `self` need to be split, because `next` takes `&mut self`
        // if `peek` is not `T![EOF]`, then there must be a next token
        let literal_token = self.next().unwrap();
        self.text(literal_token)
    };
    let lit = match lit {
        T![int] => ast::Lit::Int(
            literal_text
                .parse()
                .expect(&format!(
                    "invalid integer literal: `{}`", 
                    literal_text)
                ),
        ),
        T![float] => ast::Lit::Float(
            literal_text
                .parse()
                .expect(&format!(
                    "invalid floating point literal: `{}`", 
                    literal_text)
                ),
        ),
        T![string] => ast::Lit::Str(
            // trim the quotation marks
            literal_text[1..(literal_text.len() - 1)].to_string()
        ),
        _ => unreachable!(),
    };
    ast::Expr::Literal(lit)
}
```
I use two `match`es here so I can share the code for actions we have to take for all literals, which is resolving their text and creating an `ast::Expr::Literal` expression for them.
The `lit @ T![int]` syntax is something that you may not have come across before.
It gives a name to the kind that is matched, so that I can use it again in the second match.
The result is equivalent to calling `let lit = self.peek()` again at the start of the outer match.

In the inner `match`, we create the correct `ast::Lit` literal types depending on the type of the token.
It does feel a bit like cheating to use a function called `parse()` to implement our parser, but it's what the standard library gives us and I'm for sure not gonna write string-to-number conversion routines by hand for this post.
Even if I wanted to, other people have already done that work for me - for your implementation, you might also want to have look at [`lexical`](https://crates.io/crates/lexical) or even [`lexical_core`](https://crates.io/crates/lexical-core).

Next on the list are identifiers, which are interesting because they might just be a reference to a variable `bar`, but they might also be the start of a call to the function `bar(x, 2)`.
Our friend `peek()` (or, in this case, `at()`) will help us solve this dilemma again:
```rust
T![ident] => {
    let name = {
        let ident_token = self.next().unwrap();
        self.text(ident_token).to_string() // <- now we need a copy
    };
    if !self.at(T!['(']) {
        // plain identifier
        ast::Expr::Ident(name)
    } else {
        //  function call
        let mut args = Vec::new();
        self.consume(T!['(']);
        while !self.at(T![')']) {
            let arg = self.parse_expression();
            args.push(arg);
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        ast::Expr::FnCall { fn_name: name, args }
    }
}
```
If the token immediately after the identifier is an opening parenthesis, the expression becomes a function call.
Otherwise, it stays an sole identifier.
Remember that we filtered out any whitespace so the parenthesis will actually be the token right after the ident.

For the calls we loop for as long as there are arguments (until the parentheses get closed) and parse the argument expressions _recursively_.
This is what allows us to write `my_function(x + 4 * y, log(2*z))`, because the recursion will be able to parse any, full expression again.
In between the arguments we expect commas and at the end we skip over the closing paren and make an `ast::Expr::FnCall` node for the input.

Grouped expressions `(expr)` and prefix operators are fairly straightforward, because they also mostly call `parse_expression` recursively.
What is interesting about grouped expressions is that they will not need an extra type of node.
We only use the parentheses as boundaries of the expressions while parsing, but then the grouped expression _becomes_ the node for whatever is inside the parens:

```rust
T!['('] => {
    // There is no AST node for grouped expressions.
    // Parentheses just influence the tree structure.
    self.consume(T!['(']);
    let expr = self.parse_expression();
    self.consume(T![')']);
    expr
}
op @ T![+] | op @ T![-] | op @ T![!] => {
    self.consume(op);
    let expr = self.parse_expression();
    ast::Expr::PrefixOp {
        op,
        expr: Box::new(expr),
    }
}
```

Full code of `parse_expression` so far:
```rust
pub fn parse_expression(&mut self) -> ast::Expr {
    match self.peek() {
        lit @ T![int] | lit @ T![float] | lit @ T![string] => {
            let literal_text = {
                // if `peek` is not `T![EOF]`, then there must be a next token
                let literal_token = self.next().unwrap();
                self.text(literal_token)
            };
            let lit = match lit {
                T![int] => ast::Lit::Int(
                    literal_text
                        .parse()
                        .expect(&format!(
                            "invalid integer literal: `{}`", 
                            literal_text)
                        ),
                ),
                T![float] => ast::Lit::Float(
                    literal_text
                        .parse()
                        .expect(&format!(
                            "invalid floating point literal: `{}`", 
                            literal_text)
                        ),
                ),
                T![string] => ast::Lit::Str(
                    literal_text[1..(literal_text.len() - 1)].to_string()
                ),
                _ => unreachable!(),
            };
            ast::Expr::Literal(lit)
        }
        T![ident] => {
            let name = {
                let ident_token = self.next().unwrap();
                self.text(ident_token).to_string()
            };
            if !self.at(T!['(']) {
                // plain identifier
                ast::Expr::Ident(name)
            } else {
                //  function call
                let mut args = Vec::new();
                self.consume(T!['(']);
                while !self.at(T![')']) {
                    let arg = self.parse_expression();
                    args.push(arg);
                    if self.at(T![,]) {
                        self.consume(T![,]);
                    }
                }
                self.consume(T![')']);
                ast::Expr::FnCall { fn_name: name, args }
            }
        }
        T!['('] => {
            // There is no AST node for grouped expressions.
            // Parentheses just influence the tree structure.
            self.consume(T!['(']);
            let expr = self.parse_expression();
            self.consume(T![')']);
            expr
        }
        op @ T![+] | op @ T![-] | op @ T![!] => {
            self.consume(op);
            let expr = self.parse_expression();
            ast::Expr::PrefixOp {
                op,
                expr: Box::new(expr),
            }
        }
        kind => {
            panic!("Unknown start of expression: `{}`", kind);
        }
    }
}
```

What we have now is enough to write our first test for our new parser:
```rust
// In tests/it.rs

#[test]
fn parse_expression() {
    fn parse(input: &str) -> ast::Expr {
        let mut parser = Parser::new(input);
        parser.parse_expression()
    }

    // Weird spaces are to test that whitespace gets filtered out
    let expr = parse("42");
    assert_eq!(expr, ast::Expr::Literal(ast::Lit::Int(42)));
    let expr = parse("  2.7768");
    assert_eq!(expr, ast::Expr::Literal(ast::Lit::Float(2.7768)));
    let expr = parse(r#""I am a String!""#);
    assert_eq!(expr, ast::Expr::Literal(
        ast::Lit::Str("I am a String!".to_string())
    ));
    let expr = parse("foo");
    assert_eq!(expr, ast::Expr::Ident("foo".to_string()));
    let expr = parse("bar (  x, 2)");
    assert_eq!(
        expr,
        ast::Expr::FnCall {
            fn_name: "bar".to_string(),
            args:    vec![
                ast::Expr::Ident("x".to_string()), 
                ast::Expr::Literal(ast::Lit::Int(2)),
            ],
        }
    );
    let expr = parse("!  is_visible");
    assert_eq!(
        expr,
        ast::Expr::PrefixOp {
            op:   T![!],
            expr: Box::new(ast::Expr::Ident("is_visible".to_string())),
        }
    );
    let expr = parse("(-13)");
    assert_eq!(
        expr,
        ast::Expr::PrefixOp {
            op:   T![-],
            expr: Box::new(ast::Expr::Literal(ast::Lit::Int(13))),
        }
    );
}
```

#### Binary Operators

I'm gonna get a bit philosophical for this one, y'all ready?
Ahem.
_What really **is** a binary operator?_
Sure, it's an operator that goes in between to operands.
But, from a parsing perspective, it's a token that _extends_ an expression.

Imagine the input `-x + 3 * y ^ 2`.
With what we have now, we get as far as parsing `-x` as a unary operator on an identifier, because that's the biggest unit you can get without infix operators.
Seeing that the next token is a `+` tells us that the expression we are parsing is actually longer than that, and that after the `+` there should be another expression; the right-hand side of the addition.

In our first attempt at parsing binary operators, we will try to follow this view by adding an "operator loop" to `parse_expression`.
The entire `match` we have built so far becomes a potential left-hand side `lhs` of a binary operator, and after we parse it we check if the following token is an operator.
If so, we continue parsing its right-hand side and build a corresponding `ast::Expr::InfixOp` node:
```rust
// In parser/expressions.rs

pub fn parse_expression(&mut self) -> ast::Expr {
    let mut lhs = match self.peek() {
        // unchanged
    };
    loop {
        let op = match self.peek() {
            op @ T![+]
            | op @ T![-]
            | op @ T![*]
            | op @ T![/]
            | op @ T![^]
            | op @ T![==]
            | op @ T![!=]
            | op @ T![&&]
            | op @ T![||]
            | op @ T![<]
            | op @ T![<=]
            | op @ T![>]
            | op @ T![>=]
            | op @ T![!] => op,
            T![EOF] => break,
            T![')'] | T!['}'] | T![,] | T![;] => break,
            kind => panic!("Unknown operator: `{}`", kind),
        };

        self.consume(op);
        let rhs = self.parse_expression();
        lhs = ast::Expr::InfixOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
    }

    lhs
}
```
If we add a small test, we see that we can now parse longer, combined expressions.
To tests, we'll implement `Display` for our AST such that expressions are always put in parentheses:
```rust
// In parser/ast.rs

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::FnCall { fn_name, args } => {
                write!(f, "{}(", fn_name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
            Expr::PrefixOp { op, expr } => 
                write!(f, "({} {})", op, expr),
            Expr::InfixOp { op, lhs, rhs } => 
                write!(f, "({} {} {})", lhs, op, rhs),
            Expr::PostfixOp { op, expr } => 
                write!(f, "({} {})", expr, op),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::Str(s) => write!(f, r#""{}""#, s)
        }
    }
}
```

This test now passes:
```rust
// In tests/it.rs

#[test]
fn parse_binary_expressions() {
    fn parse(input: &str) -> ast::Expr {
        let mut parser = Parser::new(input);
        parser.parse_expression()
    }

    let expr = parse("4 + 2 * 3");
    assert_eq!(expr.to_string(), "(4 + (2 * 3))");
}
```
However, if we add some more expressions, we'll see that not all of them get parsed as we would expect to following our intuitions about maths:
```rust
#[test]
fn parse_binary_expressions() {
    fn parse(input: &str) -> ast::Expr {
        let mut parser = Parser::new(input);
        parser.parse_expression()
    }

    let expr = parse("4 + 2 * 3");
    assert_eq!(expr.to_string(), "(4 + (2 * 3))"); // passes

    let expr = parse("4 * 2 + 3");
    assert_eq!(expr.to_string(), "((4 * 2) + 3)"); // fails

    let expr = parse("4 - 2 - 3");
    assert_eq!(expr.to_string(), "((4 - 2) - 3)"); // fails

    let expr = parse("4 ^ 2 ^ 3");
    assert_eq!(expr.to_string(), "(4 ^ (2 ^ 3))"); // passes
}
```
Currently, we're extending the expression recursively unconditionally on seeing any operator.
Because the recursion happens after the operator, to parse the right-hand side, all our binary expressions are right-associative and ignore operator precedence rules like `*` being evaluated before `+`.
That said, this isn't all that surprising, giving that `parse_expression` currently has no way of knowing an operator's precedence.
Let's fix that:
```rust
// In parser/expressions.rs

trait Operator {
    /// Prefix operators bind their operand to the right.
    fn prefix_binding_power(&self) -> ((), u8);

    /// Infix operators bind two operands, lhs and rhs.
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left.
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            T![+] | T![-] | T![!] => ((), 51),
            // Prefixes are the only operators we have already seen
            // when we call this, so we know the token must be
            // one of the above
            _ => unreachable!("Not a prefix operator: {:?}", self),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
            T![||] => (1, 2),
            T![&&] => (3, 4),
            T![==] | T![!=] => (5, 6),
            T![<] | T![>] | T![<=] | T![>=] => (7, 8),
            T![+] | T![-] => (9, 10),
            T![*] | T![/] => (11, 12),
            T![^] => (22, 21), // <- This binds stronger to the left!
            _ => return None,
        };
        Some(result)
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        let result = match self {
            T![!] => (101, ()),
            _ => return None,
        };
        Some(result)
    }
}
```
For all operators, we define how tightly they bind to the left and to the right.
If the operator is a pre- or postfix operator, one of the directions is `()`.
The general idea is that the higher the binding power of an operator in some direction, the more it will try to take the operand on that side for itself and take it a way from other operators.
For example, in
```rust
    4  *   2   +  3
//   11 12    9 10
```
the binding power of `12` that `*` has to the right wins against the lower `9` that `+` has to the left, so the `2` gets associated with `*` and we get `(4 * 2) + 3`.
Most of the operators bind more tightly to the right than to the left.
This way we get
```rust
    4  -   2   -  3
//    9 10    9 10
```
the right way round as `(4 - 2) - 3` as the `10` that `-` has to the right wins against the `9` it has to the left.
For right-associative operators like `^`, we swap the higher binding power to the left:
```rust
    4  ^   2   ^  3
//   22 21   22 21
```
is grouped as `4 ^ (2 ^ 3)` as `22` wins against `21`.

How can we implement this into our `parse_expression`?
We'll need to know the right-sided binding power of the operator that triggered a recursion which parses a new right-hand side.
When we enter the operator loop, we check not only if the next token is an operator, but also its left-sided binding power.
Only if this binding power is at least as high as the current right-sided one do we recurse again, which associates the current expression with the _new, following_ operator.
Otherwise we stop and return, so the operator with the higher right-sided binding power gets the expression.

This is hard to wrap your head around the first time. Once you get it, it's great and you will never want to do anything else again to handle expression, but it needs to click first. 
[Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) by `matklad`, the man behind `rust-analyzer`, is a very good article on the topic if you'd like to get explained this again, in a different way.
In code, it looks like this:
```rust
// In parser/expressions.rs

pub fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
    let mut lhs = match self.peek() {
        lit @ T![int] | lit @ T![float] | lit @ T![string] => {
            // unchanged
        }
        T![ident] => {
            let name = {
                let ident_token = self.next().unwrap();
                self.text(ident_token).to_string()
            };
            if !self.at(T!['(']) {
                // plain identifier
                ast::Expr::Ident(name)
            } else {
                //  function call
                let mut args = Vec::new();
                self.consume(T!['(']);
                while !self.at(T![')']) {
                    let arg = self.parse_expression(0); // <- NEW!
                    args.push(arg);
                    if self.at(T![,]) {
                        self.consume(T![,]);
                    }
                }
                self.consume(T![')']);
                ast::Expr::FnCall { fn_name: name, args }
            }
        }
        T!['('] => {
            // There is no AST node for grouped expressions.
            // Parentheses just influence the tree structure.
            self.consume(T!['(']);
            let expr = self.parse_expression(0); // <- NEW!
            self.consume(T![')']);
            expr
        }
        op @ T![+] | op @ T![-] | op @ T![!] => {
            self.consume(op);
            let ((), right_binding_power) = op.prefix_binding_power(); 
            let expr = self.parse_expression(right_binding_power); // <- NEW!
            ast::Expr::PrefixOp {
                op,
                expr: Box::new(expr),
            }
        }
        kind => {
            panic!("Unknown start of expression: `{}`", kind);
        }
    };
    loop {
        let op = // unchanged;

        if let Some((left_binding_power, right_binding_power)) = 
            op.infix_binding_power() { // <- NEW!

            if left_binding_power < binding_power {
                // previous operator has higher binding power then new one
                // --> end of expression
                break; 
            }

            self.consume(op);
            let rhs = self.parse_expression(right_binding_power);
            lhs = ast::Expr::InfixOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            // parsed an operator --> go round the loop again
            continue; 
        }
        break; // Not an operator --> end of expression
    }

    lhs
}
```
Recursive calls for both prefix and infix operators get passed the right-sided binding power of the current operator.
The important stopping point is the `if left_binding_power < binding_power { break; }` inside the operator loop.
If `op`'s left-sided binding power does not at least match the required binding power of the current invocation, it does not get parsed in the loop.
It will instead get parsed some number of returns up the recursion stack, by an operator loop with sufficiently high binding power.
Function call arguments and grouped expressions in parentheses reset the required binding power to `0`, since they take precedence before any chain of operators.

For the interface of our parser, we add a small wrapper that will also call `parse_expression` with an initial binding power of `0`:
```rust
pub fn expression(&mut self) -> ast::Expr {
    self.parse_expression(0)
}
```
We need to swap that in in our expression parsing tests:
```rust
// In tests/it.rs, expression tests

fn parse(input: &str) -> ast::Expr {
    let mut parser = Parser::new(input);
    parser.expression()
}
```
The test that previously failed should now pass.
We'll add a few more complex expressions and test that they are also parsed correctly:
```rust
// In tests/it.rs

#[test]
fn parse_binary_expressions() {
    // ...unchanged

    let expr = parse(
        r#"45.7 + 3 + 5 * 4^8^9 / 6 > 4 && test - 7 / 4 == "Hallo""#
    );
    assert_eq!(
        expr.to_string(),
        r#"((((45.7 + 3) + ((5 * (4 ^ (8 ^ 9))) / 6)) > 4) && ((test - (7 / 4)) == "Hallo"))"#
    );

    let expr = parse("2.0 / ((3.0 + 4.0) * (5.0 - 6.0)) * 7.0");
    assert_eq!(expr.to_string(), "((2 / ((3 + 4) * (5 - 6))) * 7)");

    let expr = parse("min ( test + 4 , sin(2*PI ))");
    assert_eq!(expr.to_string(), "min((test + 4),sin((2 * PI),),)");
}
```
Postfix operators are now an easy addition:
```rust
// In parser/expresions.rs

pub fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
    let mut lhs = match self.peek() {
        // unchanged
    };
    loop {
        let op = // unchanged;

        // NEW!
        if let Some((left_binding_power, ())) = op.postfix_binding_power() { 
            if left_binding_power < binding_power {
                // previous operator has higher binding power then new one 
                // --> end of expression
                break;
            }

            self.consume(op);
            // no recursive call here, because we have already
            // parsed our operand `lhs`
            lhs = ast::Expr::PostfixOp {
                op,
                expr: Box::new(lhs),
            };
            // parsed an operator --> go round the loop again
            continue;
        }

        if let Some((left_binding_power, right_binding_power)) = 
            op.infix_binding_power() {
            // unchanged
        }

        break; // Not an operator --> end of expression
    }

    lhs
}
```
And to check it works:
```rust
// In tests/it.rs

#[test]
fn parse_postfix_op() {
    fn parse(input: &str) -> ast::Expr {
        let mut parser = Parser::new(input);
        parser.expression()
    }

    let expr = parse("4 + -2! * 3");
    assert_eq!(expr.to_string(), "(4 + ((- (2 !)) * 3))");
}
```

This will be the end of expressions for us.
If you want, try adding additional operators on your own.
Some language constructs that one might not necessarily think of as operators fit very cleanly into our framework.
For example, try adding `.` as an operator to model field accesses like `foo.bar`.
For a greater challenge, array indexing can be handled as a combination of postfix operator `[` and grouped expressions.
There's all kinds of expressions left for you to do, but we now have to move on to...

#### Statements
As a reward for our hard work on expressions, we are now allowed to parse anything that _indludes_ an expression.
The next level up from expressions are _statements_, of which we will consider variable definitions with `let` and re-assignments without `let`, as well as `if` statements (we'll also need a representation of explicit `{}` scopes):
```rust
// In parser/ast.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        var_name: String,
        value:    Box<Expr>,
    },
    Assignment {
        var_name: String,
        value:    Box<Expr>,
    },
    IfStmt {
        condition: Box<Expr>,
        body:      Vec<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    Block {
        stmts: Vec<Stmt>,
    },
}
```
We'll make a new module called `hierarchy` for statements and beyond and start the same way as for expressions:
```rust
// In parser/hierarchy.rs

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn statement(&mut self) -> ast::Stmt {
        match self.peek() {
            _ => todo!(),
        }
    }
}
```
The declaration and the `let` differ only by the `let` keyword:
```rust
T![let] => {
    self.consume(T![let]);
    let ident = self.next().expect("Expected identifier after `let`");
    assert_eq!(
        ident.kind,
        T![ident],
        "Expected identifier after `let`, but found `{}`",
        ident.kind
    );
    let name = self.text(ident).to_string();
    self.consume(T![=]);
    let value = self.expression();
    self.consume(T![;]);
    ast::Stmt::Let {
        var_name: name,
        value:    Box::new(value),
    }
}
T![ident] => {
    let ident = self.next().unwrap();
    let name = self.text(ident).to_string();
    self.consume(T![=]);
    let value = self.expression();
    self.consume(T![;]);
    ast::Stmt::Assignment {
        var_name: name,
        value:    Box::new(value),
    }
}
```
Note how we now just call `self.expression()` to parse the value assigned to the variable.
It will do all the expression parsing work for us and return us a nice `ast::Expr` to use in our `ast::Stmt`.
The `if` case is a bit more involved, because we need to handle the condition, the statements inside the `if` and also possible a `else`.
We can make our lives a bit easier by re-using the block scope statement for the body:
```rust
T![if] => {
    self.consume(T![if]);
    self.consume(T!['(']);
    let condition = self.expression();
    self.consume(T![')']);

    assert!(self.at(T!['{']), "Expected a block after `if` statement");
    let body = self.statement();
    let body = match body {
        ast::Stmt::Block { stmts } => stmts,
        _ => unreachable!(),
    };

    let else_stmt = if self.at(T![else]) {
        self.consume(T![else]);
        assert!(
            self.at(T![if]) || self.at(T!['{']),
            "Expected a block or an `if` after `else` statement"
        );
        Some(Box::new(self.statement()))
    } else {
        None
    };

    ast::Stmt::IfStmt {
        condition: Box::new(condition),
        body,
        else_stmt,
    }
}
T!['{'] => {
    self.consume(T!['{']);
    let mut stmts = Vec::new();
    while !self.at(T!['}']) {
        let stmt = self.statement();
        stmts.push(stmt);
    }
    self.consume(T!['}']);
    ast::Stmt::Block { stmts }
}
```
The statement test will be our longest test so far:
```rust
// In tests/it.rs

#[test]
fn parse_statements() {
    fn parse(input: &str) -> ast::Stmt {
        let mut parser = Parser::new(input);
        parser.statement()
    }

    let stmt = parse(
        unindent(
            r#"
        {
            let x = 7 + sin(y);
            {
                x = 3;
                if (bar < 3) {
                    x = x + 1;
                    y = 3 * x;
                } else if (bar < 2) {
                    let i = 2!;
                    x = x + i;
                } else {
                    x = 1;
                }
            }
        }
    "#,
        )
        .as_str(),
    );

    let stmts = match stmt {
        ast::Stmt::Block { stmts } => stmts,
        _ => unreachable!(),
    };
    assert_eq!(stmts.len(), 2);

    let let_stmt = &stmts[0];
    match let_stmt {
        ast::Stmt::Let { var_name, .. } => assert_eq!(var_name, "x"),
        _ => unreachable!(),
    }

    let stmts = match &stmts[1] {
        ast::Stmt::Block { stmts } => stmts,
        _ => unreachable!(),
    };
    assert_eq!(stmts.len(), 2);

    let assignment_stmt = &stmts[0];
    match assignment_stmt {
        ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
        _ => unreachable!(),
    }

    let if_stmt = &stmts[1];
    match if_stmt {
        ast::Stmt::IfStmt {
            condition,
            body,
            else_stmt,
        } => {
            assert!(matches!(
                &**condition,
                ast::Expr::InfixOp {
                    op:  T![<],
                    lhs: _lhs,
                    rhs: _rhs,
                }
            ));
            assert_eq!(body.len(), 2);
            let x_assignment = &body[0];
            match x_assignment {
                ast::Stmt::Assignment { var_name, .. } => 
                    assert_eq!(var_name, "x"),
                _ => unreachable!(),
            }
            let y_assignment = &body[1];
            match y_assignment {
                ast::Stmt::Assignment { var_name, .. } => 
                    assert_eq!(var_name, "y"),
                _ => unreachable!(),
            }

            let else_stmt = match else_stmt {
                Some(stmt) => &**stmt,
                None => unreachable!(),
            };

            match else_stmt {
                ast::Stmt::IfStmt {
                    condition,
                    body,
                    else_stmt,
                } => {
                    assert!(matches!(
                        &**condition,
                        ast::Expr::InfixOp {
                            op:  T![<],
                            lhs: _lhs,
                            rhs: _rhs,
                        }
                    ));
                    assert_eq!(body.len(), 2);
                    let let_i = &body[0];
                    match let_i {
                        ast::Stmt::Let { var_name, .. } => 
                            assert_eq!(var_name, "i"),
                        _ => unreachable!(),
                    }
                    let x_assignment = &body[1];
                    match x_assignment {
                        ast::Stmt::Assignment { var_name, .. } => 
                            assert_eq!(var_name, "x"),
                        _ => unreachable!(),
                    }

                    let else_stmt = match else_stmt {
                        Some(stmt) => &**stmt,
                        None => unreachable!(),
                    };

                    let stmts = match else_stmt {
                        ast::Stmt::Block { stmts } => stmts,
                        _ => unreachable!(),
                    };
                    assert_eq!(stmts.len(), 1);

                    let x_assignment = &stmts[0];
                    match x_assignment {
                        ast::Stmt::Assignment { var_name, .. } => 
                            assert_eq!(var_name, "x"),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        }
        _ => unreachable!(),
    }
}
```

#### Items
Let's move up another level to items containing statements.
We'll do struct and function definitions, for which we'll also need a notion of what a type looks like in our language.
As before, our reward for doing statements is that we're now allowed to use `self.statement()` to parse the function body.
Time to [play that same song again!](https://www.youtube.com/watch?v=_JmBsOywqzE)
```rust
// In parser/ast.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Struct {
        name:    Type,
        members: Vec<(String, Type)>,
    },
    Function {
        name:       String,
        parameters: Vec<(String, Type)>,
        body:       Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name:     String,
    pub generics: Vec<Type>,
}
```
As you can see, a `Type` is really just an identifier plus a list of generic parameters.
We use that for both function parameters and struct members, and also for the struct definition itself, to allow defining structs like `Foo<T, U>`.
Parsing types uses a recursive loop over the generic parameters (which are themselves types, like in `Bar<Baz<T>>`):
```rust
// In parser/hierarchy.rs

pub fn type_(&mut self) -> ast::Type {
    let ident = self.next()
        .expect("Tried to parse type, but there were no more tokens");
    assert_eq!(
        ident.kind,
        T![ident],
        "Expected identifier at start of type, but found `{}`",
        ident.kind
    );
    let name = self.text(ident).to_string();

    let mut generics = Vec::new();

    if self.at(T![<]) {
        self.consume(T![<]);
        while !self.at(T![>]) {
            // Generic parameters are also types
            let generic = self.type_();
            generics.push(generic);
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![>]);
    }

    ast::Type { name, generics }
}
```
We'll implement struct definitions first.
They are very similar - instead of generic parameters we loop over members, which are identifiers followed by `:` and their type:
```rust
pub fn item(&mut self) -> ast::Item {
    match self.peek() {
        T![fn] => todo!(),
        T![struct] => {
            self.consume(T![struct]);
            let mut members = Vec::new();
            let name = self.type_();
            self.consume(T!['{']);
            while !self.at(T!['}']) {
                let member_ident = self
                    .next()
                    .expect("Tried to parse struct member, 
                        but there were no more tokens");
                assert_eq!(
                    member_ident.kind,
                    T![ident],
                    "Expected identifier as struct member, but found `{}`",
                    member_ident.kind
                );
                let member_name = self.text(member_ident).to_string();
                self.consume(T![:]);
                let member_type = self.type_();
                members.push((member_name, member_type));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T!['}']);
            ast::Item::Struct { name, members }
        }
        kind => panic!("Unknown start of item: `{}`", kind),
    }
}
```
We can test types and struct definitions already:
```rust
// In tests/it.rs

#[test]
fn parse_struct() {
    fn parse(input: &str) -> ast::Item {
        let mut parser = Parser::new(input);
        parser.item()
    }

    let item = parse(
        unindent(
            r#"
        struct Foo<T, U> {
            x: String,
            bar: Bar<Baz<T>, U>
        }
    "#,
        )
        .as_str(),
    );

    match item {
        ast::Item::Struct { name, members } => {
            assert_eq!(
                name,
                ast::Type {
                    name:     "Foo".to_string(),
                    generics: vec![
                        ast::Type {
                            name:     "T".to_string(),
                            generics: vec![],
                        },
                        ast::Type {
                            name:     "U".to_string(),
                            generics: vec![],
                        }
                    ],
                }
            );
            assert_eq!(members.len(), 2);
            let (bar, bar_type) = &members[1];
            assert_eq!(bar, "bar");
            assert_eq!(
                bar_type,
                &ast::Type {
                    name:     "Bar".to_string(),
                    generics: vec![
                        ast::Type {
                            name:     "Baz".to_string(),
                            generics: vec![ast::Type {
                                name:     "T".to_string(),
                                generics: vec![],
                            }],
                        },
                        ast::Type {
                            name:     "U".to_string(),
                            generics: vec![],
                        }
                    ],
                }
            );
        }
        _ => unreachable!(),
    };
}
```

The function case should start to look familiar to you by now; it's a loop over parameters!
Additionally, we use the same trick as for `if` to parse the function body:
```rust
// In parser/hierarchy.rs, `Parser::item`

T![fn] => {
    self.consume(T![fn]);
    let mut parameters = Vec::new();

    let ident = self
        .next()
        .expect("Tried to parse struct member, but there were no more tokens");
    assert_eq!(
        ident.kind,
        T![ident],
        "Expected identifier as struct member, but found `{}`",
        ident.kind
    );
    let name = self.text(ident).to_string();

    self.consume(T!['(']);
    while !self.at(T![')']) {
        let parameter_ident = self
            .next()
            .expect("Tried to parse struct member, 
                but there were no more tokens");
        assert_eq!(
            parameter_ident.kind,
            T![ident],
            "Expected identifier as struct member, but found `{}`",
            parameter_ident.kind
        );
        let parameter_name = self.text(parameter_ident).to_string();
        self.consume(T![:]);
        let parameter_type = self.type_();
        parameters.push((parameter_name, parameter_type));
        if self.at(T![,]) {
            self.consume(T![,]);
        }
    }
    self.consume(T![')']);

    assert!(self.at(T!['{']), "Expected a block after function header");
    let body = match self.statement() {
        ast::Stmt::Block { stmts } => stmts,
        _ => unreachable!(),
    };

    ast::Item::Function { name, parameters, body }
}
```
We'll add a test for functions as well:
```rust
// In tests/it.rs

#[test]
fn parse_function() {
    fn parse(input: &str) -> ast::Item {
        let mut parser = Parser::new(input);
        parser.item()
    }

    let item = parse(
        unindent(
            r#"
        fn wow_we_did_it(x: String, bar: Bar<Baz<T>, U>) {
            let x = 7 + sin(y);
            {
                x = 3;
                if (bar < 3) {
                    x = x + 1;
                    y = 3 * x;
                } else if (bar < 2) {
                    let i = 2!;
                    x = x + i;
                } else {
                    x = 1;
                }
            }
        }
    "#,
        )
        .as_str(),
    );

    match item {
        ast::Item::Function { name, parameters, body } => {
            assert_eq!(name, "wow_we_did_it");
            assert_eq!(parameters.len(), 2);
            let (bar, bar_type) = &parameters[1];
            assert_eq!(bar, "bar");
            assert_eq!(
                bar_type,
                &ast::Type {
                    name:     "Bar".to_string(),
                    generics: vec![
                        ast::Type {
                            name:     "Baz".to_string(),
                            generics: vec![ast::Type {
                                name:     "T".to_string(),
                                generics: vec![],
                            }],
                        },
                        ast::Type {
                            name:     "U".to_string(),
                            generics: vec![],
                        }
                    ],
                }
            );
            assert_eq!(body.len(), 2);
        }
        _ => unreachable!(),
    };
}
```

## A Note on Error Handling
As we have written it now, our parser will `panic` when it encounters something it doesn't understand.
This is something all of us do from time to time, and there's no need to be ashamed about it.
Learning something new is hard, and if you've made it through this entire post as a beginner, then you have pushed the boundaries of your comfort zone more than enough for a day, so give yourselves some well-earned rest and maybe have some fun playing around with what we have made.

To wrap this back around to parsing; a "real" parser of course can't just crash on error.
If the parser is for a compiler, for which invalid input programs are useless, it may stop parsing, but it shouldn't just die.
Considering the user experience of using our parser is also important to consider.
We are all spoiled by Rust's tooling, which we will not be able to "just" / quickly imitate.
But some kind of user-facing errors are a must for any parser, and you should always strive to give the best feedback you can when something fails.

For our parser, this post is already very long and I don't want to shove an even more overwhelming amount of information down you people's throats.
If this is received well, adapting our parser to handle errors could be a nice topic for a follow-up article.
Until then, try not to panic when you're hitting a wall on your language development journey.
If you do get stuck somewhere, remember that there is always someone around in the community that can and will be happy to help you.
And instead of banging your head against that wall, maybe go outside, look at some real trees, and ponder why they are upside down.


---
[^stmt-expr]: In Rust, this is somewhat confusing, because most expressions can also be statements. For example, you can `break` a value from a `loop`. <a href="#fn-stmt-expr" class="footnote-backref" role="doc-backlink">↩︎</a>

[^shift-ops]: We will not add binary left- and right-shift operators (`<<` and `>>`) in this post, but if we did they'd be another source of ambiguity here. <a href="#fn-shift-ops" class="footnote-backref" role="doc-backlink">↩︎</a>

[^max-munch]: Be careful for which character sequences you introduce combined lexer tokens. Equality operators are usually fine, but for some character combinations munching them maximally may clash with other viable implementations. See the "Drawbacks" section of the Wikipedia article for some examples. <a href="#fn-max-munch" class="footnote-backref" role="doc-backlink">↩︎</a>
 