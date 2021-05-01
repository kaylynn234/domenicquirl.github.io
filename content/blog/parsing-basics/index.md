+++
title = "Where to Start Hand-Writing a Parser (in Rust)"
date = 2021-04-26
+++

# TODO: expressions

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
    Eof,
}
```
You can see we have a lot of single-character tokens in there (including left and right brackets of all types), and then the groupings of strings and comments, numbers, and identifiers and keywords, as well as kinds to group things like `&&` or `!=` together.
We also have the `Error` kind in case we see a character we don't understand, and the `Eof` kind, which stands for "end of file" and will be the last token produced by the lexer.
Next, we will something that will come in handy a lot during our implementation: we will define a macro for referencing token kinds:
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
        '/' => T![/],
        '^' => T![^],
        '.' => T![.],
        ',' => T![,],
        '<' => T![<],
        '>' => T![>],
        '[' => T!['['],
        ']' => T![']'],
        '{' => T!['{'],
        '}' => T!['}'],
        '(' => T!['('],
        ')' => T![')'],
        ':' => T![:],
        _ => return None,
    })
}
```
The method is essentially the revers of the `Display` implementation, but only for tokens that are one character long _and cannot be the start of anything else_.
So it includes `+` and all the brackets, but, for example, it does not include `=`, because of the possible `==`.

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
    let input = "+-(.<>):";
    let tokens = lexer.tokenize(input);
    assert_tokens!(tokens, [T![+], T![-], 
        T!['('], T![.], T![<], T![>], T![')'], T![:], T![EOF],]);
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
    let input = "+-(.<>):";
    let mut lexer = Lexer::new(input); // <- new
    let tokens = lexer.tokenize(); // <- removed `input`
    assert_tokens!(tokens, [T![+], T![-], 
        T!['('], T![.], T![<], T![>], T![')'], T![:], T![EOF],]);
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
        let input = "+-(.<>):";
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


### The Parser

---
[^stmt-expr]: In Rust, this is somewhat confusing, because most expressions can also be statements. For example, you can `break` a value from a `loop`. <a href="#fn-stmt-expr" class="footnote-backref" role="doc-backlink">↩︎</a>
 