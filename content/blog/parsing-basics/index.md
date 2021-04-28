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
To mention a few, together with some guesses on why they might leave some of `#lang-dev`'s visitors' questions open:
 - [Crafting Interpreters](https://craftinginterpreters.com/) is an _absolutely fantastic_ read. Code examples are in Java and C however (also, the C compiler they develop is of very explicitly single-pass, which often makes it difficult for people to use it as guidance for their languages if the language design does not fit that model well. But that's less related to parsing). 
 - [Writing a Simple Parser in Rust](https://adriann.github.io/rust_parser.html) is very beginner-friendly, as it describes the author's own experience coming up with their parser. The parsing result is a homogenous syntax tree, which I personally like a lot, but, based on Rust Discord conversations, a strongly typed AST seems to be easier to conceptualize for many beginners because it gives you concrete _things_ from your language to talk about (like a `Function` or a `Variable`). More generally, the post is focused on parsing arithmetic expressions. 

   There is more to most languages than those, though, and dealing with precedence and associativity is often the source of a lot of confusion. The lexer implementation also vastly simplifies in this context, and is done by matching individual characters. That is by all means sufficient for the use case, but does not help beginners with lexing identifiers, escaped string literals, or even just floating point numbers (optionally with scientific notation), even less with handling conflicts between different classes of tokens (such as keywords which look like identifiers).
 - [Make a Language](https://arzg.github.io/lang/) is very detailed, but skips from lexer-less string-based parsing directly to using the [`logos`](https://crates.io/crates/logos) crate.

While I'm on the topic of other resources, [the `rust-langdev` repository](https://github.com/Kixiron/rust-langdev) is a collection of language development-related crates organized by category, also featuring a "Resources" section with further links on a bunch of topics. Go check it out!

## This Post is
 - An introduction to programming language parsing in which we hand-write a parser and run it on some real input.
 - A starting point, leaving lots of room for own experiments and further reading.
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

{{ include_image(path="blog/parsing-basics/tree.png") }}

### "Excuse me, there's a Lexer in your Parser"

The first point of confusion that commonly arises is that what is colloquially referred to as "parsing" quite often includes not one, but **two** components: a **lexer** and a **parser**.

## Implementing our Parser

### The Lexer
Crafting Interpreters has [an excellent section](https://craftinginterpreters.com/scanning.html#lexemes-and-tokens) on lexing.

### The Parser

---
[^stmt-expr]: In Rust, this is somewhat confusing, because most expressions can also be statements. For example, you can `break` a value from a `loop`. <a href="#fn-stmt-expr" class="footnote-backref" role="doc-backlink">↩︎</a>
 