# Elaine: Programming with Higher-Order Effects

Elaine is a programming language with support for both algebraic and higher-order effects. This repository contains the prototype for the language including:

- Parser
- Type checker
- Some program transformations
- Interpreter
- Pretty printer
- Tests

The language was developed as part of the thesis available at <https://github.com/tertsdiepraam/thesis>.

## Prerequisites

To run Elaine, you need GHC installed with cabal. We recommend installing GHC via [ghcup](https://www.haskell.org/ghcup/).

## Running Elaine

The easiest way to run the elaine executable is directly via cabal:

```
cabal run elaine -- [subcommand] [args]
```

Once it's built, it can also be called directly:

```
elaine [subcommand] [args]
```

## Subcommands

The executable has the following main subcommands:

- `run`: Run an Elaine program.
- `parse`: Parse an Elaine program and print the AST.
- `check`: Parse and type check an Elaine program without running it.
- `explicit`: Make all elaborations explicit and print the resulting program.
- `pretty`: Parse and then pretty print a program.

Other, less useful commands are:

- `spans`: Print a structure with spans from the parser.
- `spans-json`: JSON version of `spans`.
- `run-unchecked`: Run an Elaine program without type checking. Note that implicit elaboration resolution won't work in this mode.
- `metadata`: Print metadata relating to variable resolution.
- `metadata-json`: JSON version of `metadata`.

## Syntax Highlighting

There is syntax highlighting for Elaine in any editor. However, [Codex](https://github.com/jdonszelmann/codex) by Jonathan Dönszelmann can output HTML pages of Elaine code with syntax highlighting.