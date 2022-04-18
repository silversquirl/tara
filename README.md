# Tara

#### WIP - nothing is implemented yet

Tara is a simple, general purpose programming language targeting WebAssembly.
It can run natively, on the web, or embedded in other programs.

## Syntax

Tara's syntax is based on [B-expressions](https://github.com/silversquirl/bexpr).

## Types

Tara is compile-time dynamically typed.
This means it can be used similarly to a dynamically typed language, but no runtime type errors will ever occur.
It also supports simple type annotations for more explicit type signatures.

## Memory Safety

Tara uses a combination of static memory management, reference counting, and mark+sweep GC.
