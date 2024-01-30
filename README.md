
# The bridge programming language project

Bridge is an ongoing programming language project.

Its rationale and ideas can be found in the [rationale](./RATIONALE.md)

## Using the project

The project is currently in early development.

## TODO

- Limit functions to 40 lines (ish)

- Implement basic initial parser
    - Use generated errors instead of just cheating with anyhow
    - Implement operator arithmetic
    - Implement comments
    - Implement lambdas
    - Implement function call with non-identifier function values

- Decide whether to make a category of functions that can panic

- Implement basic type system
- Decide compilation backend (Haskell?, C?, LLVM?)
- Implement codegen
