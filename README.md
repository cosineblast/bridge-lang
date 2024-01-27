
# The bridge programming language project

Bridge is an ongoing programming language project.

Its rationale and ideas can be found in the [[rationale]](./RATIONALE.md)

## Building the project

The project is currently incomplete.

## TODO

- Add rustfmt on commit hook
- Add clippy on commit hook
- Limit functions to 40 lines (ish)

- Implement basic initial parser
    - Use generated errors instead of just cheating with anyhow
    - Implement operator arithmetic
    - Implement comments
    

- Implement basic type system
- Implement compiler
- Decide compilation backend (Haskell?, C?)
- Decide whether to make a category of functions that can panic