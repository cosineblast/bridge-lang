
# Bridge - The Idea

Note: this is a very early draft of the overall design of the programming language.
For ease of explanation, it assumes the reader is familiar with Haskell.
It is not very suitable for didactic purposes.

## `mut` and `effect`

Bridge is meant to be a programming language, aimed to provide
a decent pure functional programming experience to everyone.
Its syntax is meant to be appealing to people not yet experienced with functional programming,
yet providing semantics familiar to those who enjoy functional programming.

To allow for that, the Bridge language must both support and incentivize pure functional
programming, but also support easy mutable data and side effect usage when deemed necessary.

To achieve that, the main idea behind Bridge is the classification of pure and impure functions.

1. Pure functions by default

By default, Bridge functions are pure; they have no side effects,
and always produce the 'same' result, given the same arguments.

Because of that, their execution semantics are similar to Haskell.

```
fn powerTwo(n{: BigInt) -> BigInt {
    fold({ acc, _ -> 2 * acc }, 1, [0..=n])
}
```

In addition to not being able to perform side-effect operations,
pure functions cannot use loop mechanisms. The reason for that,
is that almost every useful loop involves either some sort of side effect operation,
or mutability, and Bridge has the right place for both.

Although Bridge refers to this kind of functions as 'pure', they all possibly have
a known language side effect: aborting the execution of the program.

In the future, this might change, but as of now, this means that execution is strict and
optimizing function calls away is not allowed.
Although, it is planned for the initial releases of the runtime to support flags to allow for
function call erasure.

2. Side Effects: `effect` and `mut`

### `effect`

Functions can be marked as `effect`, to call other effect functions,
perform FFI calls, and other IO operations.

```
effect fn main() {
    println("hi there!");
}
```

Pure functions cannot call `effect` functions, and the language provides no known way of doing so.

All `effect` functions are implicitly considered `mut` functions as well (open to change).

### `mut`

Functions can also be marked as `mut`, so that they can perform variable and object mutation.
It also allows for the usage of loop constructs.

```
mut fn countOccurences(result: MutArray(Int), values: Array(Byte)) {
    for (i in [0..result.length()]) {
        // "a[i] = b" is syntax sugar for "set(a, i, b)"
        result[i] = 0;
    }

    for (value in values) {
        let index = value into Int;
        result[index] = result[index] + 1;
    }
}

```

However, unlike `effect` functions, `mut` functions can be executed in pure code,
using the `freeze` builtin.

```
fn mostFrequentByte(values: Array(Byte)) -> Byte {
    freeze {
        let counts = MutArray.ofZeroes(256);

        countOccurences(counts, values);

        counts.seq().zip([0..]).maxBy({(x,y) -> y}).unwrap().0
    }
}

```

To ensure that, freeze blocks cannot refer to `mut` variables created outside the freeze block.

Bridge is not an easy language to implement. This `mut` and `freeze` mechanism implies
the compiler must have some form of type tagging `mut` types, similar to `Send` or `Sync` in Rust.

Altough `mut` functions defined with `fn` on the module scope are allowed
to exist, any other kind of global `mut` value is banned.

## Type Classification

Bridge aims to have a Higher Kinded type system, and
intends to provide a type classification mechanism, akin to
rust traits, and haskell typeclasses. The name classification will be
used, to avoid learning conflicts with classes, from object oriented programming.

```
// the kind of T is inferered to be OrdinaryType
// (i.e Haskell's '*', or 'Type' kind)
forall { T }
classification Display for T {
    fn display(x: T) -> String;
}

// the field semantics have not yet been stablished.

type Fraction {
    pub numerator: Unsigned,
    pub denominator: Unsigned
}

classify Fraction as Display {
    fn display(x: Fraction) -> String {
        x.numerator().display() + "/" + x.denominator().display()
    }
}
```

Bridge also aims to have classic classifications, such as
Monoid, SemiGroup, Functor, Applicative and Monad.

```
// the kind of M is inferered as OrdinaryType -> OrdinaryType
// the kind of T is inferered as OrdinaryType
// for a type to be Monad, it must also be Applicative
forall { M, T, M classify as Applicative }
classification Monad for M<T> {
    forall { T, U }
    fn bind(value: M<T>, function: T -> M<U>) -> M<U>;
}

forall { M, T, M classify as Functor }
classification Applicative for M<T> {
    forall { T, U }
    fn employ(value: M<T>, function: M<T -> U>) -> M<U>;

    forall { T, U }
    fn emit(source: T) -> M<T>;
}

forall { M, T }
classification Functor for M<T> {
    forall { T, U }
    fn map(value: M<T>, function: T -> U) -> T<U>;
}

```

### Special Monad Support

TBD

```

fn weirdStringSum(map: Map<String, i32>) -> Option<UInt> {

    let x = map.get("foo")!;
    let y = map.get("bar")!;

    emit(x + y)
}

```
