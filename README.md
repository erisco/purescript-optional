# purescript-optional

## Description

`Optional` is a newtype on the uncurried Church encoding of `Maybe`. This has
benefits for modeling foreign optional arguments. A JavaScript program
can easily construct an `Optional a` in this way:

```
function (z, f) { m }
```

Where `m` is any block which returns `z` (encodes `default`) or `f(x)`
(encodes `use x`). Of course, this must also be a pure function.

In comparison, properly constructing a `Maybe a` with a JavaScript program
requires a PureScript program to pass along the `Just` and `Nothing`
constructors.

Similarly, a JavaScript program can easily destruct an `Optional a` in this way:

```
o(z, f) // optional z f o
```

And in comparison, properly destructing a `Maybe a` with a JavaScript program
requires a PureScript program to pass along the `maybe` destructor.

Downsides of `Optional` include the pervasiveness of `Maybe` (so
interoperating with existing libraries may require conversions) and the lack of
true constructors (so pattern matching notations cannot be used).

The purpose of `Optional` is to offer a compromise for FFI libraries between the
parametricity-violating representations based on `null` and `undefined`, such as
`Nullable` and `Undefinable`, and the cumbersome use of `Maybe`.

## Foreign Conversions

### Possibly-null

A conversion from possibly-null is:

```
function (z, f) { return x === null ? z : f(x); }
```

A conversion to possibly-null is:

```
o(null, function (x) { return x; })
```

### Possibly-undefined

A conversion from possibly-undefined is:

```
function (z, f) { return x === undefined ? z : f(x); }
```

A conversion to possibly-undefined is:

```
o(undefined, function (x) { return x; })
```
