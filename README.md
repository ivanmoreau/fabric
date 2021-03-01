# hierarchical

Abstract Syntax Tree (AST) based on JSON concepts, but more abstract for parsing and application.

## Justification

Having worked with Circe and uPickle for years there are many things I love about each, but unfortunately a
few things I was frustrated by. At a high level, I think Circe can be a bit overly complicated and compilation
quite slow in large projects. With uPickle, I found the mutable underlying references within the structure very
concerning and problematic when doing things like merges. Both of them suffer from slow releases periodically,
so I ultimately decided to try my hand at accomplishing the same and incorporate some of my own crazy ideas in
the process.

I won't say that hierarchical is a better library than either of those great projects, but it was inspired by
both of them and customized to suit my particular needs. If you find it useful as well, please use it and offer
some feedback.

## Performance

I wrote a performance benchmark with every expectation to be slower than the alternatives as I've done very
little tuning, and I'm just one person versus the many developers that have worked on the others for years.
However, I was shocked to see how well my little library performed compared to the alternatives:
https://jmh.morethan.io/?source=https://raw.githubusercontent.com/outr/hierarchical/master/bench/results/benchmarks-1.0.0.json

## Features

The focus of this project is minimalism and flexibility. To that end, the features are somewhat sparse:

- Support for JVM, Scala.js, and Scala Native
- Support for Scala 2.12, 2.13, and 3.0
- AST for representation of `Map`, `Array`, `Numeric`, `String`, `Boolean`, and `null` in a type-safe and immutable way
- Clean DSL to create tree structures
- Deep merging support
- Compile-time generation of conversions to/from case classes
- Easy and convenient extensibility support
- Parsing support for JSON on JVM and Scala.js

## Getting Started

### Setup

For SBT simply include:
`libraryDependencies += "com.outr" %%% "hierarchical" % "x.y.z"`

For JSON parsing support include:
`libraryDependencies += "com.outr" %%% "hierarchical-json" % "x.y.z"`

### Create

Creating hierarchical structures with the DSL is very easy:

```scala
import hierarchical._

val v1 = obj(
  "name" -> "John Doe",
  "age" -> 21,
  "numbers" -> List(1, 2, 3),
  "address" -> obj(
    "street" -> "123 Somewhere Rd.",
    "city" -> "San Jose"
  )
)
```

### Merging

Deep-merging is trivial:

```scala
import hierarchical._

val v2 = obj(
  "age" -> 23,
  "numbers" -> List(4, 5, 6),
  "address" -> obj(
    "state" -> "California"
  )
)

val v3 = v1.merge(v2)
```

It is worth mentioning that because values are immutable, `v1` and `v2` remain unchanged.

### Convert

Conversion to other types is very easy with the built-in compile-time conversions:

```scala
import hierarchical._
import hierarchical.rw._

val person = obj(
  "name" -> "John Doe",
  "age" -> 21
).as[Person]

case class Person(name: String, age: Int)

object Person {
  implicit val rw: ReadableWritable[Person] = ccRW[Person]
}
```

### Parse

Parsing from existing JSON requires the use of the `hierarchical-json` module:

```scala
import hierarchical._
import hierarchical.json._

val value = Json.parse("""{"name": "John Doe", "age": 21}""")
```

### Formatting

Taking an existing value and formatting it for output as JSON:

```scala
val formattedString = Json.format(value)
```