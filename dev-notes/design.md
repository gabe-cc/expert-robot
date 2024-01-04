# Recursion

Recursion concerns itself with recursive _types_ and recursive _values_.
Recursive types are types that refer to themselves. For instance: `tree = Leaf of int | Node (list tree)`.
Recursive values are values that refer to themselves. For instance, the fibonacci function.

There are recursive values with non-recursive types. eg: the fibonacci function.
There are non-recursive values with recursive types. eg: `Leaf 42` of the above-defined type `tree`.

## Recursive Types

The code implements recursive types through iso-recursion. This means that:
- Constructing / Building an expression `expr` of a recursive type `t` requires wrapping it with a `fold` (`fold expr`).
- Destructing / Using an expression `expr` of recursive type `t` requires unwrapping it through `unfold` (`unfold expr`).

This is most common with inductive types (recursive variants). In that case, you will do something like `match unfold (fold (Foo 42)) with`

## Recursive Values

The code implements recursive values through a `rec` constructor.

Consider those two examples:
```ocaml
(* Example 1 *)
let factorial = rec self -> fun x ->
if x = 0 then 1
else x * (self (x - 1))

(* Example 2 *)
let lazy_infinite_list = rec self -> 1 :: (fun () -> self)
```

Whenever `rec self -> body` is encountered during evaluation, `body` is evaluated with `self` binding to `rec self -> body` in the context.

Then, whenever `rec self -> body` is encountered when fetching a variable from the environment, it is evaluated one more time.

# Type-Level Evaluation

## Introduction

In most typed languages, you have _some_ evaluation happening at the type-level. Concretely, you have a notion of complex type expressions being reduced into simpler type values.

At the very least, in typed languages, you can define type aliases like `type T = { foo : int , bar : string }` and then use them. When you'll use them the alias will be substituted for the underlying type, which is a basic form of evaluation.

Even though you might discount this as a fully fledged form of evaluation, on the ground of being too simple, notice that it already suffers from its challenges! Without loops, functions or numbers, **with just variables**, you can already build terms that blow up exponentially. Consider the following:
```typescript
type T1 = [number , number]
type T2 = [T1 , T1]
type T3 = [T2 , T2]
// ...
type TN = [TN_minus_one , TN_minus_one]
```

If this got you thinking, potentially in the direction of "do languages put a limit on this kind of stuff?" or "haha, this one is easy, you just need to unfold these variables lazily!", great.

## Typical Type-level Evaluation

Type-level Evaluation is treated quite differently across languages. The reason why is that it is often an afterthought. A deeper reason comes from the intersection of two phenomena, which are pervasive across languages.

### Hopefully Small and Simple

Most languages act as if **type-level evaluation was small**.

"small" means many things. But fundamentally, it means that you should be able to largely ignore it. Both as a user of the language, or as a maker of the language.

For instance, type-level evaluation should be fast. If as a developer, you notice that _merely evaluating a type_ (let alone type checking!) took a non-trivial amount of time, something has gone very wrong.

Furthermore, it should have as little effects as possible. Beyond "error during type evaluation means type checking failed", it should be side-effect free.

Even more "should": the type AST, its meaning and the behavior of type-level evaluation should all be simple.

And finally: users should not try to do too many things with types. Language builders will assume that developers using the language should not try to do too many complex things with types, except if they truly know them.

### Hopelessly Big and Complex

That's unfortunately not how things are in real-life. Languages grow over time, **and they grow a lot**. They start with some simple type system, built around a core principle like Hindley-Milner, Object Encapsulation, or Dependent Types. And then they unevitably grow into massive messes.

Type systems are no exception to [Greenspun's tenth rule](https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule). In context:
> Every sufficiently advanced type system contains an ad-hoc, informally specified, bug-ridden, slow implementation of half of the language's own semantics

For type systems that are based on unification, you have [an even more specific variant (slide 26/39)](https://www.irif.fr/~gradanne/papers/gadts.pdf)
> Any sufficiently complicated type system contains an ad hoc slow implementation of half of prolog.
(I recommend reading all the slides.)

Consider:
- C-style untyped macros and C++ crazy templates
- Constraint Resolution to derive type classes instance in Haskell
- OCaml and GADTs
- Advanced type-level evaluation that happens mid type inference: [check out this paper!](https://people.mpi-sws.org/~beta/papers/unicoq.pdf)

### FML

This would be fun, if languages embraced that their type systems were complex and that type level evaluation was a proper kind of evaluation. We'd all find ways to manage this complexity and shit.

But languages are in **denial**. They still act as if type-level evaluation was trivial. This predictibly leads to many problems.

Even though users shouldn't do crazy things with types, languages do not define well behaved subsets / constraints, where if you follow, you're guaranteed to get good error messages. As a result, you're left on your own to discover which parts of the type system work well together, or not at all.

Languages have very bad support for type error messages. Type errors have no stack traces! In unification based system, this is even more of a problem.
As is expected from the lack of traces, there are no debuggers at the type-level. You do not get a step-by-step understanding of why a specific variable came to have a specific type.

Let alone traces on errors or debuggers, languages do not offer `printf` at the type-level! If you want to print some intermediary types, you'll need to create some fake variables to check the type of some part of your code, or trigger some fake type errors.
As expected from the lack of supported `printf`, your poorman's printf will suck. Sometimes, you'll see that that a variable has the very useful type `T` without knowing what `T` is. Or you might get the opposite error, where in an error message, your variable will have an absolutely massive type, without any helpful names. Sometimes, things are even worse, with primitive types like `boolean` and `number` being renamed to `cli_optional_flag` or `map_index` in all error messages.

## Proper Type-Level Evaluation

Let's do some proper type level evaluation.

### Separated Type-Level Evaluation Function

First and foremost, all of the evaluation should happen in the `teval` function.

This means it should not happen in the type comparison function. Indeed, the type comparison function should be as syntactic as possible.
There are currently two exceptions to this rule:
- Alpha-equivalence. Instead of moving the AST to a nameless encoding (HOAS/De-Bruijn indices), we instead implement an equality function modulo alpha-equivalence. This is done because we prefer a clearer AST.
- Subtyping. We have some minimal subtyping. This is both to keep some optionality, and also an obsolete experiment with literal types.

There should be no ad-hoc evaluation in the type checking functions. Given that we follow a straightforward bidirectional type checking algorithm, there is no such problem.

### Clear Evaluation Order

Then, *there should be a clear evaluation order* for types. When you evaluate a program, there is a clear evaluation order: you know in what order the expressions are processed. The same should be true at the type-level.

This doesn't work well with unification. Unification is fundamentally constraint solving, which is much less naturally directed than typical call-by-value evaluation.
In prolog, when you write `append([[1,2],[3,4],[5]],Y)`, you get `Y = [1,2,3,4,5]`, which is the expected order, although the syntax is strange (`append(input , output)`). But you can also write, `append([[1,2],Y,[5]], [1,2,3,4,5])`, in which case you get `Y = [3,4]`.
Powerful, but unnatural. So for now, we just don't do unification.

Removing unification removes a difficulty, but still does not make everything trivial, with regard to having a clear evaluation order.

Typical call-by-value does not evaluate the body of functions. For instance, if you have `let wrap_double = x => double(x)`, it will not be evaluated to `x => x + x`, until you pass it an argument.
At the type level, this is bad. Consider `type 'a double = 'a * 'a`, and `let double_apply = (type a) (f : a double -> a) (x : a) = f (x , x)`. Because it is a polymorphic function, `a double` is not applied. As such, you would get a type error on `f (x , x)` as `a double` would be different from the inferred `(a * a)`.
Fundamentally, you need to extend call-by-value so that you can reduce type expressions in the body of polymorphic functions and parametric types.

This is what we do: at the type-level, we reduce everything (including the body of functions) eagerly. We descriminate full evaluations from partial evaluations, as we need to keep track of this information at various places.

### Type-level Evaluation: more than _type_ evaluation, _static_ evaluation

We also offer some support for _value_ reductions under lambdas (guided by keyword), and should support more things in that vein. 

This might seem bad, but this comes from a simple double dependency:
- A lot of static evaluation depends on types, such as ad-hoc polymorphism
- A lot of types depend on static evaluation, which is the whole point of dependent types

This does not mean that all of static evaluation should be captured by type-level evaluation. There's a lot that should be done by regular metaprogramming. You don't really want your type-system to write to arbitrary files, or add new bits to the AST. For this, you want to rely on more traditional code generation techniques.

### FULL TYPE-LEVEL CALCULUS

Ok. So, you need an actuall full type level calculus. Don't listen to people. Type systems **are** that complicated. This is worth a top-level point though.

# Full Type-Level Calculus

You need a full type-level calculus. It has been said.

There are just too many operators that are just too practical, and as a language builder, you will not build them all.
Despite how impressive it is, OCaml has no support for ad-hoc polymorphism, or a `get_foo` type-level function that takes a record type like `{ foo : int ; bar : string }` and returns its `foo` type (`int` here).
TypeScript has them, but in ways that make me sad. And dependently typed systems do not have them, even though they get all of the complexity of dependent types.

Let's briefly go over the different possible approaches, and end with the selected one.

## Pseudo-Survey

### Ad-Hoc Land

TypeScript "solves" the problem by constantly adding more shit into its type calculus. Just check out [that page describing a whole bunch of'em](https://www.typescriptlang.org/docs/handbook/utility-types.html) or [its type's cheat sheet](https://www.typescriptlang.org/static/TypeScript%20Types-ae199d69aeecf7d4a2704a528d0fd3f9.png).

### Proper Type-Level Calculus

Rather than adding very ad-hoc operators like `ReturnType` and `ArgType` in TypeScript, you might want to add operators that are more "calculus-like". For instance, you could add a pattern matching on types, such that one could write something like:
```ocaml
let return_type (t : type) =
  match t with
  | Arrow (_arg , return) -> return
  | _ -> failwith "not a function type"

let arg_type t =
  match t with
  | Arrow (arg , _return) -> arg
  | _ -> failwith "not a function type"
```

This would already be much better than Ad-Hoc land, but still suffers from Greenspun #10. Consider this time the `get_foo` type-level function, which, given a record type, must extract its `foo` field. (`get_foo { foo : int ; bar : string } -> int`)

```ocaml
let get_foo (t : type) =
  match t with
  | Record lst -> list_assoc "foo" lst
  | _ -> failwith "not a record type
```

Here, we conjured a `list_assoc` function, which basically looks at the correct binding in list (where `lst` could be `[("foo" , int) , ("bar" , string)]` for instance). This implies **a quite big standard-library** for the type-level calculus.
More generally, a proper type-level calculus is non-trivial, takes time and will need to reimplement a whole lot that is already implemented in the regular value-level calculus.

Let's go through one last idea before explaining our decision.

### Dependently Typed Systems

If a language builder needs a proper type-level calculus, and doesn't want to reimplement a new calculus separate from the value-level, they could try to _merge_ those two calculi.

This is what the [Calculus of Construction](https://en.wikipedia.org/wiki/Calculus_of_constructions) does, and more generally [Dependently Typed Systems](https://en.wikipedia.org/wiki/Dependent_type).

While this greatly improves the expressive power of the type-level calculus, this comes at the cost of crippling the value-level calculus. Dependently Typed languages put a strong focus on _pure_ functions.
Purity makes writing regular programs a pain. It often involves non-trivial monadic encodings for otherwise trivial programs. [This example from Lean is quite representative.](https://lean-lang.org/functional_programming_in_lean/monad-transformers/reader-io.html)
Even worse, dependently typed language's notion of purity is often stronger than Haskell's: _non-termination_ is itself considered _impure_. In other words, all functions much be proven to halt. This makes defining functions as simple as `quick_sort` quite challenging.

There are ways to improve on existing dependently typed systems. Their constraints are not ours. Typical dependently typed systems care about the [Curry-Howard isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence). 
They interpret many types as statement, and finding a program with the given type as proving these statements. This is the main source of constraints over what programs are allowed:
- Effects make this whole thing unsound. So they ban effects.
- Non-terminating programs make this whole thing unsound. So they restrict programs to provenly terminating ones.
- Because of [paradoxes](https://en.wikipedia.org/wiki/System_U), `Type` (the type of types) can not have `Type` (itself). The common solution involves a universe hierarchy: an infinite "tower of `Type`", that starts with `int : Type_0`, then `Type_0 : Type_1`, `Type_1 : Type_2` and so on and so forth. This means that functions taking types as parameters (polymorphic functions or parametric types) must specify what level of the tower they target, _or the language must involve some even more complex band-aid in the shape of [universe polymorphism](https://coq.inria.fr/refman/addendum/universe-polymorphism.html) or [universe cumulativity/subtyping](https://agda.readthedocs.io/en/v2.6.2.2/language/cumulativity.html)_.
- [Someone on stack overflow says that pattern matching on types would make Coq incompatible with the Univalence Axiom.](https://stackoverflow.com/a/42141166) It is good that we do not care about this!

So, as we progress in building the language, we might move closer and closer to a unified calculus for the value-level and the type-level.
But this involves solving a bunch of unsolved problem.
Let's check out the more practical intermediate solution instead.


## Type-Level Quotations

### Introduction

The approach that we picked is inspired by quote-based metaprogramming.
Fundamentally, we introduce in the language:
- A data-type representing type expressions, let's call it `etype` (for embedded type).
- A construct that gets us the AST of a type, and represents it as an `etype`. `to_value : type -> etype`
- A construct letting us go from a value of type `etype` to a type. `to_type : etype -> type`

Implementing the famous `get_foo` from earlier would look like:
```ocaml
type 'a get_foo = to_type (
  match (to_value 'a) with
  | TRecord lst -> (
    List.assoc "foo" lst
  )
  | _ -> failwith "get_foo: expected type trecord"
)
```

This is ludicrously powerful. This lets you defined `ReturnType`, `ArgType`, `get_foo` and more as regular functions using the term-level calculus!

### Extra Considerations

Obviously, anything in `to_value` can only exist at typing-time and gets erased at runtime. So it should reuse the scaffolding meant to discriminate between typing-time and run-time values.

Less obviously is the question of how `to_value` should deal with non-fully applied parameters?
To make it clear, let's consider `get_foo`. When evaluating `get_foo`, `to_value` only get a TVar for `'a`, as `get_foo` has not been applied to an arg at start.

How should we deal with this? There are roughly three approaches:
1. Only apply `to_value` to fully reduced types. If applied to a not-fully reduced types, it should do nothing and be considered a `partial` evaluation instead.
  This is valid, although a bit restrictive.
2. Apply `to_value` to the non-fully reduced type. The current code would then fail, but `teval` should discard the failure as one of the vars that was queried was not bound, and should thus return a partial instead. 
  This is much too complicated.
3. Put burden on the definer of `get_foo`.
  - `to_value 'a` should return `(Partial | Full) of etype` instead of just an `etype`. That way, `get_foo` knows if it's dealing with a fully reduced type or not.
  - `get_foo` itself should return a `(Partial | Full) of etype`. In case of partiality, it can just signal it, instead of raising an exception.

Option 3 is the best one, and is not incompatible with Option 1. Indeed, Option 1 is just a special case of Option 3, where `get_foo` does something like: `Full x -> (* regular processing *) | Partial x -> x`.
But Option 3 is more powerful in that it can evaluate partial types. For instance, `get_foo { foo : int ; bar : A }` where `A` is not given yet would return `get_foo { foo : int ; bar : A }` with Option 1, but be evaluated to `int` in Option 3.

### Conclusion

Based.

# Namespace-Level Evaluation

Same as Type-Level Evaluation, but replace "Type" with "Namespace".

More seriously. You have a range of how much languages care about namespaces. From "not at all" with C prototypes, to some minimal namespace/module calculus like Typescript's nested namespaces, to actual first-class citizens with types and functions, like OCaml and Coq.

All that was said for types do apply to modules. Funnily enough, OCaml [has some support to talk about modules from within regular code and vice-versa](https://v2.ocaml.org/manual/firstclassmodules.html), a simpler example of what was mentioned above for types. [Here is a tutorial about it](https://dev.realworldocaml.org/first-class-modules.html).

