# fct

### Free variables in clojure

We provide a framework for global and free variables in Clojure. Global means they have a global meaning (unlike variables in a function declaration) and free means that they are undefined. This turns any code into a function that can be evaluated by assigning interpretations to the variables. 

## Usage

### Syntax

Every clojure expression is automatically an fct expression. However, only pure (= not clojure) fct expressions can call, that is, can be used as functions, because clojure functions can't handle fct expressions like variables etc. The usual way to create pure fct functions from clojure functions is by using `lift*`. Any pure fct expression can call, the result will be a pure fct expression.  

Clojure functions that turn fct expressions to clojure expressions or the other way around can be identified by the `*` ending the name, e.g. `var*`, `ev*`,etc.

Macros are simply macros, most clojure macros won't produce the desired outcome when applied to pure fct objects. There is a `lift-macro` macro for generic use, but macros are all different (unlike functions), and lifting cannot be automated. For example, the threading macros automatically do what they are supposed to, and using `lift-macro` on them produces nonsense.

The `fn`, `let`, and `loop` macros are newly defined and are similar to the corresponding clojure macros. The syntax of `loop` is most different, but the functionality is the same. The macro `fn` accepts one body only, and works like a hypothetical `new-fn` in the `clojure.core` namespace:  
```
(defmacro new-fn [vec-arg body] (fn [& args] (let [vec-arg args] body)))
```          
In particular, it does not have an arity anymore.

Special forms like `if`, `do`, `throw`,... won't work. For some there are replacement macros. For example, `if-else` or `fct.core/if` for `if`. Note that the names of special forms don't belong to a namespace, so `if` is interpreted by the compiler as the one and only `if`.  

### To do
- [ ] add doc strings

### Hints

### Pitfalls
If `a` is an fct object then
```
[a] '(a) {:? a}
```
are not fct objects. They won't be interpreted correctly. Instead use:
```
(vector a) (list a) (hash-map :? a)
```

The following won't work
```
(fn [x] {:spec (fn [] (rand 1))} x),
```
because `(rand 1)` will evaluate to some number `0.????`, but we need `[0.????]` (as in `[x]`). Instead use:
```
(fn [x] {:spec (fn [] (vector (rand 1)))} x).
```


### `*` functions in fct
name in fct.core | use
-----|------
 `ev*` | constructs the corresponding clojure object with user provided  interpretation of the variables
 `var*` | variable creation
 `gen*` | constructs a corresponding clojure object by generating the variables
 `deps*` | shows on which variables the object depends upon
 `deps-tree*` | dependence tree
 `construct*` | constructs a fct object with user provided interpretation
 `lift*` | lifts a clojure function to an fct functions
 `sub*` | variable substitution
`check*` | tests a function (one step)
`gcheck*` | first `gen*`, then `check*`
`ftest*` | tests all the way (by using `check*`)


### Macros in fct
name in fct.core| use
----------------| --------
`fn`  | function generation
`defn` | combines `def` and `fn`
`let` | local bindings
`loop` | loop
`lift-macro` | generic lifting of macros  
`if-else` | copy of `if`

Macros in `fct.core` that work like the corresponding clojure macros or special forms:
```
throw, if, if-else, cond, lazy-seq, and, or, do, ->, ->>
```
There are many more missing, contributions are welcome.

### Rand
Random generation of data functions.

name in fct.core | use
-----------------|-------
rand-fn  | random function
rand-coll | random collection from a list


## License

Copyright © 2017 Andre Chatzistamatiou

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
