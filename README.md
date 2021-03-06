# fct

### Free variables in clojure

A framework for global and free variables in [Clojure](http://clojure.org). Global means variables have a global meaning (unlike variables in a function declaration) and free means that they are undefined. This turns any code into a function which can be evaluated by assigning interpretations to the variables.

For the motivations and goals behind fct: [Motivation](http://github.com/achatz64/motivation-fct)

### Usage

Create a new project with [Leiningen](http://leiningen.org) and add this dependency:

[![Clojars Project](https://clojars.org/fct/latest-version.svg)](https://clojars.org/fct)

Require `fct.core`:
```clj
(require '[fct.core :as f])
```

Forming expressions with variables:
```clj
(def a (f/and (f/var* :x) (f/var* :y)))
```

Evaluation:
```clj
(f/eval* a {:x true :y false})
```

The expressions can be combined:
```clj
(def b (f/or a (f/and (f/var* :x) (f/var* :z))))

(f/eval* b {:x true :y false :z true})
```

Any `clojure.core` function has a "lift" in `fct.core` so that it can be used on variables. In order to lift some other function:
```clj
(def lift-my-not (f/lift* (fn my-not [x] (if x false true))))

(f/eval* (lift-my-not b) {:x true :y false :z true})
```

Some macros or special forms in `clojure.core` have a lift in `fct.core`.
Local bindings:
```clj
(def d (f/let [{:keys [my]} (f/var* :x)] my))

(f/eval* d {:x {:my "project"}})
```
The special form `if` is replaced by `f/if` or `f/if-else`
```clj
(f/if-else (f/var* :x)
	   "ok?"
	   (f/throw (Exception. "GL!")))
```
Function construction:
```clj
(def f (f/fn [a] ((f/var* :x) a (f/var* :y))))

((f/eval* f {:x + :y -4}) 4)
```
For loops the syntax is a bit different from clojure syntax:
```clj
(def l (f/loop [x (f/var* :counter)]
         {:test (f/= x 0)
          :rec (f/do (f/println x)
                     (f/rec (f/dec x)))
          :ret "Done!"}))

(f/eval* l {:counter 10})
```
For testing it's better to supply ranges for the variables, `gen*` generates a witness:
```clj
(def g (f/+ 5 (f/var* :x (f/rand-int 10))))

(f/gen* g)
```
Any fct expression can be used:
```clj
(f/gen* (f/var* :x (lift-my-not (f/rand-nth '(true false)))))
```
Function construction allows for including generators (= an fct function without argument returning a vector) for the arguments
```clj
(def h (f/fn [a y] {:gen (f/fn [] [(f/rand-int 5) (f/rand-int 5)])}
         ((f/var* :x +) a y)))

(f/gcheck* h)
```

See [Algebra](https://github.com/achatz64/example-fct-algebra) for an example written in fct.

### Syntax

Every clojure expression is automatically an fct expression. However, only pure (= not clojure) fct expressions can call, that is, can be used as functions, because clojure functions can't handle fct expressions like variables etc. The usual way to create pure fct functions from clojure functions is by using `lift*`. Any pure fct expression can call, the result will be a pure fct expression.  

Clojure functions that turn fct expressions to clojure expressions or the other way around can be identified by the `*` ending the name, e.g. `var*`, `eval*`,etc.

Macros are simply macros, most clojure macros won't produce the desired outcome when applied to pure fct objects. There is a `lift-macro` macro for generic use, but macros are all different (unlike functions), and lifting cannot be automated. For example, the threading macros automatically do what they are supposed to, and using `lift-macro` on them produces nonsense.

The `fn`, `let`, and `loop` macros are newly defined and are similar to the corresponding clojure macros. The syntax of `loop` is most different. The macro `fn` accepts one body only.

Special forms like `if`, `do`, `throw`,... won't work. For some there are replacement macros. For example, `if-else` or `fct.core/if` for `if`. Note that the names of special forms don't belong to a namespace, so `if` is interpreted by the compiler as the one and only `if`.  

In the following we list functions and macros in `fct.core`, see the [Documentation](https://github.com/achatz64/fct/blob/master/doc/documentation.md) for more details.

### `*` functions in fct
name in fct.core | use
-----|------
 `eval*` | constructs the corresponding clojure object with user provided interpretation of the variables
 `ev*` | same as `eval*`, but the interpretation of the variables must be wrapped in an atom
 `var*` | variable creation
 `gen*` | constructs a corresponding clojure object by generating the variables
 `construct*` | constructs a fct object with user provided interpretation
 `lift*` | lifts a clojure function to an fct functions
 `on-object*` | changes how an fct-object is evaluated 
 `iso*` | turns an fct object to an fct function expecting an interpretation of the variables as argument
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
`if-else` | equivalent of `clojure.core/if`

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

### Using `fct.core` as reference
The following will look up names without namespace reference in `fct.core` instead of `core.clj`:
```clj
(ns my-project.core
  (:refer-clojure :only [])
  (:require
   [fct.core]
   [clojure.core :as c]))

(c/refer 'fct.core)
```
After this remember to call `clojure.core` functions with `c/`.

## License

Copyright © 2017, 2018 Andre Chatzistamatiou

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
