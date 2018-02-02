## Documentation

### List of all functions and macros

#### `*` functions:
* (general use) `ev*`, `deps-tree*`, `deps-list*`, `deps*`, `gen*`, `construct*`, `sub*`, `var*`, `incognito-var*`, `lift*`
* (testing) `check*`, `gcheck*`, `ftest*`

#### Macros:
* (generic lifting) `lift-macro`, `throw`, `if`, `if-else`, `cond`, `lazy-seq`, `and`, `or`, `do`, `->`, `->>`,
* (different syntax) `let`, `fn`, `defn`, `loop`

#### Fct functions:
* (auxiliary)`rec`
* (rand) `rand-fn`, `rand-coll`

### Syntax and examples

`ev*`
```clj
(clojure.core/defn ^{:doc "evaluation of an fct object resulting in a clj object"} ev*
  [^{:doc "fct object"} object
   ^{:doc "map providing the interpretations for the variables"} l
   &
   {:keys [^{:doc "key, meta data of the constructed clj object corresponding to this key will be evaluated too"} key]
    :or {key :fct/spec}}] ...)
```
Examples:
```clj
 (def ^{:doc "1. example for ev* in ns fct.core"} ex1-ev*
   (ev* (vector (var* :x) 3 (+ (var* :y) (var* :z)))
        {:x ["?"] :y 5 :z -5}))

 (def ^{:doc "2. example for ev* in ns fct.core"} ex2-ev*
   (ev* (fn [a] (+ (var* :y) a))
        {:y 5}))

```

`deps-tree*`

```clj
(clojure.core/defn ^{:doc "deps tree of an fct object"} deps-tree*
  [^{:doc "fct object"} object
   ^{:doc "map providing substitutions for some variables, those variables will be considered dead ends of the dependence tree"} var-map] ...)
```
Examples:
```clj
(def ^{:doc "1. example for deps-tree* in ns fct.core"} ex1-deps-tree*  
  (deps-tree* (var* :a (fn [x] ((var* :b (var* :c)) x))) {}))

(def ^{:doc "2. example for deps-tree* in ns fct.core"} ex2-deps-tree*  
  (deps-tree* (var* :a (fn [x] ((var* :b (var* :c)) x))) {:b (var* :d)}))

```
`deps-list*`
```clj
(clojure.core/defn ^{:doc "describes all variables on which the object depends"} deps-list*
  [^{:doc "fct object"} object] ...)
```
Examples:
```clj
(def ^{:doc "1. example for deps-list* in ns fct.core"} ex1-deps-list*
  (deps-list* (var* :a (fn [x] ((var* :b (var* :c)) x)))))
```
`deps*`
```clj
(clojure.core/defn ^{:doc "lists all variables on which the object depends"} deps*
  [^{:doc "fct-object"} object] ...)
```
`gen*`
```clj
(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object"} a] ...)
```
Examples:
```clj
(def ^{:doc "1. example for gen* in ns fct.core"} ex1-gen*
  (gen* (var* :boolean (rand-nth (list true false)))))

(def ^{:doc "2. example for gen* in ns fct.core"} ex2-gen*
  (gen* (var* :a (fn [x] (map (var* :b (rand-fn (fn [] (rand-nth (list true false)))))
                               (range (rand-int x)))))))
```
`construct*`
```clj
(clojure.core/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter
   & {:keys [^{:doc "examples for the variables"} spec]
      :or {spec {}}}] ...)
```
Examples:
```clj
(def ^{:doc "1. example for construct* in ns fct.core"} ex1-construct*
  (construct* (clojure.core/fn [l] (:a l))))

(def ^{:doc "2. example for construct* in ns fct.core"} ex2-construct*
  (construct* (clojure.core/fn [l] (:boolean l)) :spec {:boolean (rand-nth (list true false))}))
```
`sub*`
```clj
(clojure.core/defn ^{:doc "substitution of variables"} sub*
  [^{:doc "fct object"} object
   ^{:doc "map providing the substitutions for the variables"} l
   &
   {:keys [^{:doc "key, as in ev*"} key]
    :or {key :fct/spec}}] ...)
```
Examples:
```clj
(def ^{:doc "1. example for sub* in ns fct.core"} ex1-sub*
  (sub* (+ 1 (var* :a)) {:a (var* :b)}))
```
`var*`
```clj
(clojure.core/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object"} spec] ...))
```
Examples:
```clj
(def ^{:doc "1. example for var* in ns fct.core"} ex1-var*
  (if-else (var* :bool)
           (var* :a)
           (var* :b)))
```  
`incognito-var*`
```clj
(clojure.core/defn ^{:doc "incognito variable construction"} incognito-var*
  [^{:doc "keyword attached to the variable"} key] ...)
```
`lift*`
```clj
(clojure.core/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn] ...)
```
Example:
```clj
(def ^{:doc "1. example for lift* in ns fct.core"} ex1-lift*
  ((lift* clojure.core/+) (var* :h) (var* :a)))
```
`lift-macro`
```clj
(clojure.core/defmacro ^{:doc "generic lifting of macros"} lift-macro
  [^{:doc "clojure macro"} macro
   & ^{:doc "arguments for the macro"} arg] ...)
```
Examples:
```clj
(clojure.core/defmacro ^{:doc "1. example for lift-macro in ns fct.core"} ex1-lift-macro [& args]
  `(lift-macro clojure.core/cond ~@args))
```
`throw`
```clj
(clojure.core/defmacro throw [& args]
  `(fct.core/lift-macro throw ~@args))
```
`if`
```clj
(clojure.core/defmacro if [& args]
  `(fct.core/lift-macro if ~@args))
```
`if-else`
```clj
(clojure.core/defmacro if-else [& args]
  `(fct.core/if ~@args))
```
`cond`
```clj
(clojure.core/defmacro cond [& args]
  `(fct.core/lift-macro clojure.core/cond ~@args))
```
`lazy-seq`
```clj
(clojure.core/defmacro lazy-seq [& args]
  `(fct.core/lift-macro clojure.core/lazy-seq ~@args))
```
`and`
```clj
(clojure.core/defmacro and [& args] `(fct.core/lift-macro clojure.core/and ~@args))
```
`or`
```clj
(clojure.core/defmacro or [& args] `(fct.core/lift-macro clojure.core/or ~@args))
```
`do`
```clj
(clojure.core/defmacro do [& args] `(fct.core/lift-macro do ~@args))
```
`->`
```clj
(clojure.core/defmacro -> [& args] `(clojure.core/-> ~@args))
```
`->>`
```clj
(clojure.core/defmacro ->> [& args] `(clojure.core/->> ~@args))
```
`let`
```clj
(clojure.core/defmacro ^{:doc "almost usual syntax (body is required (only one))"}
  let
  [^{:doc "bindings, deconstruction works"} bindings
   ^{:doc "the body"} body] ...)
```
Examples:
```clj
(def ^{:doc "1. example for let in ns fct.core"} ex1-let
  (let [{:keys [some]} (var* :a)] some))
```
`fn`
```clj
(clojure.core/defmacro fn [& sigs]
  (clojure.core/let [[x y o b] sigs
                     ^{:doc "name, used for recursion (optional)"} name (if (clojure.core/symbol? x) x nil)
                     ^{:doc "args (required)"} args (if name y x)
                     [o b] (if name [o b] [y o])
                     ^{:doc "additional options, e.g. {:gen ...} (optional)"} opt (if (clojure.core/map? o) o nil)
                     ^{:doc "body (only one!) (required)"} body (if opt b o)] ...))
```
```clj
(def ^{:doc "1. example for fn in ns fct.core"} ex1-fn
  (fn [x] x))

(def ^{:doc "2. example for fn in ns fct.core"} ex2-fn
  (fn [x] {:gen (fn [] (vector (rand-int 100)))}
    x))
```
`defn`
```clj
(clojure.core/defmacro defn [name & sigs]
  `(def ~name (fct.core/fn ~name ~@sigs)))
```
`check*`
```clj
(clojure.core/defn ^{:doc "applies gen* when called on a fct function, otherwise generates arguments and calls function on them"} check*
  [^{:doc "function"} f] ...)
```
`gcheck*`
```clj
(clojure.core/defn gcheck* [f]
  (check* (gen* f)))
```
Examples
```clj
(def ^{:doc "1. example for gcheck* in ns fct.core"} ex1-gcheck*
  (gcheck* (fn [x] {:gen (fn [] (vector (rand-int 100)))}
             x)))
```
`ftest*`
```clj
(clojure.core/defn ^{:doc "runs tests by using check*"} ftest*

  [^{:doc "function"} f
   & {:keys [count-tests] :or {count-tests 1}}] ...)
```
Examples:
```clj
(def ^{:doc "1. example for ftest* in ns fct.core"} ex1-ftest*
  (ftest* (fn [x] {:gen (fn [] (vector (rand-int 100)))}
            (x 1))))
```
`rec`
```clj
(def rec (lift* clojure.core/vector))
```
`loop`
```clj
(clojure.core/defmacro ^{:doc "the loop macro"} loop
  [^{:doc "as in fct.core/let fixing the initial conditions"} bindings
   {:keys [^{:doc "break out of loop condition"} test
           ^{:doc "iteration step, ending with (rec ...) instead of clojure's (recur ...)"} rec
           ^{:doc "return"} ret]}] ...)
```
Examples:
```clj
(def ^{:doc "1. example for loop in ns fct.core"} ex1-loop
  (loop [x (var* :counter)]
    {:test (= x 0)
     :rec (fct.core/do (println x)
                       (rec (dec x)))
     :ret "Done!"}))
```
`rand-fn`
```clj
(defn ^{:doc "random function"} rand-fn
  [^{:doc "function without argument generating values for the random function"}  ret-spec] ...)
```
Examples:
```clj
(def ^{:doc "1. example for rand-fn in ns fct.core"} ex1-rand-fn
  (gen* (rand-fn (fn [] (rand-nth '(true false))))))
```
`rand-coll`
```clj
(defn ^{:doc "random collection"} rand-coll
  [^{:doc "list of elements the collection can consist of"} l
   ^{:doc "number of elements in the collection"} i]

  (if-else (empty? l)
           '()
           (map (fn [] (rand-nth l)) (range i))))
```
Examples:
```clj
(def ^{:doc "1. example for rand-coll in ns fct.core"} ex1-rand-coll
  (gen* (rand-coll '(1 2 3 4) 5)))
```
