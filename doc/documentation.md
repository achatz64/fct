## Documentation (`0.6.0`)

### List of all functions and macros

#### `*` functions:
* (general use) [`ev*`](#ev*),  [`eval*`](#eval*),  [`gen*`](#gen*), [`construct*`](#construct*),  [`var*`](#var*),  [`lift*`](#lift*), [`on-obj*`](#on-obj*)
* (testing) [`check*`](#check*), [`gcheck*`](#gcheck*), [`ftest*`](#ftest*)

#### Macros:
* (generic lifting) [`lift-macro`](#lift-macro), [`throw`](#throw), [`if`](#if), [`if-else`](#if-else), [`cond`](#cond), [`lazy-seq`](#lazy-seq), [`and`](#and), [`or`](#or), [`do`](#do), [`->`](#->), [`->>`](#-->),
* (different syntax) [`let`](#let), [`fn`](#fn), [`defn`](#defn), [`loop`](#loop)

#### Fct functions:
* (auxiliary) [`rec`](#rec)
* (rand) [`rand-fn`](#rand-fn), [`rand-coll`](#rand-coll)

### Syntax and examples

#### <a name="var*"> </a> `var*`
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
  (list (var* :a) (var* :b) (var* :c)))
```  

#### <a name="lift*"> </a> `lift*`
```clj
(clojure.core/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn] ...)
```
Example:
```clj
(def ^{:doc "1. example for lift* in ns fct.core"} ex1-lift*
  ((lift* clojure.core/+) (var* :h) (var* :a)))
```

#### <a name="lift-macro"> </a> `lift-macro`
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

#### <a name="throw"> </a> `throw`
```clj
(clojure.core/defmacro throw [& args]
  `(fct.core/lift-macro throw ~@args))
```

#### <a name="if"> </a> `if`
```clj
(clojure.core/defmacro if [& args]
  `(fct.core/lift-macro if ~@args))
```

#### <a name="if-else"> </a> `if-else`
```clj
(clojure.core/defmacro if-else [& args]
  `(fct.core/if ~@args))
```

#### <a name="cond"> </a> `cond`
```clj
(clojure.core/defmacro cond [& args]
  `(fct.core/lift-macro clojure.core/cond ~@args))
```

#### <a name="lazy-seq"> </a> `lazy-seq`
```clj
(clojure.core/defmacro lazy-seq [& args]
  `(fct.core/lift-macro clojure.core/lazy-seq ~@args))
```

#### <a name="and"> </a> `and`
```clj
(clojure.core/defmacro and [& args] `(fct.core/lift-macro clojure.core/and ~@args))
```

#### <a name="or"> </a> `or`
```clj
(clojure.core/defmacro or [& args] `(fct.core/lift-macro clojure.core/or ~@args))
```

#### <a name="do"> </a> `do`
```clj
(clojure.core/defmacro do [& args] `(fct.core/lift-macro do ~@args))
```

#### <a name="->"> </a> `->`
```clj
(clojure.core/defmacro -> [& args] `(clojure.core/-> ~@args))
```

#### <a name="->>"> </a> `->>`
```clj
(clojure.core/defmacro ->> [& args] `(clojure.core/->> ~@args))
```

#### <a name="ev*"> </a> `ev*`
```clj
(clojure.core/defn ^{:doc "evaluation of an fct object resulting in a clj object"} ev*
  [^{:doc "fct object"} object
   ^{:doc "atom consisting of a map providing the interpretations for the variables"} l] ...)
``` 
Examples:
```clj
 (def ^{:doc "1. example for ev* in ns fct.core"} ex1-ev*
   (ev* (vector (var* :x) 3 (+ (var* :y) (var* :z)))
        (clojure.core/atom  {:x ["?"] :y 5 :z -5})))

 (def ^{:doc "2. example for ev* in ns fct.core"} ex2-ev*
   (ev* (fn [a] (+ (var* :y) a))
        {:y 5}))

```

#### <a name="eval*"> </a> `eval*`
```clj
(clojure.core/defn ^{:doc "evaluation of an fct object resulting in a clj object"} ev*
  [^{:doc "fct object"} object
   ^{:doc "map providing the interpretations for the variables"} l]
  (ev* object (clojure.core/atom l)))
``` 

#### <a name="gen*"> </a> `gen*`
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

#### <a name="construct*"> </a> `construct*`
```clj
(clojure.core/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter] ...)
```
Examples:
```clj
(def ^{:doc "1. example for construct* in ns fct.core"} ex1-construct*
  (construct* (clojure.core/fn [l] (:a (clojure.core/deref l)))))
```

#### <a name="on-obj*"> </a> `on-obj*`
```clj
(clojure.core/defn ^{:doc "replaces in the interpretation for the fct object the global state (l) with the provided state"} on-obj*
  ([^{:doc "fct object"} object
    ^{:doc "state"} state]
   
   (construct* (c/fn [l]                                  
                 (ev* object (ev* state l))))))
```

#### <a name="let"> </a> `let`
```clj
(clojure.core/defmacro ^{:doc "usual syntax"}
  let
  ([^{:doc "bindings, deconstruction works"} bindings]
   `(fct.core/let ~bindings nil))

  ([^{:doc "bindings, deconstruction works"} bindings
    ^{:doc "the body"} body]) ...)
```
Examples:
```clj
(def ^{:doc "1. example for let in ns fct.core"} ex1-let
  (let [{:keys [some]} (var* :a)] some))
```

#### <a name="fn"> </a> `fn`
```clj
(clojure.core/defmacro fn [& sigs]
  ...)
```
Examples:
```clj
(def ^{:doc "1. example for fn in ns fct.core"} ex1-fn
  (fn [x] x))

(def ^{:doc "2. example for fn in ns fct.core"} ex2-fn
  (fn [x] {:gen (fn [] [(rand-int 100)])}
    x))
```

#### <a name="defn"> </a> `defn`
```clj
(clojure.core/defmacro defn [name & sigs]
  `(def ~name (fct.core/fn ~name ~@sigs)))
```

#### <a name="check*"> </a> `check*`
```clj
(clojure.core/defn ^{:doc "applies gen* when called on a fct function, otherwise generates arguments and calls function on them"} check*
  [^{:doc "function"} f] ...)
```

#### <a name="gcheck*"> </a> `gcheck*`
```clj
(clojure.core/defn gcheck* [f]
  (check* (gen* f)))
```
Examples
```clj
(def ^{:doc "1. example for gcheck* in ns fct.core"} ex1-gcheck*
  (gcheck* (fn [x] {:gen (fn [] [(rand-int 100)])}
             x)))
```

#### <a name="ftest*"> </a> `ftest*`
```clj
(clojure.core/defn ^{:doc "runs tests by using check*"} ftest*

  [^{:doc "function"} f
   & {:keys [count-tests] :or {count-tests 1}}] ...)
```
Examples:
```clj
(def ^{:doc "1. example for ftest* in ns fct.core"} ex1-ftest*
  (ftest* (fn [x] {:gen (fn [] [(rand-int 100)])}
            (x 1))))
```

#### <a name="rec"> </a> `rec`
```clj
(def rec (lift* clojure.core/vector))
```

#### <a name="loop"> </a> `loop`
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

#### <a name="rand-fn"> </a> `rand-fn`
```clj
(defn ^{:doc "random function"} rand-fn
  [^{:doc "function without argument generating values for the random function"}  ret-spec] ...)
```
Examples:
```clj
(def ^{:doc "1. example for rand-fn in ns fct.core"} ex1-rand-fn
  (gen* (rand-fn (fn [] (rand-nth '(true false))))))
```

#### <a name="rand-coll"> </a> `rand-coll`
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
