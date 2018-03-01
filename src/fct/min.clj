(ns fct.min
  (:refer-clojure :only [])
  (:require
   [clojure.core :as c]
   [clojure.set]))

(c/defn ^{:doc "is pure fct object?"} fct?*
  [^{:doc "fct object"} object]
  (:fct/? (c/meta object)))




(c/defn ^{:doc "get the interpretation"} ev*
  [^{:doc "fct object"} object
   ^{:doc "map providing the interpretations for the variables"} l]

  (c/let [m (c/meta object)]
    (if (:fct/? m)
      
      (c/let [clj-obj ((:fct/inter m) l)
              m (c/meta clj-obj)
              meta-obj (:fct/spec m)]

        (if meta-obj

          (c/with-meta clj-obj
            (c/assoc m :fct/spec (ev* meta-obj l)))

          clj-obj))
      
      (c/cond
        (c/vector? object)
        (c/into [] (c/map (c/fn [o] (ev* o l))
                          object))

        (c/map? object)
        (c/into {} (c/map (c/fn [[k v]] [(ev* k l) (ev* v l)])
                          object))

        :else
        object))))


;; (clojure.core/defn ^{:doc "evaluation of an fct object resulting in a clj object"} ev*
;;   [^{:doc "fct object"} object
;;    ^{:doc "map providing the interpretations for the variables"} l
;;    &
;;    {:keys [^{:doc "key, meta data of the constructed clj object corresponding to this key will be evaluated too"} key]
;;     :or {key :fct/spec}}]
  
    
;;   (c/let [clj-obj (simple-ev* object l)
;;           m (c/meta clj-obj)
;;           meta-obj (key m)]

;;     (if meta-obj

;;       (c/with-meta clj-obj (c/assoc m key (simple-ev* meta-obj l)))

;;       clj-obj)))


;; (def ^{:private true :doc "1. example for ev* in ns fct.min"} ex1-ev*
;;   (ev* (vector (var* :x) 3 (+ (var* :y) (var* :z)))
;;        {:x ["?"] :y 5 :z -5}))

;; (def ^{:private true :doc "2. example for ev* in ns fct.min"} ex2-ev*
;;   (ev* (fn [a] (+ (var* :y) a))
;;        {:y 5}))


(c/defn deps* [o]
  (if (fct?* o)
    (:fct/deps (c/meta o))
    #{}))

(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter
   &  {:keys [deps] :or {deps #{}}}]


  (c/with-meta
    (c/fn [& args] (construct*
                    
                    (c/fn [l]
                      (c/apply (inter l) (c/map #(ev* % l) args)))

                    :deps (c/apply clojure.set/union (c/map deps* args))))

    {:fct/? true
     :fct/inter inter
     :fct/deps deps}))

(c/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object"} object]
   (c/let [key (if (c/keyword? key) [key] key)]
     (construct* (c/fn [l] (c/get-in l key))
                 :deps #{key}))))


(c/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn]
  (construct* (c/fn [l] clojure-fn)))

;; (def ^{:private true :doc "1. example for lift* in ns fct.min"} ex1-lift*
;;   ((lift* c/+) (var* :h) (var* :a)))


(def ^{:doc "converts expressions with vector and hash-maps to fct"} to-fct
  (lift* c/identity))

(def ^{:doc "clojure interop"} call
  (lift* (c/fn [f & a] (c/apply f a))))

(def ex-call (ev* (call c/+ 1 2 (var* :a 5)) {:a 5}))

;;
;; automatic lifting of functions
;;

;; (def clj  (c/keys (c/ns-publics 'clojure.core)))

(def ^{:private true} meta-var (c/fn [n a]
                                 (c/meta (c/eval (c/read-string (c/str "(var " (c/str n) (c/str "/") (c/str a)  ")"))))))


;(:arglists (meta-var 'clojure.core (c/symbol "+")))

(def ^{:private true} find-functions (c/fn [n]
                                       (c/let [all-meta (c/map #(meta-var n %)
                                                               (c/keys (c/ns-publics n)))
                                               fun-or-mac (c/filter #(c/not (c/= nil (:arglists %)))
                                                                    all-meta)
                                               fun (c/filter #(c/not (:macro %)) fun-or-mac)]
                                         fun)))
 
;(c/count (find-functions 'clojure.core))
;(c/nth (find-functions 'clojure.core) 101)


;; (clojure.set/intersection (c/into #{} (c/map c/first (c/ns-publics 'fct.min)))
;;                           (c/into #{} (c/map #(:name %) (find-functions 'clojure.core))))

;; (clojure.set/intersection (c/into #{} (c/map #(:name %) (find-functions 'clojure.spec.alpha)))
;;                           (c/into #{} (c/map #(:name %) (find-functions 'clojure.core))))

(def ^{:private true} lift-all (c/fn [n]
                                 (c/let [names (c/map #(:name %) (find-functions n))
                                         intersection (clojure.set/intersection (c/into #{} (c/map c/first (c/ns-publics 'fct.min)))
                                                                                (c/into #{} names))
                                         difference (c/into [] (clojure.set/difference (c/into #{} names) intersection))]
                                   (c/doseq [k difference]
                                     (c/eval (c/read-string (c/str "(def " (c/str k) " (lift* " (c/str n) "/" (c/str k) "))")))))))

(lift-all 'clojure.core)

(c/defmacro ^{:doc "generic lifting of macros"} lift-macro
  [^{:doc "clojure macro"} macro
   & ^{:doc "arguments for the macro"} arg]
  
  (c/let [l (c/gensym 'fct_lift_macro_arg)
          body (c/cons macro (c/reverse (c/loop [a arg
                                                 na '()]
                                          (if a
                                            (c/let [f (c/first a)]
                                              (recur (c/next a) (c/cons (c/list 'fct.min/ev* f l)  na)))
                                            na))))]
    `(fct.min/construct* (c/fn [~l]
                            ~body))))
 
;; (c/defmacro ^{:private true :doc "1. example for lift-macro in ns fct.min"} ex1-lift-macro [& args]
;;   `(lift-macro c/cond ~@args))


(c/defmacro throw [& args]
  `(fct.min/lift-macro throw ~@args))

(c/defmacro if [& args]
  `(fct.min/lift-macro if ~@args))

;; just a copy
(c/defmacro if-else [& args]
  `(fct.min/if ~@args))


(c/defmacro cond [& args]
  `(fct.min/lift-macro c/cond ~@args))


(c/defmacro lazy-seq [& args]
  `(fct.min/lift-macro c/lazy-seq ~@args))

;(ev* (lazy-seq (list (var* :a))) {:a 5})

(c/defmacro and [& args] `(fct.min/lift-macro c/and ~@args))

(c/defmacro or [& args] `(fct.min/lift-macro c/or ~@args))

(c/defmacro do [& args] `(fct.min/lift-macro do ~@args))

;(c/defmacro . [& args] `(lift-macro . ~@args))

(c/defmacro -> [& args] `(c/-> ~@args))

(c/defmacro ->> [& args] `(c/->> ~@args))


(c/defn ^{:doc "is the argument admissible?"} adm-arg?
  [^{:doc "string"} x]
  
  (c/let [y (c/apply c/str (c/take 5 x))
          z (c/apply c/str (c/take 7 x))]
    
    (c/not (c/or (c/= y "map__") (c/= y "seq__") (c/= y "vec__") (c/= z "first__")))))

(c/defn ^{:doc "helper function with deconstruction"} ds
  [^{:doc "symbols that appear in function definition as arguments"} args]
  (c/let [arg# (c/gensym 'fctarg__)
          d (c/destructure [args arg#])
          m (c/map (c/fn [[x y]] x)
                   (c/partition 2 d))]
    (c/loop [v (c/into '() (c/into #{} m))
                            r '()
                            rd '()]
                     (if v
                       (c/let [[x] v]
                         (if (adm-arg? (c/str x))
                           
                           (recur (c/next v)
                                  (c/-> r
                                        (c/conj (c/list 'fct.min/lift* x))
                                        (c/conj x))
                                  (c/-> rd
                                        (c/conj (c/list 'fct.min/lift* 0))
                                        (c/conj x)))
                           
                           (recur (c/next v) r rd)))
                       
                       [r rd]))))

(c/defmacro fn [& sigs]
  (c/let [[x y o & b] sigs
          ^{:doc "name, used for recursion (optional)"} name (if (c/symbol? x) x nil)
          ^{:doc "args (required)"} args (if name y x)
          ^{:doc "additional options, e.g. {:gen ...} (optional)"} opt
          (if name
            (if (c/and (c/map? o) (c/not (c/empty? b)))
              o
              nil)
            (if (c/and (c/map? y) o)
              y
              nil))
          ^{:doc "body (required)"} body (c/cons 'fct.min/do (c/cond (c/and name opt) b
                                                                     name (c/cons o b)
                                                                     opt (c/cons o b)
                                                                     (c/nil? o) (c/list y)
                                                                     :else (c/cons y (c/cons o b))))
          ;; start
          opt (if opt opt (if (c/empty? args)
                            {:gen []}
                            {:gen nil}))
          [liftd# liftdummy#] (ds args)
          for-recursion (if name
                          (c/list name (c/list 'fct.min/lift* name))
                          '())
          for-recursion-dummy (if name
                                (c/list name (c/list 'fct.min/lift* 1))
                                '())
          name (if name name (c/gensym 'fct__fn__name))]
    

    `(c/let [args-spec# (:gen ~opt)
             deps# (fct.min/deps* (clojure.core/let [~@for-recursion-dummy]
                                    (c/let [~@liftdummy#]
                                      ~body)))
             inter# (c/fn [l#]  
                      (c/with-meta

                        (c/fn ~name ~args
                          (fct.min/ev*
                           (clojure.core/let [~@for-recursion]
                             (clojure.core/let [~@liftd#]
                               ~body))
                           l#))
                        
                        {:fct/? false :fct/fcn? true :fct/spec args-spec#}))]
       
       (fct.min/construct* inter# :deps deps#))))


(def ex1-fn (ev* ((fn [{:keys [a b]}] (+ a b))
                  (var* :h))
                 {:h {:a 3 :b 5}}))

(def ex2-fn (ev* (fn g [] 
                   (c/let [a (c/rand)]
                     (if-else (< a 0.1)
                              a
                              (g))))
                 {}))

(def ex3-fn (ev* (fn [x] {:gen (fn [] [(rand-int 10)])}
                   x)
                 {}))

(c/defmacro ^{:doc "almost usual syntax"}
  let
  
  ([^{:doc "bindings, deconstruction works"} bindings]
   `(fct.min/let ~bindings nil))
  
  ([^{:doc "bindings, deconstruction works"} bindings
    ^{:doc "the body"} body]
   
   (if (c/empty? bindings)
     body
     (c/let [[args# val#]  bindings
             [liftd# liftdummy#] (ds args#)
             rec# (c/rest (c/rest bindings))]
       
       `(construct* (c/fn [l#]
                      (clojure.core/let [~args# (fct.min/ev* ~val# l#)]
                        
                        (clojure.core/let [~@liftd#] 
                          (fct.min/ev* (fct.min/let [~@rec#] ~body)
                                       l#)))))))))




;;;
;;; loop
;;;

(def ^{:private true} loopn* (c/fn 
                               ([start test iter ret]
                                (c/loop [l start]
                                  (if (test l)
                                    (ret l)
                                    (recur (iter l)))))
                               ([start test iter]
                                (c/loop [l start]
                                  (if (test l)
                                    l
                                    (recur (iter l)))))))

;; (loopn* '(1 2 3)
;;        (c/fn [a] (c/empty? a))
;;        (c/fn [a] (c/rest a)))

(def loopf (lift* loopn*))

(def rec (lift* c/vector))

(c/defmacro ^{:doc "the loop macro"} loop
  [^{:doc "as in fct.core/let fixing the initial conditions"} bindings
   {:keys [^{:doc "break out of loop condition"} test
           ^{:doc "iteration step, ending with (rec ...) instead of clojure's (recur ...)"} rec
           ^{:doc "return"} ret]}]
  
  (c/let [all# (c/partition 2 bindings)
          args# (c/into [] (c/map c/first all#))
          data# (c/map c/second all#)]
    
    `(fct.min/loopf ((fct.min/lift* c/vector) ~@data#)
                     (fct.min/fn [~args#] ~test)
                     (fct.min/fn [~args#] ~rec)
                     (fct.min/fn [~args#] ~ret))))


(def ex-loop (ev* (loop [x (range 10)
                         y []]
                    {:test (empty? x)
                     :rec (let [[f] x]
                                 (rec (rest x) (conj y f)))
                     :ret y})
                  {}))

;;
;; rand
;;

(def ^{:private true} string-to-0-99 (c/fn [string]
                                       (c/let [h (c/rem (c/hash string) 100)]
                                         (if (c/>= h 0)
                                           h
                                           (c/+ h 100)))))
 

(def ^{:doc "random function"}
  rand-fn (lift* (c/fn [^{:doc "function without argument generating values for the random function"}  ret-spec]
                   
                   (c/let [ret-samples (c/map (c/fn [x] (ret-spec))
                                              (c/range 100))]
                     (c/fn [& args] (c/let [c (string-to-0-99 (c/pr-str args))]
                                      (c/nth ret-samples c)))))))

;; (def ^{:private true :doc "1. example for rand-fn in ns fct.core"} ex1-rand-fn
;;   (gen* (rand-fn (fn [] (rand-nth '(true false))))))


(def ^{:doc "random collection"} rand-coll
  (lift* (c/fn [^{:doc "list of elements the collection can consist of"} l
                ^{:doc "number of elements in the collection"} i]
           
           (if (c/empty? l)
             '()
             (c/map (c/fn [] (c/rand-nth l)) (c/range i))))))

;; (def ^{:private true :doc "1. example for rand-coll in ns fct.core"} ex1-rand-coll
;;   (gen* (rand-coll '(1 2 3 4) 5)))

;; test

(c/defmacro defn [name & sigs]
  `(def ~name (fct.core/fn ~name ~@sigs)))


;;
;; testing
;;

(c/defn ^{:doc "applies gen* when called on a fct function, otherwise generates arguments and calls function on them"} check*
  [^{:doc "function"} f]
  
  (c/let [m (c/meta f)
          spec-structure (:fct/spec m)]
    
    (c/cond

      (c/or (c/= nil (:fct/? m))
            (c/= spec-structure nil))  nil

      true  (c/cond
              
              (:fct/? m)
              {:args nil
               :ret (ev* f {})} ; to be changed to gen*                     
              
              (:fct/fcn? m)
              (if (c/not (c/or (c/= spec-structure {})
                               (c/= spec-structure [])))
                (c/let [a (spec-structure)]
                  {:args a
                   :ret (c/apply f a)})
                
                {:args []
                 :ret (f)})))))


(c/defn ^{:doc "runs tests by using check*"} ftest*

  [^{:doc "function"} f
   & {:keys [count-tests] :or {count-tests 1}}]

  (c/let [test-f (c/fn [x c] (c/let [s (c/map (c/fn [i] (check* x))
                                              (c/range c))
                                     [f] s]
                               (if f
                                 (c/map (c/fn [c] (:ret c))
                                        s)
                                 nil)))
          
          f (c/let [m (c/meta f)]
              (c/cond (:fct/fn? m) (:fct/fn m)
                      true f))
          
          first-result (test-f f count-tests)
          inner-test-loop (c/fn [l] (c/loop [l l
                                             ret '()]
                                      (if (c/empty? l)
                                        ret
                                        (c/let [x (c/first l)
                                                y (test-f x 1)]
                                          (if y
                                            (recur (c/rest l) (c/conj ret (c/first y)))
                                            (recur (c/rest l) ret))))))]
    (c/loop [l first-result]
      (if (c/empty? l)
        true
        (recur (inner-test-loop l))))))

;; performance test

(c/defn test-loop [n f]
  (c/loop [n n]
    (if (c/= n 0)
      "done"
      (do (f)
          (recur (c/dec n))))))

(def term (fn []
            (rand-nth (map (fn [x] (str "term" x))
                           (range 20)))))

(def t (ev* term {}))

(def c-term (c/fn [] 
              (c/rand-nth (c/map (c/fn [x] (c/str "term" x))
                                 (c/range 20)))))

(c-term)

(c/println "Clojure:")
(c/time (test-loop 10000 c-term))

(c/println "Best: 600 ms")
(c/time (test-loop 10000 t))

(def element (fn [] (map (fn [x] (term))
                         (range (rand-int 10)))))


(def e (ev* element {}))

(def c-element (c/fn [] (c/map (c/fn [x] (c-term))
                               (c/range (c/rand-int 10)))))

(c-element)

(c/println "Clojure:")
(c/time (test-loop 10000 c-element))

(c/println "Best: ~40 ms")
(c/time (test-loop 10000 e))

;; decide whether two terms have the same type, we want the same object to have the same type
(def com (c/let [boolean (c/fn [] (c/rand-nth (c/list true false)))
                 some (lift* (ev* (rand-fn boolean) {}))]
           (fn [x y] 
             (if-else (= x y)
                      true
                      (some x y)))))

;; (def c-com (c/let [boolean (fn [] (rand-nth (list true false)))
;;                    some (ev* (rand-fn boolean) {})]
;;              (c/fn [x y] 
;;                (if (c/= x y)
;;                  true
;;                  (some x y)))))

;; (c-com "term1" "term6")

(def c-com (ev* com {}))

(def add-term (fn [e] 
                (if-else (not (empty? e))
                         (let [first-term (first e)
                               type-check  (every? (fn [t] (com first-term t))
                                                   e)]
                           (if-else type-check
                                    first-term
                                    (list com e)))                                             
                         (fct.min/throw (Exception. "cannot deal with empty list")))))
 
(c/defn tadd [] (ev* (add-term (element))
                     {}))

(def c-add-term (c/fn [e] 
                  (if (c/not (c/empty? e))
                    (c/let [first-term (c/first e)
                            type-check  (c/every? (c/fn [t] (c-com first-term t))
                                                  e)]
                      (if type-check
                        first-term
                        (c/list c-com e)))                                             
                    (throw (Exception. "cannot deal with empty list")))))

(def find-same (fn [t e] 
                 {:same-type (filter (fn [a] (com t a)) e)
                  :remainder (filter (fn [a] (not (com t a))) e)}))

(c/defn fs [] (ev* (find-same (term) (element)) {}))

 ;; (def s (fn [e] {:gen (fn [] (vector (element)))}
 ;;         (add-term (get (find-same (first e) e) :same-type))))

(def c-find-same (c/fn find-same [t e] 
                   (c/hash-map :same-type (c/filter (c/fn [a] (c-com t a)) e)
                               :remainder (c/filter (c/fn [a] (c/not (c-com t a))) e))))


(c/defn c-fs [] (c-find-same (c-term) (c-element)))


(c/println "Clojure:")
(c/time (test-loop 10000 c-fs))

(c/println "Best: 800 ms")
(c/time (test-loop 10000 fs))


(def simplify (fn [e] 
                (loop [x e
                       r []]
                  {:test (empty? x)
                   :rec (let [[t] x
                              {:keys [remainder same-type]} (find-same t x)
                              sum  (add-term same-type)
                              new-y (conj r sum)]
                          (rec remainder new-y))
                   :ret r})))


(c/defn tsimp [] (ev* (simplify (element)) {}))


(def c-simplify (c/fn [e] 
                  (c/loop [x e
                           r []]
                    (if (c/empty? x)
                      r
                      (c/let [[t] x
                              {:keys [remainder same-type]} (c-find-same t x)
                              sum  (c-add-term same-type)
                              new-y (c/conj r sum)]
                        (recur remainder new-y))))))

(c-simplify (c-element))

(c/defn c-tsimp [] (c-simplify (c-element)))

;; we could also use (s/coll-of element) 
(def add (fn [& elements] 
           (simplify (apply concat elements))))


(def c-add (c/fn [& elements] 
             (c-simplify (c/apply c/concat elements))))

(c-add (c-element) (c-element))

;; ;; multiplication of terms
(def mult-term (lift* ((ev* rand-fn {}) (ev* element {}))))



(def c-mult-term (ev* mult-term {}))



(def mult (fn [& elements] 

            (c/let [simple-mult1 (fn [t e] 
                                 (loop [e e r []]
                                   {:test (empty? e)
                                    :rec (let [[s] e]
                                           (rec (rest e) (concat (mult-term t s) r)))
                                    :ret (simplify r)}))
                    simple-mult  (fn [e1 e2] 
                                   (loop [e1 e1 r []]
                                     {:test (empty? e1)
                                      :rec (let [[t] e1]
                                             (rec (rest e1)
                                                  (concat (simple-mult1 t e2)
                                                          r)))
                                      :ret (simplify r)}))]
              
              (if-else (empty? elements)
                       []
                       (loop [elements (rest elements)
                              r (first elements)]
                         {:test (empty? elements)
                          :rec (let [[e] elements]
                                 (rec (rest elements) (simple-mult r e)))
                          :ret (simplify r)})))))


(c/defn mt [] (ev* (mult (element) (element) (element))
                   {}))



(def c-mult (c/fn [& elements] 

              (c/let [simple-mult1 (c/fn [t e] 
                                     (c/loop [e e r []]
                                       (if  (c/empty? e)
                                         (c-simplify r)
                                         (c/let [[s] e]
                                           (recur (c/rest e) (c/concat (c-mult-term t s) r))))))
                  simple-mult  (c/fn [e1 e2] 
                                 (c/loop [e1 e1 r []]
                                   (if (c/empty? e1)
                                     (c-simplify r)
                                     (c/let [[t] e1]
                                       (recur (c/rest e1)
                                              (c/concat (simple-mult1 t e2)
                                                        r))))))]
              
                (if (c/empty? elements)
                  []
                  (c/loop [elements (c/rest elements)
                           r (c/first elements)]
                    (if (c/empty? elements)
                      (c-simplify r)
                      (c/let [[e] elements]
                        (recur (c/rest elements) (simple-mult r e)))))))))


(c/defn c-mt [] (c-mult (c-element) (c-element) (c-element)))

(c/println  "Clojure:")
(c/time (test-loop 100 c-mt))

(c/println  "Best: 1400 ms")
(c/time (test-loop 100 mt))




