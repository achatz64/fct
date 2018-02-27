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

  (if (fct?* object)
    
    ((c/-> object c/meta :fct/inter) l)
     
    (c/cond
      (c/vector? object)
      (c/into [] (c/map (c/fn [o] (simple-ev* o l))
                        object))
      (c/map? object) (c/into {} (c/map (c/fn [[k v]] [(simple-ev* k l) (simple-ev* v l)])
                                        object))
      :else object)))



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

(c/defmacro let [& args]
  `(clojure.core/let ~@args))

(c/defn ^{:doc "is the argument admissible?"} adm-arg?
  [^{:doc "string"} x]
  
  (c/let [y (c/apply c/str (c/take 5 x))
          z (c/apply c/str (c/take 7 x))]
    
    (c/not (c/or (c/= y "map__") (c/= y "seq__") (c/= y "vec__") (c/= z "first__")))))


(c/defmacro fn [args body]
  (c/let [arg# (c/gensym 'fctarg__)
          d# (c/destructure [args arg#])
          m1# (c/map (c/fn [[x y]] x)
                     (c/partition 2 d#))
          liftall# (c/loop [v (c/into '() (c/into #{} m1#))
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
                       
                       [r rd]))
          liftd# (c/first liftall#)
          liftdummy# (c/second liftall#)]
    
    `(c/let [deps# (fct.min/deps* (c/let [~@liftdummy#]
                                    ~body))
             inter# (c/fn [l#]  
                      (c/fn ~args (fct.min/ev*
                                   (clojure.core/let [~@liftd#] 
                                     ~body)
                                   l#)))]
       
       (fct.min/construct* inter# :deps deps#))))

(def ex1-fn (ev* ((fn [{:keys [a b]}] (+ a b))
                  (var* :h))
                 {:h {:a 3 :b 5}}))


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

(c/println "Best: 280 ms")
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
(def com (let [boolean (fn [] (rand-nth (list true false)))
               some (rand-fn boolean)]
           (fn [x y] 
             (if-else (= x y)
                      true
                      (some x y)))))


(def c-com (c/let [boolean (fn [] (rand-nth (list true false)))
                   some (ev* (rand-fn boolean) {})]
             (c/fn [x y] 
               (if (c/= x y)
                 true
                 (some x y)))))







