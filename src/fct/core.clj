(ns fct.core
  (:refer-clojure :only [])
  (:require
   [clojure.core :as c]
   [clojure.set]))

;;
;; evaluate to a usual clojure object
;;

(clojure.core/defn ^{:doc "evaluation of an fct object resulting in a clj object"} ev*
  [^{:doc "fct object"} object
   ^{:doc "map providing the interpretations for the variables"} l
   &
   {:keys [^{:doc "key, meta data of the constructed clj object corresponding to this key will be evaluated too"} key]
    :or {key :fct/spec}}]
  
  (c/let [simple-ev* (c/fn [object] (if (c/-> object c/meta :fct/?)
                                      ((c/-> object c/meta :fct/inter) l)
                                      object))]
    (if (c/keyword? key)
      
      (c/let [clj-obj (simple-ev* object)
              m (c/meta clj-obj)
              meta-obj (key m)]
        (if meta-obj
          (c/with-meta clj-obj (c/assoc m key (simple-ev* meta-obj)))
          clj-obj))

      (simple-ev* object))))


;; (def ^{:private true :doc "1. example for ev* in ns fct.core"} ex1-ev*
;;   (ev* (vector (var* :x) 3 (+ (var* :y) (var* :z)))
;;        {:x ["?"] :y 5 :z -5}))

;; (def ^{:private true :doc "2. example for ev* in ns fct.core"} ex2-ev*
;;   (ev* (fn [a] (+ (var* :y) a))
;;        {:y 5}))

(c/defn ^{:doc "as merge, but works with nested maps"} nested-merge
  [& ^{:doc "hash-maps"} args]
  
  (c/let [test-some? (c/some c/identity (c/map c/map? args))]
    (if test-some?
      (c/let [args (c/filter c/map? args)
              keys (c/into #{} (c/apply c/concat (c/map c/keys args)))]
        (c/into {} (c/map (c/fn [k] [k (c/apply nested-merge (c/map (c/fn [x] (k x))
                                                                   args))])
                          keys)))
      (c/let [args (c/filter #(c/-> % c/nil? c/not)
                             args)]
        (if (c/empty? args)
          nil
          (c/first args))))))

(c/defn ^{:doc "find all keys for a nested map"} nested-keys
  [^{:doc "nested map"} m]

  (if (c/map? m)
    (c/let [keys (c/keys m)]
      (c/apply c/concat (c/map (c/fn [k] (c/let [r (c/map (c/fn [v] (c/into [] (c/cons k v)))
                                                          (nested-keys (k m)))]
                                           (if (c/empty? r)
                                             (c/list [k])
                                             r)))
                               keys)))
    '()))

(c/defn ^{:doc "convert key-value lists to nested hash-maps"} nested-into
  [^{:doc "key-value lists"} kv]
  (c/loop [r {}
           kv kv]
    (if (c/not (c/empty? kv))
      (c/let [[[k v]] kv]
        (recur (c/assoc-in r k v)
               (c/rest kv)))
      r)))

(c/defn ^{:doc "convert nested hash-map to key-value list"} nested-key-value
  [^{:doc "nested map"} m]
  (c/map (c/fn [k] [k (c/get-in m k)]) (nested-keys m)))

(c/defn dissoc-in 
  [^{:doc "nested map"} m
   ^{:doc "key"} k]
  (if (c/= 1 (c/count k))
    (c/let [[k] k]
      (c/dissoc m k))
    (c/update-in m (c/pop k) (c/fn [x] (c/dissoc x (c/peek k))))))


(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter
   & {:keys [^{:doc "generators for the variables"} gen]
      :or {gen {}}}]
  
  (c/with-meta
    (c/fn [& args] (construct*
                    (c/fn [l]
                      (c/apply (inter l) (c/map #(ev* % l) args)))
                    :gen (c/apply nested-merge (c/conj (c/map #(c/-> % c/meta :fct/gen)
                                                             args)
                                                      gen))))
    {:fct/? true
     :fct/inter inter
     :fct/gen gen}))

;; (def ^{:private true :doc "1. example for construct* in ns fct.core"} ex1-construct*
;;   (construct* (c/fn [l] (:a l)))) 

;; (def ^{:private true :doc "2. example for construct* in ns fct.core"} ex2-construct*
;;   (construct* (c/fn [l] (:boolean l)) :spec {:boolean (rand-nth (list true false))}))


(c/defn ^{:doc ""} show-gen*
  [^{:doc "fct-object"} object]
  (:fct/gen (c/meta object)))

(c/defn ^{:doc "variables on which the object depends"} deps*
  [^{:doc "fct-object"} object]
  (nested-keys (show-gen* object)))

(c/defn ^{:doc "is pure fct object?"} fct?*
  [^{:doc "fct object"} object]
  (:fct/? (c/meta object)))

(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object"} a]
  (if (fct?* a)
    
    (c/let [gen (:fct/gen (c/meta a))]
      (ev* a gen))
    
    a))

;; (def ^{:private true :doc "1. example for gen* in ns fct.core"} ex1-gen*
;;   (gen* (var* :boolean (rand-nth (list true false)))))

;; (def ^{:private true :doc "2. example for gen* in ns fct.core"} ex2-gen*
;;   (gen* (var* :a (fn [x] (map (var* :b (rand-fn (fn [] (rand-nth (list true false)))))
;;                               (range (rand-int x)))))))



(c/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object"} object]
   (c/let [key (if (c/keyword? key) [key] key)]
     (construct* (c/fn [l] (c/get-in l key))
                 :gen (c/assoc-in {} key (gen* object))))))

;; (def ^{:private true :doc "1. example for var* in ns fct.core"} ex1-var*
;;   (if-else (var* :bool)
;;            (var* :a)
;;            (var* :b)))


(c/defn ^{:doc "incognito variable construction"} incognito-var*
  [^{:doc "keyword attached to the variable"} key]
  (c/let [key (if (c/keyword? key) [key] key)]
    (construct* (c/fn [l] (c/get-in l key)))))


(c/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn]
  (construct* (c/fn [l] clojure-fn)))

;; (def ^{:private true :doc "1. example for lift* in ns fct.core"} ex1-lift*
;;   ((lift* c/+) (var* :h) (var* :a)))

;; ;; this works:
;; (c/defn new-if [test a b]
;;   (if test
;;     (a)
;;     (b)))
 

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


;; (clojure.set/intersection (c/into #{} (c/map c/first (c/ns-publics 'fct.core)))
;;                           (c/into #{} (c/map #(:name %) (find-functions 'clojure.core))))

;; (clojure.set/intersection (c/into #{} (c/map #(:name %) (find-functions 'clojure.spec.alpha)))
;;                           (c/into #{} (c/map #(:name %) (find-functions 'clojure.core))))

(def ^{:private true} lift-all (c/fn [n]
                                 (c/let [names (c/map #(:name %) (find-functions n))
                                         intersection (clojure.set/intersection (c/into #{} (c/map c/first (c/ns-publics 'fct.core)))
                                                                                (c/into #{} names))
                                         difference (c/into [] (clojure.set/difference (c/into #{} names) intersection))]
                                   (c/doseq [k difference]
                                     (c/eval (c/read-string (c/str "(def " (c/str k) " (lift* " (c/str n) "/" (c/str k) "))")))))))

(lift-all 'clojure.core)



(c/defn ^{:private true :doc "only interpretation is touched"}
  inter-sub* [^{:doc "fct object"} object
              ^{:doc "substitution map"} l]
  
  (c/let [sub (c/fn [inter]
                (c/fn [w]
                  (inter (nested-merge (nested-into (c/map (c/fn [[k v]] [k (ev* v w)])
                                                           (nested-key-value l)))
                                       w))))
          m (c/-> object c/meta)]
    (if (:fct/? m)
      (c/with-meta object (c/update m :fct/inter (c/fn [inter] (sub inter))))
      object)))


(c/defn ^{:private true :doc "only gen is touched"}
  gen-for-sub* [^{:doc "fct object"} object
                ^{:doc "substitution map"} l]
  
  (c/let [keys (c/into #{} (nested-keys l))
          l-kv (nested-key-value l)
          l-gen (nested-key-value
                 (c/apply nested-merge (c/map (c/fn [[_ v]] (show-gen* v))
                                              l-kv)))
          m (c/-> object c/meta)]
    (if (:fct/? m)
      (c/let [remove-gen (c/loop [r (:fct/gen m)
                                  keys (c/into '() keys)]
                           (if (c/empty? keys)
                             r
                             (c/let [[k] keys]
                               (recur (dissoc-in r k) (c/rest keys)))))
              new-gen (nested-merge remove-gen (nested-into l-gen))]
        {:gen new-gen})
      {})))


;;sub* is too long and difficult this way, because the values of inter have fct-objects in their meta data; maybe it's better to have fct-objects only appearing in meta data of fct-objects  

(clojure.core/defn ^{:doc "substitution of variables"} sub*
  [^{:doc "fct object"} object
   ^{:doc "map providing the substitutions for the variables"} l
   &
   {:keys [^{:doc "key, as in ev*"} key]
    :or {key :fct/spec}}]
  
  (if (fct?* object)
    (c/let [simple-inter-sub* (c/fn [inter w] (inter (nested-merge (nested-into (c/map (c/fn [[k v]] [k (ev* v w)])
                                                                                       (nested-key-value l)))
                                                                   w)))
            inter  (c/-> object c/meta :fct/inter) 
            new-inter (c/fn [w] (c/let [after-sub* (simple-inter-sub* inter w)
                                        m (c/meta after-sub*)
                                        meta-obj (key m)
                                        new-m (if meta-obj
                                                (c/assoc m key (inter-sub* meta-obj l))
                                                m)]
                                  (if meta-obj
                                    (c/with-meta after-sub* new-m)
                                    (if m
                                      (c/with-meta after-sub* m)
                                      after-sub*))))
            gen (gen-for-sub* object l)]
      (construct* new-inter :gen (:gen gen)))
    object))
  
;; (def ^{:private true :doc "1. example for sub* in ns fct.core"} ex1-sub*
;;   (sub* (+ 1 (var* :a)) {:a (var* :b)}))

;;(def t (sub* (var* :a (var* :b 4)) {:b (var* :c 40)}))


(c/defmacro ^{:doc "generic lifting of macros"} lift-macro
  [^{:doc "clojure macro"} macro
   & ^{:doc "arguments for the macro"} arg]
  
  (c/let [l (c/gensym 'fct_lift_macro_arg)
          body (c/cons macro (c/reverse (c/loop [a arg
                                                 na '()]
                                          (if a
                                            (c/let [f (c/first a)]
                                              (recur (c/next a) (c/cons (c/list 'fct.core/ev* f l)  na)))
                                            na))))
          for-gen (c/loop [a arg
                            na '()]
                     (if a
                       (c/let [f (c/first a)]
                         (recur (c/next a) (c/cons (c/list :fct/gen (c/list c/meta f))  na)))
                       na))]
    `(fct.core/construct* (c/fn [~l]
                            ~body)
                          :gen (fct.core/nested-merge ~@for-gen))))
 
;; (c/defmacro ^{:private true :doc "1. example for lift-macro in ns fct.core"} ex1-lift-macro [& args]
;;   `(lift-macro c/cond ~@args))


(c/defmacro throw [& args]
  `(fct.core/lift-macro throw ~@args))

(c/defmacro if [& args]
  `(fct.core/lift-macro if ~@args))

;; just a copy
(c/defmacro if-else [& args]
  `(fct.core/if ~@args))


(c/defmacro cond [& args]
  `(fct.core/lift-macro c/cond ~@args))


(c/defmacro lazy-seq [& args]
  `(fct.core/lift-macro c/lazy-seq ~@args))

;(ev* (lazy-seq (list (var* :a))) {:a 5})

(c/defmacro and [& args] `(fct.core/lift-macro c/and ~@args))

(c/defmacro or [& args] `(fct.core/lift-macro c/or ~@args))

(c/defmacro do [& args] `(fct.core/lift-macro do ~@args))

;(c/defmacro . [& args] `(lift-macro . ~@args))

(c/defmacro -> [& args] `(c/-> ~@args))

(c/defmacro ->> [& args] `(c/->> ~@args))

(c/defmacro ^{:doc "fn without recursion"} fnn [& sigs] 
  (c/let [;; destructuring sigs
          [^{:doc "arguments"} args
           o b] sigs 
          ^{:doc "additional options, e.g. {:gen ...}"} opt (if (c/map? o) o nil)
          ^{:doc "body (only one!)"} body (if opt b o)
          
          ;; real start
          arg# (c/gensym 'fctarg__)
          d# (c/destructure [args arg#])
          m# (c/map (c/fn [[x y]] [(c/keyword (c/gensym x)) x])
                  (c/partition 2 d#))
          m1# (c/map c/second m#)
          m2# (c/map c/first m#)]
    (c/cond
      
      (c/= args [])
      `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map fct.core/incognito-var* (clojure.core/list ~@m2#)))]
                                   ~body)
                          ev-gen# (:fct/gen (clojure.core/meta to-ev#))]
         (fct.core/construct* (clojure.core/fn [l#]
                                (clojure.core/with-meta (clojure.core/fn [& ~arg#]
                                                          (fct.core/ev* to-ev#
                                                                        (clojure.core/merge l# (clojure.core/let [~@d#]
                                                                                                 (clojure.core/into {} (clojure.core/list ~@m#))))))
                                  {:fct/? false :fct/fcn? true :fct/spec {}}))
                              :gen ev-gen#))
      
      (c/or (:spec opt) (:gen opt))
      `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map fct.core/incognito-var* (clojure.core/list ~@m2#)))]
                                   ~body)
                          ev-gen# (:fct/gen (clojure.core/meta to-ev#))
                          args-spec# (if (:spec ~opt)
                                       (:spec ~opt)
                                       (:gen ~opt))]
         
         (fct.core/construct* (clojure.core/fn [l#]
                                (clojure.core/with-meta (clojure.core/fn [& ~arg#]
                                                          (fct.core/ev* to-ev#
                                                                        (clojure.core/merge l# (clojure.core/let [~@d#]
                                                                                                 (clojure.core/into {} (clojure.core/list ~@m#))))))
                                  {:fct/? false :fct/fcn? true :fct/spec args-spec#}))
                              :gen (if (:fct/? (c/meta args-spec#))
                                     (fct.core/nested-merge (:fct/gen (c/meta args-spec#))
                                                           ev-gen#)
                                     ev-gen#)))

      :else
      `(clojure.core/let
           [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map fct.core/incognito-var* (clojure.core/list ~@m2#)))]
                     ~body)
            ev-gen# (:fct/gen (clojure.core/meta to-ev#))]
         (fct.core/construct* (clojure.core/fn [l#]
                                (clojure.core/fn [& ~arg#]
                                  (fct.core/ev* to-ev#
                                                (clojure.core/merge l# (clojure.core/let [~@d#]
                                                                         (clojure.core/into {} (clojure.core/list ~@m#)))))))
                              :gen ev-gen#)))))



(c/defmacro ^{:doc "almost usual syntax (body is required (only one))"}
  let
  [^{:doc "bindings, deconstruction works"} bindings
   ^{:doc "the body"} body]
 
  (if (c/empty? bindings)
    body
    (c/let [[arg# val#] bindings
            rec# (c/rest (c/rest bindings))]
      `((fct.core/fnn [~arg#] (fct.core/let [~@rec#] ~body))
        ~val#))))

;; (def ^{:private true :doc "1. example for let in ns fct.core"} ex1-let
;;   (let [{:keys [some]} (var* :a)] some))


(c/defmacro fn [& sigs]
  (c/let [[x y o b] sigs
          ^{:doc "name, used for recursion (optional)"} name (if (c/symbol? x) x nil)
          ^{:doc "args (required)"} args (if name y x)
          [o b] (if name [o b] [y o])
          ^{:doc "additional options, e.g. {:gen ...} (optional)"} opt (if (c/map? o) o nil)
          ^{:doc "body (only one!) (required)"} body (if opt b o)]

    (c/cond (c/and name opt)
            `(fct.core/fnn [& a#] ~opt (fct.core/let [h# (fct.core/fnn [self# & a#]
                                                                       (fct.core/let [~name (fct.core/fnn [& a#] (fct.core/apply self# (fct.core/conj a# self#)))
                                                                                      ~args a#]
                                                                         ~body))]
                                         (fct.core/apply h# (fct.core/conj a# h#))))
            
            name
            `(fct.core/fnn [& a#] (fct.core/let [h# (fct.core/fnn [self# & a#]
                                                                  (fct.core/let [~name (fct.core/fnn [& a#] (fct.core/apply self# (fct.core/conj a# self#)))
                                                                                 ~args a#]
                                                                    ~body))]
                                    (fct.core/apply h# (fct.core/conj a# h#))))
            
            opt
            `(fct.core/fnn ~args ~opt ~body)
            
            "else"
            `(fct.core/fnn ~args ~body))))

;; (def ^{:private true :doc "1. example for fn in ns fct.core"} ex1-fn
;;   (fn [x] x))

;; (def ^{:private true :doc "2. example for fn in ns fct.core"} ex2-fn
;;   (fn [x] {:gen (fn [] (vector (rand-int 100)))}
;;     x))


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

      (c/= nil (:fct/? m)) nil

      true  (c/cond
              
              (:fct/? m)
              {:args nil
               :ret (gen* f)}                    
              
              (:fct/fcn? m)
              (if (c/not (c/or (c/= spec-structure nil) (c/= spec-structure {})))
                (c/let [a (spec-structure)]
                  {:args a
                   :ret (c/apply f a)})
                
                {:args []
                 :ret (f)})))))


(c/defn gcheck* [f]
  (check* (gen* f)))


;; (def ^{:private true :doc "1. example for gcheck* in ns fct.core"} ex1-gcheck*
;;   (gcheck* (fn [x] {:gen (fn [] (vector (rand-int 100)))}
;;              x)))


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

;; (def ^{:private true :doc "1. example for ftest* in ns fct.core"} ex1-ftest*
;;   (ftest* (fn [x] {:gen (fn [] (vector (rand-int 100)))}
;;             (x 1))))
 
;;
;; loop
;;

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
    
    `(fct.core/loopf ((fct.core/lift* c/vector) ~@data#)
                     (fct.core/fnn [~args#] ~test)
                     (fct.core/fnn [~args#] ~rec)
                     (fct.core/fnn [~args#] ~ret))))
 
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


(defn ^{:doc "random collection"} rand-coll
  [^{:doc "list of elements the collection can consist of"} l
   ^{:doc "number of elements in the collection"} i]
  
  (if-else (empty? l)
           '()
           (map (fn [] (rand-nth l)) (range i))))

;; (def ^{:private true :doc "1. example for rand-coll in ns fct.core"} ex1-rand-coll
;;   (gen* (rand-coll '(1 2 3 4) 5)))

