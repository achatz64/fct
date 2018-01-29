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

(c/defn ^{:doc "dependence tree of an fct object"} deps-tree*
  [^{:doc "fct object"} object
   ^{:doc "map providing substitutions for some variables, those variables will be considered dead ends of the dependence tree"} var-map]
  
  (c/let [get-spec (c/fn [object] (c/let [m (c/meta object)]
                                    (if (:fct/? m)
                                      (:fct/spec m)
                                      {})))
          add-spec (c/fn [vector-keys object]
                     (c/map (c/fn [[k v]] (if (c/contains? var-map k)
                                            (c/vector (c/conj vector-keys k)
                                                      (k var-map))
                                            (c/vector (c/conj vector-keys k)
                                                      v)))
                            (get-spec object)))
          remove-subs (c/fn [layer]
                        (c/filter (c/fn [[kv o]] (c/not (c/contains? var-map (c/peek kv))))
                                  layer))]
    (c/loop
        [l (c/list (add-spec [] object))
         new (add-spec [] object)]
      (if (c/empty? new)
        (if (c/empty? (c/first l))
          (c/rest l)
          l)
        (c/let [rec-new (c/apply c/concat (c/map (c/fn [[vk o]] (add-spec vk o))
                                                 (remove-subs new)))]
          (recur (c/conj l rec-new)
                 rec-new))))))

;; (def ^{:private true :doc "1. example for deps-tree* in ns fct.core"} ex1-deps-tree*  (deps-tree* (var* :a (fn [x] ((var* :b (var* :c)) x))) {}))

;; (def ^{:private true :doc "2. example for deps-tree* in ns fct.core"} ex2-deps-tree*  (deps-tree* (var* :a (fn [x] ((var* :b (var* :c)) x))) {:b (var* :d)}))

(c/defn ^{:doc "describes all variables on which the object depends"} deps-list*
  [^{:doc "fct object"} object]

  (c/let [start (deps-tree* object {})]
    (c/map (c/fn [l] (c/into {} (c/map (c/fn [[x y]] [(c/peek x) y])
                                       l)))
           start)))

;; (def ^{:private true :doc "1. example for deps-list* in ns fct.core"} ex1-deps-list*  (deps-list* (var* :a (fn [x] ((var* :b (var* :c)) x)))))


(c/defn ^{:doc "lists all variables on which the object depends"} deps*
  [^{:doc "fct-object"} object]
  (c/into #{} (c/apply c/concat (c/map c/keys (c/-> object deps-list*)))))



(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object"} a]
  
  (c/let [^{:doc "loop creating the map which provides the interpretations for the variables"} l
          (c/loop [d (deps-list* a)
                   l {}]
            (if (c/empty? d)
              l
              (c/let [f (c/first d)
                      ev-f (c/into {} (c/map (c/fn [[k v]] [k (ev* v l)])
                                             f))]
                (recur (c/rest d) (c/merge ev-f l)))))]
    (ev* a l :key :fct/spec)
    ))

;; (def ^{:private true :doc "1. example for gen* in ns fct.core"} ex1-gen*
;;   (gen* (var* :boolean (rand-nth (list true false)))))

;; (def ^{:private true :doc "2. example for gen* in ns fct.core"} ex2-gen*
;;   (gen* (var* :a (fn [x] (map (var* :b (rand-fn (fn [] (rand-nth (list true false)))))
;;                               (range (rand-int x)))))))



(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter
   & {:keys [^{:doc "examples for the variables"} spec]
      :or {spec {}}}]

  (c/with-meta
    (c/fn [& args] (construct*
                    (c/fn [l]
                      (c/apply (inter l) (c/map #(ev* % l) args)))
                    :spec (c/apply c/merge (c/conj (c/map #(c/-> % c/meta :fct/spec)
                                                        args)
                                                   spec))))
    {:fct/? true
     :fct/inter inter
     :fct/spec spec}))

;; (def ^{:private true :doc "1. example for construct* in ns fct.core"} ex1-construct*
;;   (construct* (c/fn [l] (:a l)))) 

;; (def ^{:private true :doc "2. example for construct* in ns fct.core"} ex2-construct*
;;   (construct* (c/fn [l] (:boolean l)) :spec {:boolean (rand-nth (list true false))}))


(c/defn ^{:private true :doc "Used in sub*"} collaps-tree
  [^{:doc "as returned by deps-tree*"} tree
   ^{:doc "as in deps-tree*"} var-map]
  
  (c/let [change-spec (c/fn [object spec-replacement]
                        (c/let [m (c/meta object)]
                          (if (:fct/? m)
                            (c/with-meta object
                              (c/assoc m :fct/spec spec-replacement))
                            object)))
          find-children (c/fn [layer kv]
                          (c/filter (c/fn [[x o]] (c/every? c/identity
                                                            (c/map c/= kv x)))
                                    layer))
          sub-spec (c/fn [children]
                     (c/let [children-remain (c/filter (c/fn [[x o]] (c/not (c/contains? var-map (c/peek x))))
                                                       children)
                             children-sub (c/filter (c/fn [[x o]] (c/contains? var-map (c/peek x)))
                                                    children)]
                       (c/merge
                        (c/apply c/merge (c/map (c/fn [[x o]] (:fct/spec (c/meta o)))
                                                children-sub))
                        (c/into {} (c/map (c/fn [[x o]] [(c/peek x) o])
                                          children-remain)))))
          collaps (c/fn [layer kvo]
                    (c/let [children (find-children layer (c/first kvo))
                            new-spec (sub-spec children)]
                      (if (c/empty? children)
                        kvo
                        [(c/first kvo) (change-spec (c/second kvo) new-spec)])))]
    (c/loop [tree tree]
      (if (c/empty? (c/rest tree))
        (if (c/empty? tree)
          tree
          (sub-spec (c/first tree)))
        
        (c/let [[f s] tree
                rec-s (c/map (c/fn [kvo] (collaps f kvo))
                             s)]
          (recur (c/conj (c/drop 2 tree)
                         rec-s)))))))

;; sub* is too long and difficult this way, because the values of inter have fct-objects in their meta data; maybe it's better to have fct-objects only appearing in meta data of fct-objects  

(clojure.core/defn ^{:doc "substitution of variables"} sub*
  [^{:doc "fct object"} object
   ^{:doc "map providing the substitutions for the variables"} l
   &
   {:keys [^{:doc "key, as in ev*"} key]
    :or {key :fct/spec}}]
  
  (if (c/-> object c/meta :fct/?)
    (c/let [spec-sub* (c/fn [object] (c/-> object (deps-tree* l) (collaps-tree l)))
            simple-inter-sub* (c/fn [inter w] (inter (c/merge w (c/into {} (c/map (c/fn [[k v]] [k (ev* v w)])
                                                                           l)))))
            inter-sub* (c/fn [object] (c/let [m (c/-> object c/meta)]
                                        (if (:fct/? m)
                                          (c/with-meta object (c/update m :fct/inter (c/fn [inter] (c/fn [w] (simple-inter-sub* inter w)))))
                                          object)))
            inter  (c/-> object c/meta :fct/inter) 
            new-inter (c/fn [w] (c/let [after-sub* (simple-inter-sub* inter w)
                                        m (c/meta after-sub*)
                                        meta-obj (key m)
                                        new-m (if meta-obj
                                                (c/assoc m key (inter-sub* meta-obj))
                                                m)]
                                  (if meta-obj
                                    (c/with-meta after-sub* new-m)
                                    (if m
                                      (c/with-meta after-sub* m)
                                      after-sub*))))]
      (construct* new-inter :spec (spec-sub* object)))
    object))
  
;; (def ^{:private true :doc "1. example for sub* in ns fct.core"} ex1-sub*
;;   (sub* (+ 1 (var* :a)) {:a (var* :b)}))


(c/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "spec or fct object"} spec]
   (if (c/keyword? key)
     (construct* (c/fn [l] (key l))
              :spec (c/hash-map key spec))
     (throw (Exception. "fct: var* expects a keyword as first argument")))))

;; (def ^{:private true :doc "1. example for var* in ns fct.core"} ex1-var*
;;   (if-else (var* :bool)
;;            (var* :a)
;;            (var* :b)))


(c/defn ^{:doc "incognito variable construction"} incognito-var*
  [^{:doc "keyword attached to the variable"} key]
  (construct* (c/fn [l] (key l))))


(c/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn]
  (construct* (c/fn [l] clojure-fn)))

(def ^{:private true :doc "1. example for lift* in ns fct.core"} ex1-lift*
  ((lift* c/+) (var* :h) (var* :a)))

;; ;; this works:
;; (c/defn new-if [test a b]
;;   (if test
;;     (a)
;;     (b)))


(c/defmacro ^{:doc "generic lifting of macros"} lift-macro
  [^{:doc "clojure macro"} macro
   & ^{:doc "arguments for the macro"} arg]
  
  (c/let [l (c/gensym 'fct_lift_macro_arg)
          body (c/cons macro (c/reverse (c/loop [a arg
                                                 na '()]
                                          (if a
                                            (c/let [f (c/first a)]
                                              (recur (c/next a) (c/cons (c/list 'ev* f l)  na)))
                                            na))))
          for-spec (c/loop [a arg
                            na '()]
                     (if a
                       (c/let [f (c/first a)]
                         (recur (c/next a) (c/cons (c/list :fct/spec (c/list c/meta f))  na)))
                       na))]
    `(construct* (c/fn [~l]
                   ~body)
                 :spec (c/merge ~@for-spec))))
 
;; (c/defmacro ^{:private true :doc "1. example for lift-macro in ns fct.core"} ex1-lift-macro [& args]
;;   `(lift-macro c/cond ~@args))


(c/defmacro throw [& args]
  `(lift-macro throw ~@args))

(c/defmacro if [& args]
  `(lift-macro if ~@args))

;; just a copy
(c/defmacro if-else [& args]
  `(fct.core/if ~@args))


(c/defmacro cond [& args]
  `(lift-macro c/cond ~@args))

;; (c/defmacro cond
;;   [& clauses]
;;   (c/let [c (c/reverse clauses)]
;;     (c/loop [nc (c/next (c/next c))
;;              r (c/list 'fct.core/if (c/second c) (c/first c) nil)]
;;       (if nc
;;         (recur (c/next (c/next nc))
;;                (c/list 'fct.core/if  (c/second nc) (c/first nc) r))
;;         r))))

(c/defmacro lazy-seq [& args]
  `(lift-macro c/lazy-seq ~@args))

;(ev* (lazy-seq (list (var* :a))) {:a 5})

(c/defmacro and [& args] `(lift-macro c/and ~@args))

(c/defmacro or [& args] `(lift-macro c/or ~@args))

(c/defmacro do [& args] `(lift-macro do ~@args))

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
      `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map incognito-var* (clojure.core/list ~@m2#)))]
                                   ~body)
                          ev-spec# (:fct/spec (clojure.core/meta to-ev#))]
         (construct* (clojure.core/fn [l#]
                       (clojure.core/with-meta (clojure.core/fn [& ~arg#]
                                                 (ev* to-ev#
                                                      (clojure.core/merge l# (clojure.core/let [~@d#]
                                                                               (clojure.core/into {} (clojure.core/list ~@m#))))))
                         {:fct/? false :fct/fcn? true :fct/spec {}}))
                     :spec ev-spec#))
      
      (c/or (:spec opt) (:gen opt))
      `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map incognito-var* (clojure.core/list ~@m2#)))]
                                   ~body)
                          ev-spec# (:fct/spec (clojure.core/meta to-ev#))
                          args-spec# (if (:spec ~opt)
                                       (:spec ~opt)
                                       (:gen ~opt))]
         (construct* (clojure.core/fn [l#]
                      (clojure.core/with-meta (clojure.core/fn [& ~arg#]
                                                (ev* to-ev#
                                                    (clojure.core/merge l# (clojure.core/let [~@d#]
                                                                             (clojure.core/into {} (clojure.core/list ~@m#))))))
                        {:fct/? false :fct/fcn? true :fct/spec args-spec#}))
                     :spec (if (:fct/? (c/meta args-spec#))
                             (c/merge (:fct/spec (c/meta args-spec#))
                                      ev-spec#)
                             ev-spec#)))

      :else
      `(clojure.core/let
             [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map incognito-var* (clojure.core/list ~@m2#)))]
                       ~body)
              ev-spec# (:fct/spec (clojure.core/meta to-ev#))]
           (construct* (clojure.core/fn [l#]
                        (clojure.core/fn [& ~arg#]
                          (ev* to-ev#
                              (clojure.core/merge l# (clojure.core/let [~@d#]
                                                       (clojure.core/into {} (clojure.core/list ~@m#)))))))
                       :spec ev-spec#)))))



(c/defmacro ^{:doc "almost usual syntax (body is required (only one))"}
  let
  [^{:doc "bindings, deconstruction works"} bindings
   ^{:doc "the body"} body]
 
  (if (c/empty? bindings)
    body
    (c/let [[arg# val#] bindings
            rec# (c/rest (c/rest bindings))]
      `((fnn [~arg#] (let [~@rec#] ~body))
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
            `(fnn [& a#] ~opt (let [h# (fnn [self# & a#]
                                           (let [~name (fnn [& a#] (apply self# (conj a# self#)))
                                                 ~args a#]
                                             ~body))]
                               (apply h# (conj a# h#))))
            
            name
            `(fnn [& a#] (let [h# (fnn [self# & a#]
                                       (let [~name (fnn [& a#] (apply self# (conj a# self#)))
                                             ~args a#]
                                         ~body))]
                           (apply h# (conj a# h#))))
            
            opt
            `(fnn ~args ~opt ~body)
            
            "else"
            `(fnn ~args ~body))))

;; (def ^{:private true :doc "1. example for fn in ns fct.core"} ex1-fn
;;   (fn [x] x))

;; (def ^{:private true :doc "2. example for fn in ns fct.core"} ex2-fn
;;   (fn [x] {:gen (fn [] (vector (rand-int 100)))}
;;     x))


(c/defmacro defn [name & sigs]
  `(def ~name (fn ~name ~@sigs)))


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


(def ^{:private true :doc "1. example for gcheck* in ns fct.core"} ex1-gcheck*
  (gcheck* (fn [x] {:gen (fn [] (vector (rand-int 100)))}
             x)))


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
    
    `(loopf ((lift* c/vector) ~@data#)
            (fnn [~args#] ~test)
            (fnn [~args#] ~rec)
            (fnn [~args#] ~ret))))
 
;; (gen* (loop [x '(1 2 3) y '()]
;;         (= 0 (count x))
;;         (rec (rest x) (conj y (first x)))
;;         y))

;; ;; there is no if, need to use cond, behaves almost like in clojure

;; (def ^{:private true} cond* (c/fn [& args]
;;                               (loopn* args
;;                                       #(c/cond
;;                                          (c/empty? %) true
;;                                          (c/empty? (c/rest %)) true
;;                                          (c/let [[x y] %] x) true
;;                                          :else false)
;;                                       #(c/-> % c/rest c/rest)
;;                                       #(if (c/<= (c/count %) 1)
;;                                          (c/first %)
;;                                          (c/second %)))))

;; (def cond (lift* cond*))

;(def if-else cond)



;; ;; works but rf? is better
;; (c/defn rf [n]
;;   (c/let [a (c/first n)]
;;     (if (c/= a 0)
;;       (c/str "Done")
;;       (c/lazy-seq (rf (c/cons (c/dec a) n))))))

;; ;; works
;; (c/defn rf? [n]
;;   (c/let [a (c/first n)]
;;     (if (c/= a 0)
;;       (c/list (c/str "Done"))
;;       (c/lazy-seq (rf? (c/map c/dec n))))))

;; ;;works
;; (c/defn rf?? [n]
;;   (c/lazy-seq
;;    (c/let [a (c/first n)]
;;      (if (c/= a 0)
;;        (c/list (c/str "Done"))
;;        (rf?? (c/map c/dec n))))))

;; ;; doesn't work
;; (c/defn rf??? [n]
;;   (c/lazy-seq (c/list
;;                (c/let [a (c/first n)]
;;                  (if (c/= a 0)
;;                    (c/str "Done")
;;                    (c/first (rf??? (c/list (c/dec a)))))))))

;; (defn t [n] {:spec (fn [] (vector (rand-int 5)))}
;;   (if-else (= n 0)
;;            0
;;            (/ 1 n)))



;; (c/defn s [a time-consuming]
;;   (if a
;;     "Done"
;;     time-consuming))

;; (c/time (s true (c/time (c/apply c/+ (c/range 100000000)))))

;; (c/defn s [a time-consuming]
;;   (if a
;;     "Done"
;;     (time-consuming)))

;; (c/time (s true (fn [] (c/apply c/+ (c/range 100000000)))))

;; (c/defn s [a time-consuming]
;;   (new-if a
;;           (c/fn [] "Done")
;;           time-consuming))

;; (c/time (s true (c/fn [] (c/apply c/+ (c/range 100000000)))))

;; (c/defn t [n]
;;   (c/let [lazy (c/lazy-seq (c/cons 0 (c/list (c// 1 n))))]
;;     (c/take 1 lazy)))

;; (t 0)

;; this works:
;; (c/defn s [a time-consuming]
;;   (new-if a
;;           (c/fn [] "Done")
;;           (c/first time-consuming)))

;; (c/time (s true (c/lazy-seq (c/list (c/fn [] (c/apply c/+ (c/range 100000000)))))))




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

(def ^{:private true :doc "1. example for rand-fn in ns fct.core"} ex1-rand-fn
  (gen* (rand-fn (fn [] (rand-nth '(true false))))))


(defn ^{:doc "random collection"} rand-coll
  [^{:doc "list of elements the collection can consist of"} l
   ^{:doc "number of elements in the collection"} i]
  
  (if-else (empty? l)
           '()
           (map (fn [] (rand-nth l)) (range i))))

;; (def ^{:private true :doc "1. example for rand-coll in ns fct.core"} ex1-rand-coll
;;   (gen* (rand-coll '(1 2 3 4) 5)))



