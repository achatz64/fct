(ns fct.core
  (:refer-clojure :only [])
  (:require
   [clojure.core :as c]
   [clojure.set]))

;; [clojure.spec.alpha :as s] 
;;    [clojure.spec.gen.alpha :as gen]
;;    [clojure.spec.test.alpha :as stest]

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
;;        {:x ["?"] :y 5 :z (clojure.core/* -1 5)}))

;; (def ^{:private true :doc "2. example for ev* in ns fct.core"} ex2-ev*
;;   (ev* (fn [a] (+ (var* :y) a))
;;        {:y 5}))

(c/defn deps-tree* [object var-map]
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

(c/defn deps-list* [object]
  (c/let [start (deps-tree* object {})]
    (c/map (c/fn [l] (c/into {} (c/map (c/fn [[x y]] [(c/peek x) y])
                                       l)))
           start)))


;; (c/defn ^{:doc "finding the total spec"} total
;;   [^{:doc "map of fct objects"} spec-structure]
  
;;   (if (c/or (c/= nil spec-structure)
;;             (c/empty? spec-structure))
;;     {}
;;     (c/let [new-specs (c/map (c/fn [[k v]] (:fct/spec (c/meta v)))
;;                              spec-structure)]
;;       (c/merge spec-structure (c/apply c/merge
;;                                        (c/map total new-specs))))))

;(def t (var* :a (var* :b (map first (var* :l c/list?)))))
;(total (:fct/spec (c/meta t)))

;; (c/defn ^{:doc "resolves the dependencies"} deps
;;   [^{:doc "map of fct objects"} spec-structure]

;;   (c/let [^{:doc "checks whether dependencies are satisfied"} deps?
;;           (c/fn [spec-structure set-of-keys]
;;             (clojure.set/subset? (c/into #{} (c/keys spec-structure))
;;                                  set-of-keys))

;;           spec-structure (total spec-structure)]
;;     (c/loop [l '() spec-structure spec-structure ok-keys {}]
;;       (if (c/empty? spec-structure)
;;         (c/reverse l)
;;         (c/let [^{:doc "depend only on ok-keys"} independent
;;                 (c/into {} (c/filter (c/fn [[k v]] (deps? (:fct/spec (c/meta v))
;;                                                           ok-keys))
;;                                      spec-structure))
                
;;                 the-rest (c/into {} (c/filter (c/fn [[k v]] (c/not (deps? (:fct/spec (c/meta v))
;;                                                                           ok-keys)))
;;                                               spec-structure))]
;;           (recur (c/conj l independent)
;;                  the-rest
;;                  (clojure.set/union ok-keys
;;                                     (c/into #{} (c/keys independent)))))))))

(c/defn ^{:doc "shows on which variables the object depends"} deps*
  [^{:doc "fct-object"} object]
  (c/into #{} (c/apply c/concat (c/map c/keys (c/-> object deps-list*)))))



;; (def t (var* :a (var* :b (map (var* :f c/int?) (var* :l c/list?)))))
;;(deps (:fct/spec (c/meta (var* :a (lift* 3)))))


(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object or spec"} a]
  
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

;; (clojure.core/defn ^{:doc "generates a witness"} gen-new*
;;   [^{:doc "fct object or spec"} a]
  
;;   (c/let [^{:doc "loop creating the map which provides the interpretations for the variables"} l
;;           (c/loop [d (deps-list* a)
;;                    l {}]
;;             (if (c/empty? d)
;;               l
;;               (c/let [f (c/first d)
;;                       ev-f (c/into {} (c/map (c/fn [[k v]] [k (ev* v l)])
;;                                              f))]
;;                 (recur (c/rest d) (c/merge ev-f l)))))]
;;     l
;;     ))


;; (gen* (var* :x c/int?))
;; (gen* (var* :a (var* :b (map first (var* :l (s/coll-of (s/tuple c/string? c/int?)))))))


;; (def ^{:privat true :doc "1. example for gen* in ns fct.core"} ex1-gen*
;;   (gen* (vari :a clojure.core/boolean?)))

;; (def ^{:privat true :doc "2. example for gen* in ns fct.core"} ex2-gen*
;;   (gen* (fn [b] ((vari :a (fspec* (fn [x] (if-exception (int? x)
;;                                                         true 
;;                                                         "not an integer"))
;;                                   clojure.core/int?))
;;                  b))))


(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning maps with variable bindings (like l in ev*) a clojure object"} inter
   & {:keys [^{:doc "specs for the variables"} spec
             ^{:doc "an alternative for inter used for function testing"} gen]
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


(c/defn collaps-tree [^{:doc "as returned by deps-tree*"} tree
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


;; (def ^{:private true :doc "1. example for construct* in ns fct.core; constructs a plain version of the variable :x"} ex1-construct*
;;   (construct* (c/fn [l] (:x l))))


;; (c/defn vari-old [key]
;;   (construct* (c/fn [l] (key l))))

;; (c/defn ^{:doc "substitution"} sub*
;;   [^{:doc "fct object"} object
;;    ^{:doc "map providing the interpretations for some variables"} sub-l]

;;   (c/let [update-spec (c/into {}
;;                               (c/map (c/fn [[k v]] (if (c/contains? sub-l k)

;;                                                      (c/let [new-value (k sub-l)]
;;                                                        [k (s/spec #{new-value})])

;;                                                      [k v]))
;;                                      (:fct/spec (c/meta object))))
          
;;           rec-sub (c/fn [a] (c/let [m (c/meta a)]
;;                               (if (:fct/? m)
;;                                 (sub* a sub-l)
;;                                 a)))]
    
;;     (construct* (c/fn [l] (ev* object (c/merge l sub-l)))
                
;;                 :spec (c/into {} (c/map (c/fn [[k v]] [k (rec-sub v)])
;;                                         update-spec)))))


;; (def t (var* :a (var* :b c/int?)))

;; (def nt (sub* t {:b 0}))
;;(ev* nt {:a 5})
;; => 5 ; that's intended (gen* nt) will return 0



(c/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "spec or fct object"} spec]
   (if (c/keyword? key)
     (construct* (c/fn [l] (key l))
              :spec (c/hash-map key spec))
     (throw (Exception. "fct: var* expects a keyword as first argument")))))

(c/defn ^{:doc "incognito variable construction"} incognito-var*
  [^{:doc "keyword attached to the variable"} key]
  (construct* (c/fn [l] (key l))))

;; (def ^{:private true} rand-digit (c/fn [n] (c/loop [r (c/range n)
;;                                                     v '()]
;;                                              (if (c/empty? r)
;;                                                (c/apply c/str v)
;;                                                (recur (c/rest r) (c/conj v
;;                                                                          (c/rand-int 10)))))))

;; (def ^{:private true} free-key (c/fn [n]
;;                                   (c/keyword (c/str "free_anonymous__" (rand-digit n)))))

;; (c/defmacro ^{:doc "anonymous variable construction"} avar*
;;   [^{:doc "spec or fct object that can be generated with gen*"} spec]
;;   (c/let [key (free-key 100)]
;;     `(var* ~key ~spec)))

;; (def ^{:private true} free-key? (c/fn [key]
;;                                   (c/let [key (c/str key)
;;                                           first-17 (c/map c/str (c/str ":free_anonymous__"))]
;;                                     (c/and (c/= first-17 (c/take 17 (c/map c/str key)))
;;                                            (c/= 117 (c/count key))))))

;; (def ^{:private true} dissoc-free-keys (c/fn [l] (c/into {} (c/filter (c/fn [[k v]] (c/not (free-key? k)))
                                                                      ;; l))))
;; ;; (c/defn vari
;; ;;   ([arg]
;; ;;    (if (c/keyword? arg)
;; ;;      (construct* (c/fn [l] (arg l)))
;; ;;      (c/let [key (c/keyword (c/str (c/rand 5)))]
;; ;;        (vari key arg))))
;; ;;   ([key spec]
;; ;;    (construct* (c/fn [l] (key l))
;; ;;               :spec (if spec
;; ;;                       (c/hash-map key spec)
;; ;;                       {}))))

;; (def t (fn [x] {:spec {x c/int?}} ((vari (fn [x] true)) 4)))
;; (def bt (fn [x] {:spec (s/tuple c/int?)} ((vari (fn [x] true)) x)))

;; (def pt (vari (c/keyword (c/str (c/gensym "anon")))
;;               (fn [x] true)))

;; (def tt (fn [x] {:spec {x c/int?}} ((vari :anon (fn [x] true)) x)))



(c/defn lift* [clojure-fn]
  (construct* (c/fn [l] clojure-fn)))

;(c/meta ((lift* c/+) (vari :h c/int?) (vari :a c/int?)))


;; (c/defn hash-spec [hash]
;;   (s/with-gen
;;     (s/spec (c/fn [x] (if (c/map? x)
;;                         (c/let [xkeys (c/into #{} (c/keys x))]
;;                           (c/every? (c/fn [[k v]] (c/and (c/contains? xkeys k)
;;                                                          (s/valid? v (k x))))
;;                                     hash))
;;                         false)))
;;     (c/fn [] (s/gen #{(c/into {} (c/map (c/fn [[x y]] [x (gen/generate (s/gen y))])
;;                                         hash))}))))

(c/defmacro fnn [& sigs] 
  (c/let [;; destructuring sigs
          [^{:doc "arguments"} args
           o b] sigs 
          ^{:doc "additional options, e.g. {:spec ...}"} opt (if (c/map? o) o nil)
          ^{:doc "body (only one!)"} body (if opt b o)
          
          ;; real start
          arg# (c/gensym 'fctarg__)
          d# (c/destructure [args arg#])
          m# (c/map (c/fn [[x y]] [(c/keyword (c/gensym x)) x])
                  (c/partition 2 d#))
          m1# (c/map c/second m#)
          m2# (c/map c/first m#)]
    (c/cond
      
      ;; (c/map? (:spec opt))
      ;; (c/let [arg-keys# (c/into [] (c/keys (:spec opt)))
      ;;         spec# (c/into {} (c/map (c/fn [[x y]] [(c/keyword x) y])
      ;;                                 (:spec opt)))
      ;;         args-map# (c/into {} (c/map (c/fn [[x y]] [(c/keyword x) x])
      ;;                                     (:spec opt)))
      ;;         just-keys# (c/into [] (c/map (c/fn [[x y]] (c/keyword x))
      ;;                                    (:spec opt)))]
      ;;   `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map #(vari % clojure.core/int?) (clojure.core/list ~@m2#)))]
      ;;                                ~body)
      ;;                       ev-spec# (:fct/spec (clojure.core/meta to-ev#))
      ;;                       to-ev-gen# (clojure.core/let [{:keys ~arg-keys#}
      ;;                                                     (clojure.core/into {}
      ;;                                                                        (clojure.core/map (clojure.core/fn [x#] [x# (vari x#)])
      ;;                                                                                          ~just-keys#))]
      ;;                                    ~body)]
      ;;      (construct* (clojure.core/fn [l#]
      ;;                     (clojure.core/with-meta (clojure.core/fn [& ~arg#]
      ;;                                               (ev* to-ev#
      ;;                                                   (clojure.core/merge l# (clojure.core/let [~@d#]
      ;;                                                                            (clojure.core/into {} (clojure.core/list ~@m#))))))
      ;;                       {:fct/? false :fct/fcn? true :fct/name '~name}))
      ;;                   :spec ev-spec#
      ;;                   :gen (clojure.core/fn [l#]
      ;;                          (clojure.core/with-meta (clojure.core/fn [{:keys ~arg-keys#}]
      ;;                                                    (ev* to-ev-gen#
      ;;                                                        (clojure.core/merge l# ~args-map#)))
      ;;                            {:fct/? false :fct/fcn? true :fct/name '~name :fct/spec-map? true :fct/spec  ~spec#})))))
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
      
      (:spec opt)
      `(clojure.core/let [to-ev# (clojure.core/let [[~@m1#] (clojure.core/into [] (clojure.core/map incognito-var* (clojure.core/list ~@m2#)))]
                                   ~body)
                          ev-spec# (:fct/spec (clojure.core/meta to-ev#))
                          args-spec# (:spec ~opt)]
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



(c/defmacro let [bindings body]
  (if (c/empty? bindings)
    body
    (c/let [[arg# val#] bindings
            rec# (c/rest (c/rest bindings))]
      `((fnn [~arg#] (let [~@rec#] ~body))
        ~val#))))



;; (c/defn g [x] 
;;   (if (c/= x 0)
;;     0
;;     (c/+ x (g (c/dec x)))))


;; in clojure:
;;(def s (fn [arg self] (if (= arg 0) 0 (+ arg (self (dec arg) self)))))
;; (s 1000 s)
;; (def s (fn [args] (let [f  (fn [arg self] (if (= arg 0) 0 (+ arg (self (dec arg) self))))] (f args f))))
;; (s 1000)

;; in fct the following gives produces always a stackoverflow, 
;;(def s (fn [arg] (let [f (fn [arg self] (if-else (= arg 0) 0 (+ arg (self (dec arg) self))))] (f arg f))))

;;(def s (c/fn [] (c/let [f (c/fn [self] (c/fn [] (self self)))] (f f))))
;; (((s)))

;;(def s (fn [] (let [f (fn [self] (fn [] (self self)))] (f f))))

;; body depending on f, we want recursion for f
;; (fn [& args] (let [h (fn [self & args]
;;                        (let [f (fn [& args] (apply self (conj args self)))
;;                              [variables] (destructure' args)]
;;                          body))]
;;                (apply h (conj args h))))
 
;; (c/defmacro rec-fn [name args opt body]
;;   `(c/fn [& a#] (c/let [h# (c/fn [self# & a#]
;;                              (c/let [~name (c/fn [& a#] (c/apply self# (c/conj a# self#)))
;;                                      ~args a#]
;;                                ~body))]
;;                  (c/apply h# (c/conj a# h#)))))


(c/defmacro fn [& sigs]
  (c/let [[x y o b] sigs
          ^{:doc "name, used for recursion (optional)"} name (if (c/symbol? x) x nil)
          ^{:doc "args (required)"} args (if name y x)
          [o b] (if name [o b] [y o])
          ^{:doc "additional options, e.g. {:spec ...} (optional)"} opt (if (c/map? o) o nil)
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

;; (def s (with-meta (fn s [x] {:spec {}}
;;                     (with-meta (fn [y] (s y))
;;                       (hash-map :count (inc (get (meta x) :count))))) {:count 0}))

;; (:count (c/meta (ev* (s (s s)) {})))

(c/defmacro defn [name & sigs]
  `(def ~name (fn ~name ~@sigs)))


;; note that in the following example [3 (vari :h)] would not work
;; (ev (let [[x z] ((lift* c/list) 3 (vari :h))
;;           y ((lift* c/*) x 2)]
;;       ((lift* c/+) y x z))
;;     {:h 100})

;;(ev (let [x (vari :x) y (vari :y)] ((lift* c/+) x y)) {:x 1 :y 2})

;; (ev ((lift* c/vec) ((lift* c/list) 3 (vari :h)))
;;     {:h 5})

;; (c/time (ev ((lift* c/apply) (lift* c/+) ((lift* c/range) 10000000))
;;           {}))

;; (c/time (c/apply c/+ (c/range 10000000)))

;;
;; testing
;;

(def check* (c/fn  [f & {:keys [count-tests]
                         :or {count-tests 1}}]
              (c/let [m (c/meta f)
                      spec-structure (:fct/spec m)
                      test-loop (c/fn []
                                  (c/loop [t (c/range count-tests)
                                           
                                           ret '()]
                                    (if (c/empty? t)
                                      ret
                                      (recur (c/rest t) 
                                             (c/conj ret
                                                     (c/cond
                                                       
                                                       (:fct/? m)
                                                       {:args nil
                                                        :ret (gen* f)}                    
                                                       
                                                       (:fct/fcn? m)
                                                       (if (c/not (c/or (c/= spec-structure nil) (c/= spec-structure {})))
                                                         (c/let [a (spec-structure)]
                                                           {:args a
                                                            :ret (c/apply f a)})

                                                         {:args []
                                                          :ret (f)})))))))]
                (c/cond
                  (c/= nil (:fct/? m)) '()
                  true  (test-loop)))))


(c/defn gcheck* [f & {:keys [count-tests]
                      :or {count-tests 1}}]
  (check* (gen* f) :count-tests count-tests))

;; (def ^{:private true} con (c/fn [x y]
;;                             (c/loop [x x y y]
;;                               (if (c/empty? y)
;;                                 x
;;                                 (recur (c/conj x (c/peek y))
;;                                        (c/pop y))))))


(c/defn ftest* [f & {:keys [count-tests print?] :or {count-tests 1
                                                     print? false}}]
  (c/let [test-f (c/fn [x c] (c/map (c/fn [{:keys [ret]}] ret)
                                    (check* x :count-tests c)))
          
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
                                          (if (c/empty? y)
                                            (recur (c/rest l) ret)
                                            (recur (c/rest l) (c/conj ret (c/first y))))))))]
    (c/loop [l first-result]
      (if (c/empty? l)
        true
        (recur (inner-test-loop l))))))


(def test-f (c/fn [x c] (c/map (c/fn [{:keys [ret]}] ret)
                                    (check* x :count-tests c))))

;; (def s (fn [x] {:spec (args (rand-int 10))} x))
;; (check* (gen* s))

;;
;; some macros
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

;; (gen* (loopf (vector '(1 2 3) '())
;;              (fn [[x y]] (= 1 (count x)))
;;              (fn [[x y]] (vector (rest x) (conj y (first x))))
;;              (fn [[x y]] y)))

(def rec (lift* c/vector))


;; (c/defmacro ^{:doc ""} loop
;;   [args test-body iter-body ret-body]
;;   `(fn [& a#]
;;      (loopf a#
;;             (fn [~args] ~test-body)
;;             (fn [~args] ~iter-body)
;;             (fn [~args] ~ret-body))))

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

(def ^{:private true} cond* (c/fn [& args]
                              (loopn* args
                                      #(c/cond
                                         (c/empty? %) true
                                         (c/empty? (c/rest %)) true
                                         (c/let [[x y] %] x) true
                                         :else false)
                                      #(c/-> % c/rest c/rest)
                                      #(if (c/<= (c/count %) 1)
                                         (c/first %)
                                         (c/second %)))))

(def cond (lift* cond*))

(def if-else cond)

;(ev (if-else (vari :b c/boolean?) "nope" "Now!") {:b true})

(def and (lift* (c/fn [& args] (c/every? c/identity args))))

(def or (lift* (c/fn [& args] (c/some c/identity args))))

;; exceptions
(def if-exception (lift* (c/fn [test something etext]
                          (if test
                            something
                            (throw (Exception. etext))))))

;(def ^{:doc "lift of s/valid?"} valid? (lift* s/valid?))

;(def gen (lift* (c/fn [spec] (gen/generate (s/gen spec)))))


;; (def t (gen (free* (avar* c/int?))))

;; (def s (c/map (c/fn [x] (gen/generate (s/gen c/int?))) (c/range 10)))

;; (def s (free* (map (fn [x] (gen c/int?)) (range (rand-int 10)))))

;;
;; automatic lifting 
;;

;; (def clj  (c/keys (c/ns-publics 'clojure.core)))
;; (def spe (c/keys (c/ns-publics 'clojure.spec.alpha)))
 

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
;; around spec
;;

(def ^{:private true} string-to-0-99 (c/fn [string]
                                       (c/let [h (c/rem (c/hash string) 100)]
                                         (if (c/>= h 0)
                                           h
                                           (c/+ h 100)))))
 
;; (c/defn fct-gen [args-spec ret-spec name]
;;   (c/fn [] (s/gen #{(c/let [ret-samples (gen/sample (s/gen ret-spec) 100)]
;;                       (c/fn [& args] (c/let [c (string-to-0-99 (c/pr-str args))
;;                                              args-valid? (s/valid? args-spec (c/into [] args))]
;;                                    (if args-valid?
;;                                      (c/nth ret-samples c)
;;                                      (if (c/= name nil)
;;                                        (throw (AssertionError. (c/str "fct: args validation of (name unknown) "  (s/explain-str args-spec (c/into [] args)))))
;;                                        (throw (AssertionError. (c/str "fct: args validation of " name " " (s/explain-str args-spec (c/into [] args))))))))))})))

;; (c/defn fct-gen [args-test ret-spec name]
;;   (c/fn [] (s/gen #{(c/let [ret-samples (c/map (c/fn [x] (gen* ret-spec))
;;                                                (c/range 100))]
;;                       (c/fn [& args] (c/let [c (string-to-0-99 (c/pr-str args))
;;                                              args-valid? (c/apply args-test args)]
;;                                        (if args-valid?
;;                                          (c/nth ret-samples c)
;;                                          (if (c/= name nil)
;;                                            (throw (AssertionError. (c/str "fct: args validation of (name unknown) failed"  (c/meta args-valid?))))
;;                                            (throw (AssertionError. (c/str "fct: args validation of " name " failed " (c/meta args-valid?)))))))))})))


;; (def ^{:private true} fct-gen-new (c/fn [args-test ret-spec name]
;;                                     (c/fn [] (s/gen
;;                                               #{(gen* (let [ret-samples  (c/map (c/fn [x] (gen* ret-spec))
;;                                                                                 (c/range 100))]
;;                                                         (fn [& args] (let [c ((lift* string-to-0-99) ((lift* c/pr-str) args))
;;                                                                            args-valid? (apply args-test args)]
;;                                                                        (if-exception args-valid?
;;                                                                                      (nth ret-samples c)
;;                                                                                      (c/str "fct: args validation failed for: " name))))))}))))

(def some-fn (lift* (c/fn [ret-spec]
                      (c/let [ret-samples (c/map (c/fn [x] (ret-spec))
                                                 (c/range 100))]
                        (c/fn [& args] (c/let [c (string-to-0-99 (c/pr-str args))]
                                         (c/nth ret-samples c)))))))

;(def s (some-fn c/int?))


;; (def ^{:private true} any
;;   (s/with-gen
;;     (s/spec (c/fn [x] true))
;;     #(s/gen #{:any?})))

; fixing some problems? with function generation?
;; (c/defn fspec* [& sigs]
;;   (c/let [name (if (c/keyword? (c/first sigs)) (c/str (c/first sigs)) nil)
;;           sigs (if name (c/next sigs) sigs)
;;           [args ret] sigs]
;;     (s/with-gen
;;       any
;;       (gen ((lift* fct-gen) args ret name)))))
 
; fixing some problems? with function generation?

;; (c/defn fspec* [& sigs]
;;   (c/let [name (if (c/keyword? (c/first sigs)) (c/str (c/first sigs)) nil)
;;           sigs (if name (c/next sigs) sigs)
;;           [args ret] sigs]
;;     (s/with-gen
;;       any
;;       (fct-gen-new args ret name))))

;; (c/defn ^{:doc "spec creation with gen*, single argument use implies no validation"} spec*
;;   ([^{:doc "spec or fct object that can be generated"} arg]
;;    (s/with-gen
;;      any
;;      (c/fn [] (s/gen
;;                #{(gen* arg)}))))
;;   ([^{:doc "validation fct-function"} val
;;     ^{:doc "spec or fct object that can be generated"} arg]
;;    (s/with-gen
;;      (s/spec (gen* val))
;;      (c/fn [] (s/gen
;;                #{(gen* arg)})))))

;; rand side effect works
;; (def s (s/with-gen
;;          (s/spec (c/fn [x] true))
;;          #(s/gen #{(c/rand 1)})))

;; produces random integer
;; (def st (s/with-gen
;;           (s/spec (c/fn [x] true))
;;           #(s/gen #{(gen* (avar* c/int?))})))

;; construct* works
;; (def to-spec* (c/fn [a]
;;                 (construct* (c/fn [l] (s/with-gen
;;                                         (s/spec (c/fn [x] true))
;;                                         #(s/gen #{(ev* a l)}))))))

;; (c/defn ^{:doc "constructs an fct-object, interpretating to a spec under ev*, from an fct-object by taking the anonymous variables as generators"} free*
;;   [^{:doc "fct-object"} a]
;;   (construct* (c/fn [l] (s/with-gen (s/spec (c/fn [x] true))
;;                           #(s/gen #{(gen* (sub* a (dissoc-free-keys l)))})))
;;               :spec (:fct/spec (c/meta a))))


;; (c/defmacro ^{:doc ""} args [& ^{:doc "fct-object"} objects]
;;   `(free* (avar* (vector ~@objects))))

;; (defn ^{:doc ""} args [& ^{:doc "fct-object"} objects]
;;   (fn [] (apply vector objects)))

;; (def t (free* (avar*  c/int?)))
;; (def st (gen* t))

;; (c/defn prim [count-primitive]
;;   (c/into #{} (gen/sample (s/gen c/any?) count-primitive)))


;; different behaviour, explanation: c/cond is a macro
;; (gen*
;;  (cond true "All right"
;;        :else (throw (Exception. "asds"))))

;; (c/cond true "All right"
;;         :else (throw (Exception. "asds")))


;; different behaviour
;; (gen* (apply (fn testt [[x y]] (cond (and (int? x) (int? y)) true
;;                                     :else false)) (list [4 5] 6 7)))

;; (c/apply (c/fn testt [[x y]] (c/cond (c/and (c/int? x) (c/int? y)) true
;;                                      :else false)) (c/list [4 5] 6 7))

;; comes from fn working in the same way as let 
;; (c/let [[[x y]] (c/list [4 5] 6 7)] [x y])



