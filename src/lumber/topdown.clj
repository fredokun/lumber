
(ns lumber.topdown
  (:require [clojure.spec :as s]
            [clj-by.example :refer [example do-for-example]]))

(defonce +examples-enabled+ true)

(do-for-example
 (def *bin-trees*
   {::bin-tree #{::leaf ::node}
    ::leaf [:lit 1 :leaf]
    ::node [:seq 1 [:lit :node] [:ref ::bin-tree] [:ref ::bin-tree]]
    :init ::bin-tree}))

(defn rule-branch? [b]
  (set? b))

(example
 (rule-branch? #{::leaf ::node}) => true)

(example
 (rule-branch? [:leaf 1]) => false)

(defn rule-cons-kind? [t]
  (contains? #{:lit :seq} t))

(defn rule-cons? [b]
  (and (vector? b)
       (rule-cons-kind? (first b))
       (>= (count b) 3)))

(example
 (rule-cons? #{::leaf ::node}) => false)

(example
 (rule-cons? [:lit 1 :leaf]) => true)

(example
 (rule-cons? [:seq 1 [:lit :node] [:ref ::bin-tree] [:ref ::bin-tree]]) => true)

(defn safe-get
  [m k]
  (if-let [v (get m k)]
    v
    (throw (ex-info "No such key in collection" {:coll m :key k}))))

(declare recognize-td-literal)
(declare recognize-td-seq)

(defn recognize-topdown-rec
  "Recognizes the tree `t` for specification `spec`.
This uses a naive top-down algorithm implemented in a non-tail
recursive way. Hence its simplicity against speed:
  worst-case time complexity is exponential."
  ([spec tree] (recognize-topdown-rec spec tree (:init spec)))
  ([spec tree state]
   (let [rule (safe-get spec state)]
     (cond
       (rule-branch? rule)
       (some (fn [st]
               (recognize-topdown-rec spec tree st)) rule)
       (rule-cons? rule)
       (let [[kind _ & contents] rule]
         (case kind
           :lit (recognize-td-literal tree (first contents))
           :seq (and (sequential? tree)
                     (recognize-td-seq spec tree contents))
           (throw (ex-info "Unknown cons rule kind" {:rule rule :kind kind}))))
       :else (throw (ex-info "Wrong rule" {:name state :rule rule}))))))

(defn recognize-td-literal
  [tree litt]
  (= tree litt))

(defn recognize-td-seq
  [spec tree components]
  (if (seq components)
    (let [[kind arg] (first components)]
      (case kind
        :lit (and (seq tree)
                  (recognize-td-literal (first tree) arg)
                  (recognize-td-seq spec (rest tree) (rest components)))
        :ref (and (seq tree)
                  (recognize-topdown-rec spec (first tree) arg)
                  (recognize-td-seq spec (rest tree) (rest components)))
        (throw (ex-info "Unsupported cons kind" {:kind kind :component (first components) :tree tree}))))
    ;; no more component
    (not (seq tree))))

(example
 (recognize-topdown-rec *bin-trees* :leaf) => true)

(example ;; wrong leaf
 (recognize-topdown-rec *bin-trees* :loaf) => nil)

(example
 (recognize-topdown-rec
  *bin-trees*
  [:node
   [:node :leaf [:node :leaf :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf]
    :leaf]]) => true)

(example ;; missing a branch
 (recognize-topdown-rec
  *bin-trees*
  [:node
   [:node :leaf [:node :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf]
    :leaf]]) => nil)

(example ;; too many branches
 (recognize-topdown-rec
  *bin-trees*
  [:node
   [:node :leaf [:node :leaf :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf :leaf]
    :leaf]]) => nil)


(do-for-example
 (def *nary-trees*
   {::nary-tree #{::leaf ::node}
    ::leaf [:leaf 1]
    ::node [:node 1 [:seq ::nary-tree]] }))

(do-for-example
 (def *unary-binary*
   {::ub-tree #{::leaf ::unary ::binary}
    ::leaf [:leaf 1]
    ::unary [:unary 1 ::ub-tree]
    ::binary [:binary 1 ::ub-tree ::ub-tree]}))

(do-for-example
 (def *series-parallel*
   {::sp #{::serie ::parallel}
    ::serie #{::leaf ::snode}
    ::snode [:serie 1 ::parallel ::parallel [:seq ::parallel]]
    ::parallel #{::leaf ::pnode}
    ::pnode [:parallel 1 ::serie ::serie [:seq ::serie]]
    ::leaf [:leaf 1]}))

(do-for-example
 (def *seq*
   {::seq [:seq 1 [:seq ::seq]]}))






