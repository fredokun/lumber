
(ns lumber.bottomup
  (:require [clojure.spec :as s]
            [clj-by.example :refer [example do-for-example]]))

(defonce +examples-enabled+ true)

(do-for-example
 (def +bin-trees+
   {:branches {::bin-tree [#{::leaf} #{::node}]}
    :nodes {::node [[:accept ::node-label] [:branch ::bin-tree] [:branch ::bin-tree]]}
    :accept {::leaf [:lit :leaf]
             ::node-label [:lit :node]}
    :init ::bin-tree}))

(defn safe-get
  [m k]
  (if-let [v (get m k)]
    v
    (throw (ex-info "No such key in collection" {:coll m :key k}))))

(defn expand-branch [spec state]
  (safe-get (:branches spec) state))

(declare recognize-bu-leaf)
(declare recognize-bu-seq)

(defn recognize-bottomup-rec
  ([spec tree] (let [[accept-states node-states] (expand-branch spec (:init spec))]
                 (recognize-bottomup-rec spec tree accept-states node-states)))
  ([spec tree accept-states node-states]
   (println "[recognize-bottomup-rec]" (str " states=[" accept-states ", " node-states "]"))
   (println "   ==> tree =" tree)
   (let [accept-states' (filter (fn [st]
                                  (recognize-bu-leaf spec st tree))
                                accept-states)]
     (if (seq accept-states')
       true ;; there is some accepting state so we're done
       ;; no accepting state, try to parse a node
       (if (and (sequential? tree)
                (seq tree)
                (seq node-states))
         (recognize-bu-seq spec tree 0 node-states)
         ;; not a non-leaf node, or no node-states to explore
         false)))))

(defn recognize-bu-leaf
  [spec accept-state tree]
  (let [[kind arg] (safe-get (:accept spec) accept-state)]
    (case kind
      :lit (= arg tree)
      (throw (ex-info "Unknown accept kind" {:accept-state accept-state :rule [kind arg] :kind kind})))))

(declare fetch-child-states)
(declare check-finish-node-state)

(defn recognize-bu-seq
  [spec tree pos node-states]
  (println "[recognize-bu-seq] pos=" pos "node-states=" node-states)
  (println "  ==> tree =" tree)
  (if (seq tree)
    (let [child-states-map (reduce (fn [acc node-state]
                                    (if-let [child-states (fetch-child-states spec node-state pos)]
                                      (assoc acc node-state child-states)
                                      acc)) {} node-states)]
      (println "  ==> child-states-map =" child-states-map)
      (let [next-node-states (filter (fn [node-state]
                                       (let [[child-accept-states child-node-states] (get child-states-map node-state)]
                                         (println "  ==> node-state=" node-state)
                                         (println "  ==> child-accept-states=" child-accept-states)
                                         (println "  ==> child-node-states=" child-node-states)
                                         (flush)
                                         (recognize-bottomup-rec spec (first tree) child-accept-states child-node-states))) node-states)]
        (println "[recognize-bu-seq] next-node-states =" next-node-states)
        (if (seq next-node-states)
          (recur spec (rest tree) (inc pos) next-node-states)
          false)))
    ;; no more element in tree
    (some (fn [st] (check-finish-node-state spec st pos)) node-states)))

(defn fetch-child-states
  [spec node-state pos]
  (let [node (get (:nodes spec) node-state)
        child (get node pos)]
    (if child
      (case (first child)
        :accept [#{(second child)} #{}]
        :branch (expand-branch spec (second child))
        (throw (ex-info "Unknown child spec kind" {:node-state node-state :child child :pos pos})))
      nil)))

(defn check-finish-node-state
  [spec node-state pos]
  (let [node (get (:nodes spec) node-state)]
    (= (count node) pos)))

(example
 (recognize-bottomup-rec +bin-trees+ :leaf) => true)

(example ;; wrong leaf
 (recognize-bottomup-rec +bin-trees+ :loaf) => false)

(example
 (recognize-bottomup-rec +bin-trees+ [:node :leaf :leaf])
 => true)

(example ;; too many children
 (recognize-bottomup-rec +bin-trees+ [:node :leaf :leaf :leaf])
 => false)

(example
 (recognize-bottomup-rec
  +bin-trees+
  [:node
   [:node :leaf [:node :leaf :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf]
    :leaf]]) => true)

(example ;; missing a branch
 (recognize-bottomup-rec
  +bin-trees+
  [:node
   [:node :leaf [:node :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf]
    :leaf]]) => false)

(example ;; too many branches
 (recognize-bottomup-rec
  +bin-trees+
  [:node
   [:node :leaf [:node :leaf :leaf]]
   [:node
    [:node [:node :leaf :leaf] :leaf :leaf]
    :leaf]]) => false)

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






