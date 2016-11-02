
(ns lumber.spec
  (:require [clojure.spec :as s]
            [clj-by.example :refer [example do-for-example]]))

(defonce +examples-enabled+ true)

(do-for-example
 (def *bin-trees*
   {::bin-tree #{::leaf ::node}
    ::leaf [:leaf 1]
    ::node [:node 1 ::bin-tree ::bin-tree]}))

(defn rule-branch? [b]
  (set? b))

(example
 (rule-branch? #{::leaf ::node}) => true)

(example
 (rule-branch? [:leaf 1]) => false)

(defn rule-cons? [b]
  (and (vector? b)
       (>= (count b) 2)))

(example
 (rule-cons? #{::leaf ::node}) => false)

(example
 (rule-cons? [:leaf 1]) => true)

(example
 (rule-cons? [:node 1 ::bin-tree ::bin-tree]) => true)

(defn rule-cons-info [rule]
  (first rule))

(example
 (rule-cons-info [:leaf 1]) => :leaf)

(example
 (rule-cons-info [:node 1 ::bin-tree ::bin-tree]) => :node)

(defn rule-cons-weight [rule]
  (second rule))

(example
 (rule-cons-weight [:leaf 1]) => 1)

(example
 (rule-cons-weight [:node 1 ::bin-tree ::bin-tree]) => 1)

(defn rule-cons-children [rule]
  (rest (rest rule)))

(example
 (rule-cons-children [:leaf 1]) => '())

(example
 (rule-cons-children [:node 1 ::bin-tree ::bin-tree])
 => [::bin-tree ::bin-tree])

(defn rule-component-ref? [c]
  (keyword? c))

(defn rule-component-seq? [c]
  (and (vector? c)
       (= (count c) 2)
       (= (first c) :seq)
       (keyword? (second c))))

(defn rule-cons-arity [rule]
  (loop [children (rule-cons-children rule), count 0, arbitrary false]
    (if (seq children)
      (cond
        (rule-component-ref? (first children))
        (recur (rest children) (inc count) arbitrary)
        (rule-component-seq? (first children))
        (recur (rest children) count true)
        :else (throw (ex-info "Bad rule component" {:component (first children) :rule rule})))
      (if arbitrary
        [:min count]
        [:exact count]))))

(example
 (rule-cons-arity [:leaf 1]) => [:exact 0])

(example
 (rule-cons-arity [:node 1 ::bin-tree ::bin-tree]) => [:exact 2])

(example
 (rule-cons-arity [:node 1 ::bin-tree [:seq ::bin-tree] ::bin-tree])
 => [:min 2])

(defn recognizer [spec cont]
  (if (seq cont)
    (let [[rulename data] (first cont)
          rule (get spec rulename)]
      ())))

(declare rec-leaf)
(declare rec-node)
(defn rec-bin-tree [t]
  (let [[ok? ret] (rec-leaf t)]
    (if ok?
      [true ret]
      (let [[ok? ret] (rec-node t)]
        (if ok?
          [true ret]
          [false {:input t :fails ::bin-tree}])))))

(defn rec-leaf [t]
  (if (= t :leaf)
    [true t]
    [false {:input t :fails ::leaf}]))

(defn rec-node [t]
  (if (not (sequential? t))
    [false {:input t :fails ::node :reason "not sequential"}]
    (if (not= (count t) 3)
      [false {:input t :fails ::node :reason "wrong arity"
              :expected-arity 3 :given-arity (count t)}]
      (let [[info sub1 sub2] t]
        (if (not= info :node)
          [false {:input t :fails ::node :reason "wrong content"
                  :expected-contents :node
                  :given-contents info}]
          )))))

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






