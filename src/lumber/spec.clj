
(ns lumber.spec
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

(defn rule-cons-literal [rule]
  (nth rule 2))

(example
 (rule-cons-literal [:lit 1 :leaf]) => :leaf)

(defn rule-cons-children [rule]
  (rest (rest rule)))

(example
 (rule-cons-children [:seq 1 [:lit :node] [:ref ::bin-tree] [:ref ::bin-tree]])
 => [[:lit :node] [:ref ::bin-tree] [:ref ::bin-tree]])

(defn rule-cons-arity [rule]
  (loop [children (rule-cons-children rule), count 0, arbitrary false]
    (if (seq children)
      (let [[kind val] (first children)]
        (case kind
          (:lit :ref)
          (recur (rest children) (inc count) arbitrary)
          :seq
          (recur (rest children) count true)
          (throw (ex-info "Bad rule component, unknown kind" {:kind kind :component (first children) :rule rule}))))
      (if arbitrary
        [:min count]
        [:exact count]))))

(example
 (rule-cons-arity [:seq 1 [:lit :node] [:ref ::bin-tree] [:ref ::bin-tree]])
 => [:exact 3])

(example
 (rule-cons-arity [:seq 1 [:lit :node] [:seq ::bin-tree] [:ref ::bin-tree]])
 => [:min 2])

(defn mkrule [r]
  (cond
    (rule-branch? r) {:kind :branch :rule r}
    (rule-cons? r) {:kind :cons :rule r}
    :else (throw (ex-info "Bad rule" {:rule r}))))

(defn recognizer [spec data]
  (loop [cont [(mkrule (get spec (:init spec)))
               data]]
    (if (seq cont)
      (let [[{kind :kind
              rule :rule
              alt :alt :or {alt '()}} data] (first cont)]
        (case kind
          :branch (recur (conj cont [{:kind :cons
                                      :rule (mkrule (first rule))
                                      :alt (rest rule)}
                                     data]))
          :cons (let [[ckind weight & children] rule]
                  (case ckind
                    :lit (cond
                           ;; literal check
                           (= (first children) data)
                           (recur (rest cont))
                           ;; literal is wrong, try alt if any
                           (seq alt)
                           (recur (conj cont [{:kind :cons
                                               :rule (mkrule (first alt))
                                               :alt (rest alt)}] data))
                           :else ;; no more alternative
                           [:ko {:msg "Missing literal" :literal (first children) :data data :rule rule}])
                    :seq (if-not (sequential? data)
                           (if (seq alt)
                             (recur (conj cont [{:kind :cons
                                                 :rule (mkrule (first alt))
                                                 :alt (rest alt)} data]))
                             [:ko {:msg "Not a sequence" :data data :rule rule}])
                           ;; sequential data
                           (recur (conj cont [{:kind :seq
                                               :rule children
                                               :alt alt} data])))))
          :seq )))))


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






