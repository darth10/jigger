(ns fuzzylogic.fuzzycalculation
  (:require [fuzzylogic.fuzzyfunction :as fuzzyfunction])
  (:require [fuzzylogic.fuzzyoperator :as fuzzop]))


(defrecord FuzzyRule [term fuzzysets tnorm])

(defn calcFuzzyRule
  [input sets fuzzOp]
  {:pre [(map? input) (map? sets)]}
  "(calFuzzyRule input sets fuzzOp) calculates the affialiation value for the input variables with the passed fuzzy sets with the defined fuzzy operator."
  (reduce 
    fuzzOp
    (for[attribut (keys input)]
      ( (get sets attribut)
        (get input attribut)))
    ))

(defn calcFuzzyRules
  [input rules]
  "(calcFuzzyRules input rules) calculates the membership value for the passed rules with the specified input and returns a lazy collection wich contains the rule term and the membershipvalue."
  (map 
    #(let [memberval (calcFuzzyRule input (:fuzzyfunctions %1) (:tnorm %1))
           implication (:term %1)] 
       {:memberval memberval, :implication implication}) 
    rules ))


;; Definitions for fast tests.
(def variables {"v1" 3, 
                "v2" 1, 
                "v3" 2})

(def fuzzysets_1 {"v1" (fuzzyfunction/getDelta 2 3 4), 
               "v2" (fuzzyfunction/getDelta 1/6 2 3/2),
               "v3" (fuzzyfunction/getDelta 1 3 6)})

(def fuzzysets_2 {"v1" (fuzzyfunction/getTrapezoid 2 3 4 5), 
               "v2" (fuzzyfunction/getDelta 1/6 2 3/2),
               "v3" (fuzzyfunction/getTrapezoid 1 3 6 10)})

(def rule1 (FuzzyRule. 
             {:term "rule1", :function (fuzzyfunction/getDelta 3 5 8)} 
             fuzzysets_1 fuzzop/minimum ))
(def rule2 (FuzzyRule. 
             {:term "rule2", :function (fuzzyfunction/getTrapezoid 0 3 5 10)} 
             fuzzysets_2 fuzzop/einsteinProduct ))

(def fuzzOperator fuzzop/minimum)
