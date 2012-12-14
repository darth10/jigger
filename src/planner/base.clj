(ns planner.base
  (:use [clojure.set]))

(defrecord Operation [action preconds add-list del-list])
(defrecord Environment [operations state performed-actions])

(defn create-new-operation [action preconds add-list del-list]
  (Operation. action preconds add-list del-list))

(defn create-new-atom-environment [ ]
  "(create-new-atom-environment operations state) creates a new environment, where 'operations', 'state' and 'performed-actions' are stored into atoms."
  (Environment. 
    (atom {}) 
    (atom #{}) 
    (atom {})))

(defn add-operation! [^Operation op ^Environment en]
  "(add-operation! op en) adds the passed operation to the passed environment."
  (if (not (contains? (:operations en) (:action op))) (swap! (:operations en) conj op)))

(defn check-conditions [cond ^Environment en]
  {:pre [(coll? cond)]}
  "(check-conditions cond en) verifies whether the passed sequence of conditions 'cond' are fulfilled in the state of the passed environment.
This method modify the passed environment!"
  (subset? cond (deref (:state en) )))
  
(defn add-condition! [cond ^Environment en]
  "(add-condition! cond en) adds the passed condition 'cond' to the conditions of the passed environment.
This method modify the passed environment!"
  (swap! (:state en) conj cond))

(defn remove-condition! [cond ^Environment en]
  "(remove-condition! cond en) removes the passed condition 'cond' from the conditions of the passed environment.
This method modify the passed environment!"
  (swap! (:state en) disj cond))

(defn find-all-possible-operations [^Environment en]
  "(find-all-possible-operations en) returns all operations, which can be performed in the passed environment."
  (filter #(check-conditions (:preconds %1) en) (:operations en)))

(defn remove-conditions! [conditions en]
   {:pre [(coll? conditions)]}
  "(remove-conditions! conditions en) removes the given conditions from the passed environment 'en'.
This method modify the passed environment!"
  (doseq [new-element conditions] (remove-condition! new-element en)))

(defn add-conditions! [conditions en]
   {:pre [(coll? conditions)]}
  "(add-conditions! conditions en) adds the given conditions from the passed environment 'en'.
This method modify the passed environment!"
  (doseq [new-element conditions] (add-condition! new-element en)))

(defn perform-operation! [^Operation op ^Environment en]
  "(perform-operation op en) performs the specified operation, which changes the state of the passed environment.
This method modify the passed environment!"
  (remove-conditions (:del-list op) en)
  (add-conditions (:add-list) en)
  {:action op, :en en})