(ns fuzzylogic.fuzzyfunction
  (:require [clojure.math.numeric-tower :as math]))

;; Fuzzy Set functions
(defn deltaFunction[x a b c]
  "(deltafunction x a b c) calculates the affiliation value for 'x' in the triangle defined by 'a', 'b' and 'c'."
  (cond
    (and (<= a x) (< x b)) (/ (- x a) (- b a))
    (and (<= b x) (< x c)) (/ (- c x) (- c b))
    :else 0
    ))

(defn trapezoidFunction[x a b c d]
  "(trapezoidfunction x a b c d) calculates the affiliation value for 'x' in the trapezoid area defined by 'a', 'b', 'c' and 'd'."
  (cond
    (and (<= a x) (< x b)) (/ (- x a) (- b a))
    (and (<= b x) (< x c)) 1
    (and (<= c x) (< x d)) (/ (- d x) (- d c))
    :else 0
    ))

(defn gaussianFunction[x a m]
  "(gaussianfunction x a m) calculates the affiliation value for 'x' in the gaussian are defined by 'a' and 'm'"
  (math/expt Math/E (* -1 a (math/expt (- x m) 2))))


;; Fuzzy Function Closures

(defn getDelta
  [a b c]
  "(getDelta a b c) returns a new affiliation function for the delta function with the parameters 'a', 'b' and 'c'."
  (fn [x] (deltaFunction x a b c)))

(defn getTrapezoid
  [a b c d]
  "(getDelta a b c d) returns a new affiliation function for the trapezoid function with the parameters 'a', 'b', 'c' and 'd'."
  (fn [x] (trapezoidFunction x a b c d)))

(defn getGauss
  [a m]
  {:pre [(> a 0)]}
  "(getDelta a b m) returns a new affiliation function for the gaussian function with the parameters 'a' and 'm'."
  (fn [x] (gaussianFunction x a m)))

(def supportedFuzzyFunctions {:delta getDelta, 
                              :trap getTrapezoid, 
                              :gauss getGauss})

(defrecord FuzzyFunction [variable term function parameters])

