(ns fuzzylogic.fuzzydbstore
  (:gen-class)
  (:use [fuzzylogic.fuzzyfunction])
  (:use [fuzzylogic.fuzzyoperator])
  (:use [fuzzylogic.fuzzycalculation])
  (:use [fuzzylogic.deffuzification])
  (:require [monger core collection util])
  (:import (com.mongodb Mongo DB DBObject))
  (:use [clojure.tools.cli]))


(defn buildFuzzyFunction 
  [databaseFuzzyFunction]
  "(buildFuzzyFunction databasefuzzyFunction) returns a concrete fuzzy function on the basis of the passed FuzzyFunction"
  (apply 
    ((keyword (:function databaseFuzzyFunction)) supportedFuzzyFunctions) 
    (:parameters databaseFuzzyFunction)))