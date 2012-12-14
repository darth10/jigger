(ns fuzzylogic.fuzzyoperator)

;; AND-Operators
(defn minimum [ µa µb]
  "(minimum µa µb) calculates the minimum fuzzy operator"
  (min µa µb))

(defn algebraicProduct [ µa µb]
  "(algebraicProduct µa µb) calculates the algebraic product fuzzy operator"
  (* µa µb))

(defn einsteinProduct [ µa µb]
  "(einsteinProduct µa µb) calculates the Einstein-Product fuzzy operator"
  (/ 
    (* µa µb) 
    (- 2 
       (- (+ µa µb)
          (* µa µb)))))

(defn limitedDifference [ µa µb]
  "(limitedDifference µa µb) calculates the limited difference fuzzy operator"
  (max 0 (- (+ µa µb) 1)))

;; OR-Operators
(defn maximum [ µa µb]
  "(maximum µa µb) calculates the maximum fuzzy operator"
  (max µa µb))

(defn algebraicSum [ µa µb]
  "(algebraicSum µa µb) calculates the algebraic sum fuzzy operator"
  (- (+ µa µb)(* µa µb)))

(defn einsteinSum [ µa µb]
  "(einsteinSum µa µb) calculates the Einstein-Sum fuzzy operator"
  (/ (+ µa µb) 
     (+ 1 (* µa µb))))

(defn limitedSum [ µa µb]
  "(limitedSum µa µb) calculates the limited difference fuzzy operator"
  (min 1 (+ µa µb)))

;; Supported functions
(def supportedFuzzyOperators {:MinMax {:Tnorm minimum, :Snorm maximum}
                              :algebraic {:Tnorm algebraicProduct, :Snorm algebraicSum}
                              :einstein {:Tnorm einsteinProduct, :Snorm einsteinSum}
                              :limited {:Tnorm limitedDifference, :Snorm limitedSum}
                              })