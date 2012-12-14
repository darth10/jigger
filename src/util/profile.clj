(ns util.profile)

(defn unit-of-memory 
  [value unit]
  "(unit-of-memory value unit) returns the value in relation to the defined unit in bytes.
Example: (unit-of-memory 10 :kilo-byte) will return 10240.
following Keywords are supported: 
:byte
:kilobyte
:megabyte
:terabyte"
  (* value (case unit
             :byte 1
             :kilobyte 1024
             :megabyte 1048576
             :terabyte 1073741824)))

(defn transform-memory 
  [value from to]
  "(transform-memory value from to) calculates the value from the unit 'from' to the unit 'to'"
  (/ (unit-of-memory value from)
     (unit-of-memory 1 to)))

(defn get-free-memory 
  ( []
    "(get-free-memory) returns the free memory of the used Java Virtual Machine"
    (.. Runtime (getRuntime) (freeMemory)))
  ([in]
    "(get-free-memory in) returns the free memory of the used Java Virtual Machine in the defined memory size"
    (transform-memory (get-free-memory) :byte in)))

(defn get-total-memory 
  ([]
    "(get-total-memory) returns the total memory of the used Java Virtual Machine"
    (.. Runtime (getRuntime) (totalMemory)))
  ([in]
    "(get-total-memory in) returns the total memory of the used Java Virtual Machine in the defined memory size"
    (transform-memory (get-total-memory) :byte in)))

(defn get-max-memory 
  ([]
    "(get-max-memory) returns the maximal memory of the used Java Virtual Machine"
    (.. Runtime (getRuntime) (maxMemory)))
  ([in]
    "(get-max-memory in) returns the maximal memory of the used Java Virtual Machine in the defined memory size"
    (transform-memory (get-max-memory) :byte in)))

(defn get-used-memory 
  ([]
    "(get-used-memory) returns the actual used memory of the used Java Virtual Machine"
    (- (get-total-memory) (get-free-memory)))
  ([in] 
    "(get-used-memory in) returns the actual used memory of the used Java Virtual Machine in the given memory size"
  (transform-memory (get-used-memory) :byte in)))
