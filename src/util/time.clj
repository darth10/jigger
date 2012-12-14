(ns util.time)

(defn unit-of-time 
  [value unit]
  "(unit-of-time value unit) returns the value in relation to the defined unit in seconds.
Example: (unit-of-time 10 :minute) will return 600 and (unit-of-time 3 :millisecond) 3/1000.
following Keywords are supported: 
:second
:minute
:houre
:day
:millisecond
:microsecond
:nanosecond"
  (* value (case unit
             :second 1
             :minute 60
             :houre 3600
             :day 86400
             :millisecond 1/1000
             :microsecond 1/1000000
             :nanosecond 1/1000000000)))

(defn transform-time 
  [value from to]
  "(transform-time value from to) calculates the value from the unit 'from' to the unit 'to'"
  (/ (unit-of-time value from)
     (unit-of-time 1 to)))
