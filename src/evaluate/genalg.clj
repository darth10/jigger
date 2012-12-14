(ns evaluate.genalg
  (:use [evolution.genalg])
  (:use [clojure.tools.cli])
  (:use [util.profile])
  (:use [incanter core stats charts]))
  
(def creators [7])
(def filename-string "stats/generic-algorithm-stat_{0}_{1}.stat")
(def system-information {:os-name (. System getProperty "os.name")
                         :os-version (. System getProperty "os.version")
                         :os-arch (. System getProperty "os.arch")
                         :java-version (. System getProperty "java.version")
                         })

;; To use until monger can store also clojure rationals
(defn- create-filename [addition]
  "(create-filename addition) creates a new filename to store the tracking stats and adds the addition to it."
  (. (. filename-string replace "{0}" (str addition))
  replace "{1}" (:os-arch system-information)))

(defn- store-stats-in-file [stats filename]
    (spit filename
          (with-out-str (prn stats))))

(defn- load-stats-from-file [filename]
  (read-string (slurp filename)))

(defn load-stat 
  "(load-stat creatos) loads the file which was dtored for the given number of creators and for the actual archticture"
  [creators]
  (load-stats-from-file (create-filename creators)))
  
(defn retrieve-new-stats
  [number-of-creators time timetype]
  "(retrieve-new-stats number-of-creators time timetype) starts a new genetic algorithm run with 'number-of-creators' creator agents for the a time period given by 'time' and 'timetype'."
  (prepare-evolution number-of-creators) 
  (start-evolution) 
  (track-evolution time timetype))

(defn create-stats-for-creators [creators time timetype]
  "(create-stats-for-creators creators time tyimetype) evalutes the generic algorithm for the specified number of creators for the given time and stores the stats into a file."
  (let [stats (retrieve-new-stats creators time timetype)
        filename (create-filename creators)
        ]
  (store-stats-in-file stats filename)))

;; Plot functions

(defn- plot-agent
  [agent-log agent-id key]
  (let [log-info (get agent-log agent-id)
        extracted-keys (sort (keys log-info))
        y (map #(get (get log-info %1) key) extracted-keys)
        x (for [x extracted-keys] (- x (first extracted-keys)))]
    (xy-plot x y)))


(defn empty-xy 
  "This function is here to work around the broken legend of xy-plots in Incanter"
  []
  (set-theme-default (org.jfree.chart.ChartFactory/createXYLineChart "" "x" "y" (org.jfree.data.xy.XYSeriesCollection.) org.jfree.chart.plot.PlotOrientation/VERTICAL true false false)))

(defn show-memory-useage-for [creators]
  (let [test-stat (load-stat creators)
        memory (:used-memory test-stat)
        x (sort (keys memory))
        y (map #(get memory %1) x)]
    (view (xy-plot x y 
               :title "Used Memory" 
               :x-label "Runtime in ns" 
               :y-label "Memory in MB" 
               :legend true))))

(defn show-population-courve [creators]
  (let [test-stat (load-stat creators)
        values (:population-courve test-stat)
        x (sort (keys values))
        y (map #(get values %1) x)]
    (view (xy-plot x y 
               :title "Population" 
               :x-label "Runtime in ns" 
               :y-label "Population" 
               :legend true))))

(defn show-grimreaper-time [creators]
  (let [test-stat (load-stat creators)
        grimreaper-log (:grimreaper-log test-stat)]
    (view (plot-agent grimreaper-log (first (keys grimreaper-log)) :time))))


(defn show-one-kill-numbers [creators]
  (let [test-stat (load-stat creators)
        kill-number (count (val (first (:grimreaper-log test-stat))))]
      {creators kill-number}))


(defn show-one-creator-numbers [creators]
  (let [counted (loop [log (:creator-log (load-stat creators)) number 0]
                  (if (empty? log) number
                    (let [creation-number (count (val (first log)))]
                      (recur (rest log) (+ number creation-number))
                      )))]
    {creators counted}))

(defn show-many-creation-numbers [& creators]
  (let [creation-map (reduce conj (for [x creators] (show-one-creator-numbers x)))]
    (view (bar-chart (keys creation-map) (vals creation-map) 
                     :x-label "Number of Creator Agents"
                     :Y-label "Number of created individuals"
                     :title "Creator activity"))))


(defn show-many-kill-numbers [& creators]
  (let [kill-map (reduce conj (for [x creators] (show-one-kill-numbers x)))]
    (view (bar-chart (keys kill-map) (vals kill-map) 
                     :x-label "Number of Creator Agents"
                     :Y-label "Number of killed individuals"
                     :title "Grimreaper activity"))))
