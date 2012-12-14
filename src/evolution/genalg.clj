(ns evolution.genalg
  (:use [util.time])
  (:use [util.profile]))

;; Implementation of a genetic algorithm based on the code given
;; in the book "Clojure" by Stefan Kamphausen and Tim Oliver Kaiser.

(def population (atom{}))

(def the_target "Thursday Next")

(def gen-max-length 20)

(def allowed-symbols (str "abcdefghijklmnopqrstuvwxyzäöü "
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ "))

(def population-size 1000)

(defn fitness-calculation [indiv]
  "(fitness-calculation indiv) calculates the fitness of the actual
individual"
  (+ (* 2 (Math/abs (- (count the_target) (count indiv))))
     (reduce + (map #(if (= %1 %2) 0 1)
                    indiv the_target))))

(defn random-symbol []
  "(random-symbol) returns a randomly selected allowed symbol."
  (nth allowed-symbols (rand-int (count allowed-symbols))))

(defn random-genom [size]
  "(random-genom size) returns a randomly designed genom with the
defined 'size'"
  (apply str
         (take (inc (rand-int size))
               (repeatedly random-symbol))))

;; Genom selection

(defn genom-selection [g1 g2]
  "(genom-selection g1 g2) selects a randomly chosen symbol from one
of the passed genoms. There is also a 10% chance that this symbol will
be mutated."
  (if (= 0 (rand-int 10))
    (random-symbol)
    (if (= 0 (rand-int 2)) g1 g2)))

(defn size-selection [i1 i2]
  "(size-selection i1 i2) returns the length of a genom based on the
passed individuals."
  (let [c1 (count i1)
        c2 (count i2)]
    (if (= 0 (rand-int 2))
      (if (= 0 (rand-int 2))
        c1
        c2)
      (max gen-max-length
           (+ (min c1 c2)
              (rand-int (Math/abs (- c1 c2))))))))

(defn genom-recombination [i1 i2]
  "(genom-recombination i1 i2) combines the genoms if individual i1
and individual i2."
  (apply str
         (take (size-selection i1 i2)
               (map genom-selection (cycle i1)
                    (cycle i2)))))

;; Population dynamic
(defn random-population [size indiv-length]
  "(random-population size indiv-length) crates a new population with
the defined size and where every individual has the defined length."
  (into {}
        (map (fn [x] {x (fitness-calculation x)})
             (take size
                   (repeatedly
                     #(random-genom indiv-length))))))

(defn any-individual [population]
  "(any-individual population) returns a random chosen individual of
the defined population"
  (let [ks (keys population)]
    (rand-nth ks)))

(defn select-individual [pop selection priority]
  "(select-individual pop selection priority) selects 'selection'
times individuals of the passed population."
  (let [cmp-fun (if (= priority :good) < >)]
    (first
      (sort #(cmp-fun (pop %1) (pop %2))
            (take selection
                  (repeatedly
                    #(any-individual pop)))))))

;; Control structure with agents

(def ^{:doc "Agent, who runs the system"}
  start-stop-agent (agent "stop"))

(def ^{:doc "Domiciles for agents which creates new individuals are
crated. Every agent gets one domicile."}
      domiciles)

(def ^{:doc "The number of domiciles must be tuned for the Grimreaper,
to ensure a stable population."}
      number-of-domiciles 2)

(def ^{:doc "Agent whos purpose is to reduce the population and hold it stable."}
      grimreaper (agent {}))

(def ^{:doc "Atom who keeps track of the best individuals in a time spot."}
      list-of-the-best (atom {}))

(def ^{:doc "Atom who keeps track of the population in a time spot."}
      revisor-population (atom {}))

(def ^{:doc "Atom who keeps track of the used memory."}
      revisor-memory (atom {}))

(defn create-domiciles 
  ([]
  "(create-domiciles) creates the standart number of new domiciles for the
agents."
  (doall
    (for[i (range number-of-domiciles)]
      (agent {}))))
([number]
  "(create-domiciles number) creates a 'number' of new domiciles for the
agents.)"
  (doall
    (for[i (range number)]
      (agent {})))))

(defn a-day-in-the-life []
  "(a-day-in-the-life) is the central function for steps in the
evolution."
  (let [o1 (select-individual @population 3 :good)
        o2 (select-individual @population 3 :good)
        new (genom-recombination o1 o2)
        fit (fitness-calculation new)]
    (swap! population conj {new fit})))

(defn a-day-in-the-life-agent-fn [status]
  "(a-day-in-the-life-agent-fn status) starts a new agent."
  (when (= @start-stop-agent "running")
    (send *agent* a-day-in-the-life-agent-fn))
  (let [start (. System (nanoTime))
        result (a-day-in-the-life)
        end (. System (nanoTime))
        error (agent-error *agent*)
        agent-id (. (str *agent*) replace "clojure.lang.Agent@" "")
        runtime (- end start)]
    
    (conj status {start {:time runtime
                       :error error}})))

(defn breath-of-death[]
  "(breath-of-death) removes bad individuals from the population"
  (when (> (count @population) population-size)
    (let [gen (select-individual @population 2 :bad)]
      (swap! population dissoc gen))))

(defn breath-of-death-agent-fn [status]
  (when (= @start-stop-agent "running")
    (send *agent* breath-of-death-agent-fn))
  (let [start (. System (nanoTime))
        result (breath-of-death)
        end (. System (nanoTime))
        error (agent-error *agent*)
        agent-id (. (str *agent*) replace "clojure.lang.Agent@" "")
        runtime (- end start)
        ]
    (conj status {start {:time runtime
                       :error error}})))

(defn create-start-population []
  "(create-start-population) creates a new start population."
  (reset!
    population
    (random-population population-size gen-max-length)))

(defn prepare-evolution
  ([]
    "(prepare-evolution) prepares the environment with the standart parameters."
    (create-start-population)
    (send grimreaper (constantly {}))
    (def domiciles (create-domiciles)))
  ([number-of-domiciles]
    "(prepare-evolution number-of-domicilies) prepares the environment with the passed parameters."
    (create-start-population)
    (send grimreaper (constantly {}))
    (def domiciles (create-domiciles number-of-domiciles)))
  )

(defn start-evolution []
   (def start-time (. System (nanoTime)))
    (send start-stop-agent (constantly "running"))
    (await start-stop-agent)
    (send grimreaper breath-of-death-agent-fn)
    (map #(send % a-day-in-the-life-agent-fn) domiciles))

(defn stop-evolution []
  (send start-stop-agent (constantly "stopped")))

(defn best-individuals
  ([number]
  (let [p @population]
    (take number (sort #(< (p (first %1))
                           (p (first %2)))
                       p))))
  ([] (best-individuals 1)))

(defn print-agent [agent]
  (let [agent-id (. (str agent) replace "clojure.lang.Agent@" "")
        agent-state (deref agent)]
     {agent-id agent-state}))

(defn track-evolution
 ( []
  (while (= @start-stop-agent "running")
    ( let [best (best-individuals 2)
           number (count @population)
           runtime (- (. System (nanoTime)) start-time)]
      (printf "Population: %d" number)
      (println " - Domiciles: " (map deref domiciles))
      (println " - runtime-second: " (transform-time runtime :nanosecond :second))
      (println best)
      (when (= 0 (second (first best)))
        (stop-evolution))
      (Thread/sleep 2000)
      )))
 ( [time timetype]
  (while (= @start-stop-agent "running")
    ( let [maximal-time (transform-time time timetype :nanosecond)
           best (best-individuals 2)
           number (count @population)
           runtime (- (. System (nanoTime)) start-time)
           key runtime
           memory (get-used-memory :megabyte)]
      (swap! list-of-the-best conj {key best})
      (swap! revisor-population conj {key number})
      (swap! revisor-memory conj {key memory})
      (when (or
              (= 0 (second (first best)))
              (> runtime maximal-time))
        (stop-evolution))
      (Thread/sleep 10)))
  
  (let [pop-count (count @population)
        creator-count (count domiciles)
        best-list (deref list-of-the-best)
        used-memory (deref revisor-memory)
        population-courve (deref revisor-population)
        creator-log (reduce conj (map print-agent domiciles))
        grimreaper-log (print-agent grimreaper)
        ]
    {:start-time start-time
   :end-population pop-count
   :creators-count creator-count
   :bestlist best-list 
   :used-memory used-memory
   :population-courve population-courve 
   :creator-log creator-log
   :grimreaper-log grimreaper-log
   :maximal-time time
   :run-time-nano (- (. System (nanoTime)) start-time)
   })
  ))
