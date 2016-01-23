(ns the-hero-hammer.processing_job
  (:require [the-hero-hammer.client_req_process :refer :all]
            [the-hero-hammer.storage :refer :all]
            [the-hero-hammer.core :refer :all]
            ))

(defn gen-key-for-filter-matchup-id
  [hero-user-id hero-opponent-id filter-id]
  (str "lol-filter-for-matchup-"
       hero-user-id "-" hero-opponent-id "-" filter-id))

(defn fetch-filter-count
  [hero-user-id hero-opponent-id filter-id]
  (get-key (gen-key-for-filter-matchup-id
             hero-user-id hero-opponent-id filter-id)))

(defmacro count-in [] 1)
(defmacro ignore [] 2)
(defmacro drop-out [] -1)

(defn max-available-range [curr-max my-range]
  (cond (<= (:to my-range) curr-max)
    {:from (:to my-range) :to curr-max}
    :else {:from (:to my-range) :to (:to my-range)}))

(defn in-range [number the-range]
  (if (and
        (> (:to the-range) number)
        (<= (:from the-range) number)) true nil))

(defn end-of-range [number the-range]
  (if (= (:to the-range) number) true nil))

(defn ahead-of-range [number the-range]
  (if (< (:to the-range) number) true nil))

(defn main-filter []
  {:id 0
   :expected max-available-range
   :process-question (fn [curr-id rec]
     (cond
       (in-range curr-id rec) (ignore)
       (ahead-of-range curr-id rec) (drop-out)
       (end-of-range curr-id rec) (count-in)))
   :required-questions ["poking"]
   })

(defn all-filters []
  [(main-filter)])

(defmacro lol-new-unprocessed-questions-key
  [idx]
  (str "lol-processed-questions-for-filter-" idx))

(defn lol-extract-pair-from-key
  [the-key]
  (into [] (rest
    (map parse-int
      (first (re-seq #"lol-question-by-matchup-and-id-(\d+)-(\d+)-\d+"
         the-key))))))

(defn lol-get-unproccessed-questions-range [idx]
  (let [outdb (get-key (lol-new-unprocessed-questions-key idx))
        globcnt (lol-global-question-count)]
    (cond
      (= nil globcnt) nil
      (= nil outdb) [0 globcnt]
      :else [(:to outdb) globcnt])))

(defn lol-reduce-range-to-units [question-range]
  (->> (range (get question-range 0) (get question-range 1))
       (map lol-get-question)
       (map lol-extract-pair-from-key)
       (distinct)
       (into [])))

(defn get-first-occourences-of-questions [questions]
  (->> questions
       (map #(get *lol-global-short-to-index-map* %))
       (map lol-gen-question-first-time-key)
       (map get-key)
       (into [])))

(defn get-filter-questions [filter-id]
  (->> (all-filters)
       (filter #(= (:id %) filter-id))
       (map #(:required-questions %))
       (first)))

(defn get-all-filters-for-matchup
  [hero-user-id hero-opponent-id]
  (->> (range (count (all-filters)))
       (map (partial
            fetch-filter-new-or-empy
            hero-user-id hero-opponent-id))
       (into [])))

(defn new-filter-count [questions]
  (if (= nil questions)
    {:from 0 :to 0}
    (let [max-occourence
          (first (max
            (get-first-occourences-of-questions
                 questions)))]
      {:from max-occourence :to max-occourence})))

(defn fetch-filter-new-or-empy
  "Create filter record if not exists."
  [hero-user-id hero-opponent-id filter-id]
  (let [curr (fetch-filter-count hero-user-id hero-opponent-id filter-id)]
    (if (= nil curr)
      (new-filter-count
        (get-filter-questions filter-id))
      curr)))

(defn filter-frequencies [filters-pending]
  (->> filters-pending
       (map #(get % 2))
       frequencies
       (sort-by #(get % 1) >)
       (into [])))

(defn zip-counts-with-filters [counts curr-newest-question]
  (->> (map vector counts (all-filters))
       (map #(vector (get %1 0) (get %1 1)
                     ((:expected (get %1 1))
                      curr-newest-question (get %1 0))))
       (into [])))

(defn get-matchup-questions-nodes
  [hero-user-id hero-opponent-id filters]
  (for [flt filters curr-q *global-questions-map-lol*]
    (let [count-key (lol-gen-key-for-count
                     hero-user-id hero-opponent-id
                     (:id curr-q) (:id flt))]
    ;[flt curr-q]
    count-key
      )
    ))

(defn process-n-questions
  [q-range the-filters hero-user-id hero-opponent-id]
  (let [questions (lol-get-n-questions-matchup-id
                    hero-user-id hero-opponent-id
                    (:from q-range)
                    (- (:to q-range) (:from q-range)))]
    (let [all-nodes
      ;(get-matchup-questions-nodes
        ;hero-user-id hero-opponent-id the-filters)
        1
      ]
      all-nodes)))

(defn process-according-to-frequences
  "Process according to frequences,
  first greatest, then the rest"
  [frequences the-filters to-process]
  (for [x frequences]
      (process-n-questions (get x 0) the-filters
        (get to-process 0) (get to-process 1))))

(defn lol-process-single-pair [currmax to-process]
  (let [the-filters (apply get-all-filters-for-matchup to-process)]
    (let [paired (zip-counts-with-filters the-filters currmax)]
      ; paired -> [ [ <count> <metadata> <expected range> ] .. ]
      (let [freqs (filter-frequencies paired)]
        (process-according-to-frequences freqs paired to-process)))))

(defn lol-process-pairs [to-process]
  (let [currmax (lol-global-question-count)]
    (into [] (map
       (partial lol-process-single-pair currmax)
       to-process))))
