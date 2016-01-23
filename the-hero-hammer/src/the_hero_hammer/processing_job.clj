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
(defmacro drop-out [] 2)

(defn max-available-range [curr-max my-range]
  (cond (<= (:to my-range) curr-max)
    {:from (:to my-range) :to curr-max}
    :else (:from (:to my-range) :to (:to my-range))))

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
   :record-key gen-key-for-filter-matchup-id
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

(defn new-filter-count [questions]
  (if (= nil questions)
    {:from 0 :to 0}
    (let [
          max-occourence
          (max (get-first-occourences-of-questions
                 questions))]
      {:from max-occourence :to max-occourence})))

(defn fetch-filter-new-or-empy
  "Create filter record if not exists."
  [hero-user-id hero-opponent-id filter-id]
  (let [curr (fetch-filter-count hero-user-id hero-opponent-id filter-id)]
    (if (= nil curr)
      (new-filter-count
        (get-filter-questions filter-id))
      curr)))

(defn lol-process-single-pair [to-process]
  to-process)

(defn lol-process-pairs [to-process]
  (into [] (map lol-process-single-pair to-process)))
