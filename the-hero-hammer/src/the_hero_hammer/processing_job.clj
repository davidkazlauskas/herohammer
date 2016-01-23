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
   :required-questions ["poke"]
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
