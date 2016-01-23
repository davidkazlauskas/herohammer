(ns the-hero-hammer.processing_job
  (:require [the-hero-hammer.client_req_process :refer :all]
            [the-hero-hammer.storage :refer :all]
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

(defn main-filter []
  {:id 0
   :record-key gen-key-for-filter-matchup-id
   :expected (fn [curr-max record] "")
   :process-question [curr-id record]
   })

(defmacro lol-new-unprocessed-questions-key
  [idx]
  (str "lol-processed-questions-for-filter-" idx))

(defn lol-get-unproccessed-questions []
  (let [outdb] (get-key (lol-new-unprocessed-questions-key)))
)
