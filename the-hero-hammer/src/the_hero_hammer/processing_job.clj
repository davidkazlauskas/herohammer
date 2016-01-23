(ns the-hero-hammer.processing_job
  (:require [the-hero-hammer.client_req_process :refer :all]
            [the-hero-hammer.storage :refer :all]
            ))

(defn gen-key-for-filter-matchup-id
  [hero-user-id hero-opponent-id filter-id]
  (str "lol-filter-for-matchup-"
       hero-user-id "-" hero-opponent-id "-" filter-id))

(defn main-filter []
  {:fetch-record []})

(defmacro lol-new-unprocessed-questions-key
  [idx]
  (str "lol-processed-questions-for-filter-" idx))

(defn lol-get-unproccessed-questions []
  (let [outdb] (get-key (lol-new-unprocessed-questions-key)))
)
