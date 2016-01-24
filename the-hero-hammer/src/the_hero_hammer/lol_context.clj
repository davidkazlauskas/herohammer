(ns the-hero-hammer.lol_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [taoensso.nippy :as nippy]))

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

(defn lol-filters []
  [(main-filter)])

(defn lol-generate-global-question-key [the-id]
  ["lol" "glob-question" the-id])

(defn lol-generate-global-question-count []
  ["lol" "glob-question-count" "count"])

(defn lol-generate-question-first-time-key [the-id]
  ["lol" "question-first-time" the-id])

(def ^:dynamic *hh-context-lol*
  {
   :dbinfo {
     :get-key the-hero-hammer.storage/get-key
     :set-key the-hero-hammer.storage/set-key
     :set-if-not-exists the-hero-hammer.storage/set-if-not-exists
   }
   :filters (lol-filters)
   :queries {
     :glob-question-count lol-generate-global-question-count
     :glob-question-id lol-generate-global-question-key
     :question-first-time lol-generate-question-first-time-key
   }
  })
