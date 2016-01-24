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

(defn all-filters []
  [(main-filter)])

(def ^:dynamic *hh-context*
  {
   :dbinfo {
     :get-key the-hero-hammer.storage/get-key
     :set-key the-hero-hammer.storage/set-key
     :set-if-not-exists the-hero-hammer.storage/set-if-not-exists
   }
   :filters (all-filters)
  })

