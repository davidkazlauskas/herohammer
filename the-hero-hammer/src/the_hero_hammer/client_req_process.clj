(ns the-hero-hammer.client_req_process
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.storage :refer :all]))


(def ^:dynamic *global-questions-map-lol* (all-questions-lol))
(def ^:dynamic *global-short-to-index-map*
  (->> *global-questions-map-lol*
       (map-indexed #(vector (get %2 :shortname) %1))
       (into {})))

(defn store-question [arg-map]
  ())

