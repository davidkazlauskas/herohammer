(ns the-hero-hammer.client_req_process
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.storage :refer :all]))


(def ^:dynamic *global-questions-map-lol* (all-questions-lol))
(def ^:dynamic *global-short-to-index-map*
  (->> *global-questions-map-lol*
       (map-indexed #(vector (get %2 :shortname) %1))
       (into {})))

(defn gen-key-for-count-lol
  [hero-user hero-opponent question-id filter-id]
  (str "lol-question-count-" hero-user
       "-" hero-opponent "-" question-id "-" filter-id))

(defmacro lol-question-count-key [] "lol-total-question-counter")

(defn lol-question-by-id-key [id]
  (str "lol-question-by-id-" id))

(defn lol-store-next-question [data]
  (let [curr (get-key (lol-question-count-key))]
    (let [toreturn
          (if (some? curr)
          (do (set-key (lol-question-count-key) (inc curr))
              curr)
          (do (set-key (lol-question-count-key) 1)
              0))]
      (set-key (lol-question-by-id-key toreturn) data)
      toreturn)
    )
  )

(defn lol-get-question [id]
  (get-key (lol-question-by-id-key id)))

; todo next
(defn store-question [arg-map]
  ())

