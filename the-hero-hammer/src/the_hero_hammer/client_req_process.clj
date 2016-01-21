(ns the-hero-hammer.client_req_process
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.storage :refer :all]))


(def ^:dynamic *global-questions-map-lol* (all-questions-lol))
(def ^:dynamic *global-short-to-index-map*
  (->> *global-questions-map-lol*
       (map-indexed #(vector (get %2 :shortname) %1))
       (into {})))

(def ^:dynamic *global-hero-to-index-lol*
  (into {} (map-indexed
           #(vector (clojure.string/lower-case %2) %1)
           (all-heroes-lol))))

(defn lol-hero-index [name]
  (get *global-hero-to-index-lol* name))

(defn lol-gen-key-for-count
  "Generate db key for question counter for question and filter."
  [hero-user hero-opponent question-id filter-id]
  (str "lol-question-count-" (lol-hero-index hero-user)
       "-" (lol-hero-index hero-opponent)
       "-" question-id "-" filter-id))

(defn lol-gen-key-for-matchup-question-count
  "Generate key for question count for specific matchup."
  [hero-user hero-opponent]
  (str "lol-question-index-count-"
       (lol-hero-index hero-user)
       "-" (lol-hero-index hero-opponent))
  )

(defmacro lol-question-count-key [] "lol-total-question-counter")

(defn lol-question-by-id-key [id]
  (str "lol-question-by-id-" id))

(defn lol-question-by-matchup-and-id-key
  [hero-user hero-opponent id]
  (str "lol-question-by-matchup-and-id-"
       (lol-hero-index hero-user)
       "-" (lol-hero-index hero-opponent)
       "-" id
       ))

(defn generic-store-next-item
  "Store next item (or create) for specified key (for counter)
  And the function which generates key from id."
  [count-key id-gen-function data]
  (let [curr (get-key count-key)]
    (let [toreturn
          (if (some? curr)
            (do (set-key count-key (inc curr))
                curr)
            (do (set-key count-key 1)
                0)
            )]
      (set-key (id-gen-function toreturn) data)
      toreturn)))

(defn lol-store-next-question-glob-index [data]
  "Increment and store in global counter of questions.
  We store keys to matchup questions."
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

(defn lol-store-next-question-for-matchup
  [hero-user hero-opponent data]
  (let [the-key (lol-gen-key-for-matchup-question-count
                 hero-user hero-opponent)
        curr (get-key the-key)]
    (let [toreturn (if (some? curr)
      (do (set-key the-key (inc curr))
          curr)
      (do (set-key the-key 1) 0))]
      (set-key (lol-question-by-matchup-and-id-key
                 hero-user hero-opponent toreturn)
               data)
      toreturn
      )))

(defn lol-get-question-for-matchup-by-id
  "Return question of matchup by number."
  [hero-user hero-opponent id]
  (get-key (lol-question-by-matchup-and-id-key
    hero-user hero-opponent id)))

(defn lol-get-question [id]
  (get-key (lol-question-by-id-key id)))

(defn lol-traverse-matchup-questions
  "Make iterator to traverse all question values (lazy)"
  [hero-user hero-opponent]
  (let [matchup-count (get-key (lol-gen-key-for-matchup-question-count
                       hero-user hero-opponent))]
    (if (some? matchup-count)
      (let [produce-for-id
        (fn [id]
           {:count id
            :until (dec matchup-count)
            :val (lol-get-question-for-matchup-by-id
                      hero-user hero-opponent id)})]
        (take-while some? (iterate (fn [val-map]
                 (if (= (:count val-map) (:until val-map))
                   nil
                   (produce-for-id (inc (:count val-map)))))
                 (produce-for-id 0))))
      nil)
    )
  )

; todo next
(defn store-question [arg-map]
  ())

