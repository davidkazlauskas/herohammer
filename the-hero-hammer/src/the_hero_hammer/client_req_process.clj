(ns the-hero-hammer.client_req_process
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.questions_spec :refer :all]
            [the-hero-hammer.storage :refer :all]
            [taoensso.nippy :as nippy]))


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

(defn lol-gen-key-for-matchup-question-count-id
  "Generate key for question count for specific matchup."
  [hero-user hero-opponent]
  (str "lol-question-index-count-"
       hero-user "-" hero-opponent)
  )

(defn lol-gen-key-for-matchup-comment-count
  "Generate key for comment count for specific matchup."
  [hero-user hero-opponent]
  (str "lol-question-comment-count-"
       (lol-hero-index hero-user)
       "-" (lol-hero-index hero-opponent))
  )

(defn lol-gen-key-for-matchup-comment
  "Generate key to store specific comment."
  [hero-user hero-opponent id]
  (str "lol-question-comment-id-"
       (lol-hero-index hero-user)
       "-" (lol-hero-index hero-opponent)
       "-" id)
  )

(defn lol-gen-key-for-matchup-comment-id
  "Generate key to store specific comment."
  [hero-user hero-opponent id]
  (str "lol-question-comment-id-"
       hero-user "-" hero-opponent "-" id)
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

(defn lol-question-by-matchup-and-id-key-id
  [hero-user hero-opponent id]
  (str "lol-question-by-matchup-and-id-"
       hero-user "-" hero-opponent "-" id
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
  (generic-store-next-item
    (lol-question-count-key)
     lol-question-by-id-key
     data))

(defn lol-store-next-question-for-matchup
  [hero-user hero-opponent data]
  (generic-store-next-item
    (lol-gen-key-for-matchup-question-count
      hero-user hero-opponent)
    (partial lol-question-by-matchup-and-id-key
             hero-user hero-opponent)
    data))

(defn lol-store-next-question-for-matchup-by-id
  [hero-user hero-opponent data]
  (generic-store-next-item
    (lol-gen-key-for-matchup-question-count-id
      hero-user hero-opponent)
    (partial lol-question-by-matchup-and-id-key-id
             hero-user hero-opponent)
    data))

(defn lol-store-next-comment-for-matchup
  [hero-user hero-opponent data]
  (generic-store-next-item
    (lol-gen-key-for-matchup-comment-count
      hero-user hero-opponent)
    (partial lol-gen-key-for-matchup-comment
             hero-user hero-opponent)
    data)
  )

(defn lol-store-next-comment-for-matchup-id
  [hero-user hero-opponent data]
  (generic-store-next-item
    (lol-gen-key-for-matchup-comment-count
      hero-user hero-opponent)
    (partial lol-gen-key-for-matchup-comment-id
             hero-user hero-opponent)
    data)
  )

(defn lol-get-question-for-matchup-by-id
  "Return question of matchup by number."
  [hero-user hero-opponent id]
  (get-key (lol-question-by-matchup-and-id-key
    hero-user hero-opponent id)))

(defn lol-get-question [id]
  (get-key (lol-question-by-id-key id)))

(defn generic-traverse-nodes
  "Generically traverse all nodes from start-at index
  with count-key which stores value count and
  id generation function id-gen-function"
  ([start-at count-key id-gen-function]
  (let [matchup-count (get-key count-key)]
    (if (some? matchup-count)
      (let [produce-for-id
        (fn [id]
           {:count id
            :until (dec matchup-count)
            :val (get-key (id-gen-function id))})]
        (take-while some? (iterate (fn [val-map]
                 (if (= (:count val-map) (:until val-map))
                   nil
                   (produce-for-id (inc (:count val-map)))))
                 (produce-for-id start-at))))
      nil)
    ))
  ([count-key id-gen-function]
   (generic-traverse-nodes 0 count-key id-gen-function))
  )

(defn lol-traverse-matchup-questions
  "Make iterator to traverse all question values (lazy)"
  [hero-user hero-opponent]
  (generic-traverse-nodes
    (lol-gen-key-for-matchup-question-count
      hero-user hero-opponent)
    (partial lol-get-question-for-matchup-by-id
      hero-user hero-opponent)))

(defn lol-traverse-matchup-comments
  "Make iterator to traverse all question values (lazy)"
  [hero-user hero-opponent]
  (generic-traverse-nodes
    (lol-gen-key-for-matchup-comment-count
      hero-user hero-opponent)
    (partial lol-gen-key-for-matchup-comment
      hero-user hero-opponent)))

(defn lol-traverse-all-questions
  "Make iterator to traverse all question keys (globally)"
  []
  (generic-traverse-nodes
    (lol-question-count-key)
    lol-question-by-id-key))


(defn curr-unix-timestamp []
  (quot (System/currentTimeMillis) 1000))

(defn lol-process-question
  "Process (save) question in form
  {:hero-user :hero-opponent :comment :answers []}"
  [data]
  (let [glob-id (lol-store-next-question-glob-index 0)
        frozen-questions (nippy/freeze
         {:globid glob-id :date (curr-unix-timestamp) :answers (:answers data)})
        hu (:hero-user data)
        ho (:hero-opponent data)
        ]
    (let [qid (lol-store-next-question-for-matchup-by-id
      hu ho frozen-questions)]
      (set-key (lol-question-by-id-key glob-id)
               (lol-question-by-matchup-and-id-key-id hu ho))
      (lol-store-next-comment-for-matchup-id hu ho
         (nippy/freeze {:qid qid :comment data}))
      )
    )
  )

