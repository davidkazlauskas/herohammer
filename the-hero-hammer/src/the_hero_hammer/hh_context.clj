(ns the-hero-hammer.hh_context)

(def ^:dynamic *hh-context* nil)
(def ^:dynamic *ctx-get-func* nil)

(defn get-key [& args]
  "Get key in db"
  (apply (get-in (*ctx-get-func*) [:dbinfo :get-key]) args))

(defn set-key [& args]
  "Set key in db"
  (apply (get-in (*ctx-get-func*) [:dbinfo :set-key]) args))

(defn set-if-not-exists
  "Put data into storage if it doesn't exist"
  [db-key value]
  (if (nil? (get-key db-key))
    (set-key db-key value)))

(defn all-filters []
  ((:filters (*ctx-get-func*)))
  )




; FILTER FUNCS
(defmacro count-in [] 1)
(defmacro ignore [] 2)
(defmacro drop-out [] -1)

(defn max-available-range [curr-max my-range]
  (cond (<= (:to my-range) curr-max)
    {:from (:to my-range) :to curr-max}
    :else {:from (:to my-range) :to (:to my-range)}))

(defn ahead-of-range [number the-range]
  (if (< (:to the-range) number) true nil))

(defn in-range [number the-range]
  (if (and
        (> (:to the-range) number)
        (<= (:from the-range) number)) true nil))

(defn end-of-range [number the-range]
  (if (= (:to the-range) number) true nil))

(defn pair-vec
  "Turn this: {:user 7 :opponent 8}
   to this: [7 8]"
  [matchup]
  [(:user matchup) (:opponent matchup)])

; GENERIC QUERIES
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

(defn generic-traverse-nodes-raw-count
  "Generically traverse all nodes from start-at index
  with count-key which stores value count and
  id generation function id-gen-function"
  ([start-at predef-count id-gen-function]
  (let [matchup-count predef-count]
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
  ([predef-count id-gen-function]
   (generic-traverse-nodes-raw-count 0 predef-count id-gen-function)))

(defn generic-traverse-nodes
  "Generically traverse all nodes from start-at index
  with count-key which stores value count and
  id generation function id-gen-function"
  ([start-at count-key id-gen-function]
  (generic-traverse-nodes-raw-count
    start-at (get-key count-key) id-gen-function))
  ([count-key id-gen-function]
   (generic-traverse-nodes 0 count-key id-gen-function)))

; KEY GETTERS
(defn fn-global-question-count
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :glob-question-count]))

(defn fn-global-question-id
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :glob-question-id]))

(defn fn-matchup-question-count
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :matchup-question-count]))

(defn fn-matchup-question-id
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :matchup-question-id]))

; SPEC OPS
(defn store-next-question-global [data]
  (generic-store-next-item
    ((fn-global-question-count))
    (fn-global-question-id)
    data))

(defn traverse-all-questions-global []
  "Make iterator to traverse all question keys (globally)"
  (generic-traverse-nodes
    ((fn-global-question-count))
    (fn-global-question-id)))

(defn store-next-question-matchup
  "Store next question for matchup"
  [matchup data]
  (generic-store-next-item
    ((fn-matchup-question-count) matchup)
    (partial (fn-matchup-question-id) matchup)
    data))

(defn traverse-all-questions-matchup
  "Traverse all questions for matchup."
  [matchup]
  (generic-traverse-nodes
    ((fn-matchup-question-count) matchup)
    (partial (fn-matchup-question-id) matchup)))
