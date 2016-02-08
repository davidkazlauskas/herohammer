(ns the-hero-hammer.hh_context
  (:require [the-hero-hammer.db_context :refer :all]
            [taoensso.nippy :as nippy]))

; EZ START
;(def ^:dynamic *ctx-get-func* (fn [] the-hero-hammer.lol_context/*hh-context-lol*))
;(def ^:dynamic *ctx-get-func* (fn [] the-hero-hammer.dota_context/*hh-context-dota*))

(def ^:dynamic *hh-context* nil)
(def ^:dynamic *ctx-get-func* nil)

(defn get-ctx-jobs []
  (get-in (*ctx-get-func*) [:jobs]))

(defn filters-full []
  (get-in (*ctx-get-func*) [:filters :full]))

(defn questions-filters-cross []
  (get-in (*ctx-get-func*)
    [:questions :cross-question-filter]))

; CONTEXT UTIL
(defn hero-name-full-to-short
  "Remove non alphanum chars from name.
  Cho'Gath -> chogath"
  [hname]
  (clojure.string/replace
    (clojure.string/lower-case hname)
    #"[^a-z0-9]" ""))

(defn heroes-shortnames
  "Get shortname vector of heroes"
  [full]
  (->> full
       (map hero-name-full-to-short)
       (into [])))

(defn heroes-full-to-short
  "Turn full hero map to map which takes
  shortname and retrieves key (index)"
  [full]
  (->> full
       (map-indexed #(vector
         (hero-name-full-to-short %2) %1))
       (into {})))

(defn gen-matchup [u o] {:user u :opponent o})

(defn vec-to-matchup [vecm]
  {:user (nth vecm 0) :opponent (nth vecm 1)})

(defn question-index-shortnames [questions]
  (->> questions
       (map #(vector (:shortname %1) (:id %1)))
       (into {})))

(defn question-index
  "Turn question shortname to index."
  [shortname]
  (get-in (*ctx-get-func*)
    [:questions :short-to-index shortname]))

(defn hero-index
  "Turn shortname to index"
  [shortname]
  (get-in (*ctx-get-func*) [:heroes :short-to-index shortname]))

(defn matchup-shortname-to-index
  "Turn shortnames to indexes"
  [hero-user-shortname hero-opponent-shortname]
  (gen-matchup
    (hero-index hero-user-shortname)
    (hero-index hero-opponent-shortname)))

(defn cross-questions-and-filters
  [questions filters]
  (into [] (for [q questions f filters]
    {:question q :filter f})))

(defn matchup-pair-from-key [the-key]
  ((get-in (*ctx-get-func*)
           [:util :matchup-pair-from-key])
   the-key))

; QUESTONS
(defn questions-full []
  (get-in (*ctx-get-func*) [:questions :full]))

; HEROES
(defn heroes-full []
  (get-in (*ctx-get-func*) [:heroes :full]))

(defn heroes-count []
  (count (heroes-full)))

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

(defn ranges-overlap [range-a range-b]
  (if (and
        (> (:to range-a) (:from range-b))
        (< (:from range-a) (:to range-b)))
    true
    false))

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
  (let [curr (atomic-increment-key count-key)]
    (let [toreturn curr]
      (set-key (id-gen-function toreturn) data)
      toreturn)))

(defn generic-traverse-nodes-raw-count
  "Generically traverse all nodes from start-at index
  with count-key which stores value count and
  id generation function id-gen-function"
  ([start-at predef-count id-gen-function]
  (if (< start-at predef-count)
    (let [matchup-count predef-count]
      (if (some? matchup-count)
        (let [produce-for-id
          (fn [id]
             {:count id
              :until (dec matchup-count)
              :val (get-key (id-gen-function id))})]
          (take-while some? (iterate (fn [val-map]
                   (if (or
                         (nil? (:val val-map))
                         (= (:count val-map) (:until val-map)))
                     nil
                     (produce-for-id (inc (:count val-map)))))
                   (produce-for-id start-at))))
        nil)
      )))
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

(defn fn-global-question-proc
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :glob-question-proc]))

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

(defn fn-matchup-comment-count
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :matchup-comment-count]))

(defn fn-matchup-comment-id
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :matchup-comment-id]))

(defn fn-question-first-time-id
  "Returns function."
  []
  (get-in (*ctx-get-func*) [:queries :question-first-time]))

(defn fn-question-filter-count
  "Returns function (matchup-pair question-id filter-id)"
  []
  (get-in (*ctx-get-func*) [:queries :matchup-filter-count]))

(defn fn-hero-question-filter-count
  "Returns function (hero question-id filter-id)"
  []
  (get-in (*ctx-get-func*) [:queries :matchup-hero-count]))

(defn fn-macthup-most-popular-global
  "Returns function (matchup-pair question-id filter-id)"
  []
  (get-in (*ctx-get-func*) [:queries :matchup-most-popular-global]))

(defn get-most-popular-matchups-global []
  (let [the-data (get-key ((fn-macthup-most-popular-global)))]
    (if the-data (butlast (:val (nippy/thaw the-data))) nil)))

(defn get-most-recent-questions [count-from-top]
  (let [count-key ((fn-global-question-count))
        count-res (get-key count-key)
        glob-id-gen (fn-global-question-id)
        ]
    (if count-res
      (let [bot-range (- count-res (or count-from-top 10))
            fn-bot-range (if (>= bot-range 0) bot-range 0)
            the-keys (->> (range fn-bot-range count-res)
                          (map glob-id-gen)
                          (into []))
            the-pull-res (get-key-batch the-keys)]
        (if the-pull-res
          (into [] (map nippy/thaw the-pull-res)))))))

; SPEC OPS
(defn gen-matchup [u o] {:user u :opponent o})

(defn range-size [the-range]
  (- (:to the-range) (:from the-range)))

(defn shorten-range [the-range max-len]
  (let [size (range-size the-range)]
    (if (> size max-len)
      {:from (:from the-range)
       :to (+ (:from the-range) max-len)}
      the-range)))

(defn short-gen-matchup [u o]
  {:user (hero-index u) :opponent (hero-index o)})

(defn store-next-question-global [data]
  (generic-store-next-item
    ((fn-global-question-count))
    (fn-global-question-id)
    (nippy/freeze data)))

(defn traverse-all-questions-global []
  "Make iterator to traverse all question keys (globally)"
  (->> (generic-traverse-nodes
         ((fn-global-question-count))
         (fn-global-question-id))
       (map #(update-in %1 [:val] nippy/thaw))))

(defn get-n-global-questions
  "Get specified range of global questions.
  Example of range-to-get: {:from 0 :to 7}"
  [range-to-get]
  (->> (generic-traverse-nodes-raw-count
         (:from range-to-get)
         (:to range-to-get)
         (fn-global-question-id))
       (filter #(some? (:val %1)))
       (map #(nippy/thaw (:val %1)))
       (into [])))

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
  (->> (generic-traverse-nodes
         ((fn-matchup-question-count) matchup)
         (partial (fn-matchup-question-id) matchup))
       (map #(update-in %1 [:val] nippy/thaw))))

(defn get-answers-with-comment [matchup id]
  (let [ans-key ((fn-matchup-question-id) matchup id)
        comm-key ((fn-matchup-comment-id) matchup id)
        batch (get-key-batch [ans-key comm-key])
        ans (nth batch 0)
        comm (nth batch 1)]
    (if ans
      (let [ans-unzipped (nippy/thaw ans)]
        (if comm
          (assoc ans-unzipped :comment
            (:comment (nippy/thaw comm)))
          ans-unzipped)))))

(defn get-matchup-question-count [matchup]
  (let [res (get-key ((fn-matchup-question-count) matchup))]
    (or res 0)))

(defn store-next-comment-matchup
  "Store next question for matchup"
  [matchup data]
  (generic-store-next-item
    ((fn-matchup-comment-count) matchup)
    (partial (fn-matchup-comment-id) matchup)
    data))

(defn traverse-all-comments-matchup
  "Traverse all questions for matchup."
  [matchup]
  (->>
    (generic-traverse-nodes
      ((fn-matchup-comment-count) matchup)
      (partial (fn-matchup-comment-id) matchup))
    (map #(update-in %1 [:val] nippy/thaw))))

(defn get-comments-by-id
  "Get comments for specific matchup"
  [matchup idvec]
  (let [initial-keys
        (into [] (map #((fn-matchup-comment-id) matchup %1) idvec))
        initial-query (get-key-batch initial-keys)]
    (into [] (map #(if %1 (nippy/thaw %1)) initial-query))))

(defn get-comments-count
  "Get comment count for matchup"
  [matchup]
  (or (get-key ((fn-matchup-comment-count) matchup)) 0))

(defn get-question-first-time
  "Get first occourence of question asked"
  [question-id]
  (get-key ((fn-question-first-time-id) question-id)))

(defn get-n-questions-matchup-id
  [matchup-pair spec-range]
  (->> (generic-traverse-nodes-raw-count
        (:from spec-range) (:to spec-range)
        (partial (fn-matchup-question-id)
          matchup-pair))
       (map :val)
       (filter some?)
       (map nippy/thaw)
       (into [])))

(defn set-question-first-time
  "Set first question occourence if not exists."
  [question-id data]
  (set-if-not-exists
    ((fn-question-first-time-id) question-id) data))

(defn curr-unix-timestamp []
  (quot (System/currentTimeMillis) 1000))

(defn get-matchup-filter-count
  [matchup-pair question-id filter-id]
  (let [tmp (get-key ((fn-question-filter-count)
             matchup-pair question-id filter-id))]
    (if (some? tmp) (nippy/thaw tmp) nil)))

(defn set-matchup-filter-count
  [matchup-pair question-id filter-id data]
  (set-key ((fn-question-filter-count)
             matchup-pair question-id filter-id)
           (nippy/freeze data)))

(defn rnd-answers []
  (->> (questions-full)
       (map #(vector (:id %1)
                     (rand-int
                       (count (:options %1)))))
       (into [])))

(defn random-question-data []
  {
   :hero-user (rand-int (heroes-count))
   :hero-opponent (rand-int (heroes-count))
   :comment "berkbreken"
   :answers (rnd-answers)
  })


(defn random-question-for-hero-range [heroes]
  {
   :hero-user (rand-nth heroes)
   :hero-opponent (rand-nth heroes)
   :comment (clojure.string/join
              " " (shuffle
                (clojure.string/split "its peanut butter jelly time" #" ")))
   :answers (rnd-answers)
  })

(defn process-question
  "Process (save) question in form
  {:hero-user :hero-opponent :comment :answers []}"
  [data]
  (let [comm-data (:comment data)
        matchup (gen-matchup (:hero-user data) (:hero-opponent data))
        glob-id (store-next-question-global 0)
        comm-id (if (not (clojure.string/blank? comm-data))
                  (store-next-comment-matchup matchup 0))
        main-block {:globid glob-id :date (curr-unix-timestamp)
          :answers (into [] (flatten (:answers data)))}
        with-comment (if comm-id
                       (assoc main-block :commid comm-id) main-block)
        frozen-questions (nippy/freeze with-comment)
        ]
    (let [qid (store-next-question-matchup
      matchup frozen-questions)]
      (set-key ((fn-global-question-id) glob-id)
               (nippy/freeze
                 ((fn-matchup-question-id) matchup qid)))
      (set-key ((fn-matchup-comment-id) matchup comm-id)
         (nippy/freeze {:qid qid :comment comm-data}))
      (doseq [i (:answers data)]
        (set-question-first-time (get i 0) glob-id)))))

