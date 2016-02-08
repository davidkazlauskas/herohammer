(ns the-hero-hammer.hh_process
  (:require
    [the-hero-hammer.hh_context :refer :all]
    [the-hero-hammer.db_context :refer :all]
    [taoensso.nippy :as nippy]))

(defn global-question-count []
  (get-key ((fn-global-question-count))))

(defn global-question-proc []
  (get-key ((fn-global-question-proc))))

(defn set-global-question-proc [the-proc]
  (set-key ((fn-global-question-proc)) the-proc))

(defn inc-arr-index-longs [arr idx]
  (aset ^longs arr idx (inc (aget ^longs arr idx))))

(defn get-first-occourences-of-questions [questions]
  (->> questions
       (map question-index)
       (map get-question-first-time)
       (into [])))

(defn new-count-vector [num-answers]
  (->> (repeat 0)
       (take num-answers)
       (into [])))

(defn new-filter-count [questions answer-count]
  (if (= nil questions)
    {:from 0 :to 0 :val
     (new-count-vector answer-count)}
    (let [first-occ (get-first-occourences-of-questions
                      questions)
          max-occourence
          (first (max first-occ))]
      (if (some? max-occourence)
        {:from max-occourence
         :to max-occourence
         :val (new-count-vector answer-count)}
        nil))))

(defn fetch-count-for-question
  [matchup filter-id question]
  (assoc question :counts
     (or (get-matchup-filter-count
       matchup (:id question) filter-id)
       (new-filter-count
         nil (count (:options question))))))

(defn fetch-relevant-matchup-data [matchup filter-id]
  (->> (questions-full)
       (map (partial
              fetch-count-for-question
              matchup filter-id))
       (into [])))

(defn fetch-relevant-hero-data [hero filter-id]
  (let [questions (questions-full)
        key-func (fn-hero-question-filter-count)
        the-keys (map
                   #(key-func hero (:id %) filter-id)
                   questions)
        the-batch (get-key-batch the-keys)
        thawed (->> the-batch
                    (mapv #(if %1 (nippy/thaw %1) nil)))
        with-questions
          (map-indexed #(let [the-q (nth questions %1)]
                          (assoc the-q
                            :answers (or %2 (empty-q-vec the-q))))
                       thawed)
        ]
    with-questions))

; GENRIC MAP REDUCE
(defn map-reduce-task-context [the-range id-func nipped]
  {:range the-range
   :id-key-function id-func
   :nippy-record nipped})

(defn map-reduce-task [
   save-key-func
   map-func
   reduce-function
   initial-reduce
   final-reduce]
  {:save-key-func save-key-func
   :map-function map-func
   :reduce-function reduce-function
   :initial-reduce initial-reduce
   :final-reduce final-reduce})

(defn map-single-task-range [the-task]
  (let [db-query (get-key (:save-key-func the-task))
        nipped (if (some? db-query) (nippy/thaw db-query))
        fn-query (or nipped
                     (:initial-range
                       the-task {:from 0 :to 0 :val nil}))
        ]
    (assoc the-task :current-range fn-query)))

(defn map-task-ranges [task-vec proc-range]
  (->> task-vec
       (map map-single-task-range)
       (map #(let [rng (:current-range %1)]
               (if (and (>= (:to rng) (:from proc-range))
                        (<= (:to rng) (:to proc-range)))
                 (assoc %1 :expected-range
                        (max-available-range
                          (:to proc-range) rng)))))
       (filter some?)
       (into [])))

(defn generic-fetch-records [key-func the-range use-nippy]
  (->> (generic-traverse-nodes-raw-count
       (:from the-range) (:to the-range) key-func)
       (map #(if use-nippy
               (assoc %1 :val (nippy/thaw (:val %1))) %1))
       (into [])))

(defn range-sorter [left right]
  (let [lfreq (get left 1)
        rfreq (get right 1)]
     (if (not (= lfreq rfreq))
       (> lfreq rfreq)
       (< (range-size (get left 0))
          (range-size (get right 0)))
       ; prefer smaller ranges
       )))

(defn distill-ranges [task-ranges]
  (->> task-ranges
       (map :expected-range)
       frequencies
       (filter #(> (range-size (get %1 0)) 0))
       (sort-by identity range-sorter)
       (into [])))

(defn tasks-for-range [tasks the-range]
  (->> tasks
       (filter #(ranges-overlap
          (:expected-range %1) the-range))
       (into [])))

(defn make-map-reduce-arrays [the-tasks]
  (let [res-reduce (object-array (count the-tasks))
        res-traverse (long-array (count the-tasks))]

    (doall
      (map-indexed #(do
         (aset res-reduce %1
           ((:initial-reduce %2 identity)
            (get-in %2 [:current-range :val])))
         (aset res-traverse %1 0))
         the-tasks))

    {:arr-reduce res-reduce
     :arr-traversed res-traverse}))

(defn appropriate-to-process [trav-array task-vec current irange]
  (let [curr-task (nth task-vec current)
        curr-task-to (get-in curr-task [:current-range :to])
        curr-count (aget trav-array current)]
    (= (+ curr-task-to curr-count) irange)))

(defn reduce-in-place [red-array i func data]
  (let [outres (func (aget red-array i) data)]
    (aset red-array i outres)))

(defn assemble-final-reduce [red-array trav-array task i]
  {:from (get-in task [:current-range :from])
   :to (+ (get-in task [:current-range :to]) (aget trav-array i))
   :val ((:final-reduce task identity) (aget red-array i))})

(defn map-reduce-single-frequency
  [the-range the-limit full-ranges data]
    (let [the-tasks (tasks-for-range full-ranges the-range)
          victim-array (make-map-reduce-arrays the-tasks)
          trav-array (:arr-traversed victim-array)
          red-array (:arr-reduce victim-array)
          from-rng (:from the-range)
          to-rng (dec (:to the-range))]
      (loop [i from-rng]
           (dotimes [t (count the-tasks)]
             (if (appropriate-to-process
                   trav-array the-tasks t i)
               (let [map-func (:map-function (nth the-tasks t))
                     red-func (:reduce-function (nth the-tasks t))
                     the-q (nth data (- i from-rng))
                     mapped (map-func (:val the-q))]
                 (reduce-in-place red-array t red-func mapped)
                 (inc-arr-index-longs trav-array t))
               ))
           (if (< i to-rng)
             (recur (inc i)))
        )
      ; save results
      (dotimes [t (count the-tasks)]
        (let [curr-t (nth the-tasks t)
              final-res (assemble-final-reduce
                          red-array trav-array curr-t t)
              zipped (nippy/freeze final-res)
              save-key (:save-key-func curr-t)]
          (set-key save-key zipped)))
      (range-size the-range)))

(defn query-reduction-db [task]
   (let [rec (get-key (:save-key-func task))]
     (if (some? rec) (:val (nippy/thaw rec)))))

(defn perform-map-reduce
  [the-range data full-ranges distilled-ranges]
  (let [to-process-lim (range-size
                         the-range)]
    (map-reduce-single-frequency
      the-range
      to-process-lim
      full-ranges
      data)))

(defn process-map-reduce-task-context [the-context task-ranges]
  (let [the-range (:range the-context)
        data (generic-fetch-records
               (:id-key-function the-context)
               the-range
               (:nippy-record the-context))
        distilled-ranges (distill-ranges task-ranges)]
    (perform-map-reduce
      the-range data task-ranges distilled-ranges)
    distilled-ranges))

; THE JOB

(defn map-reduce-job [the-key id-gen-function is-nipped tasks]
  {:count-key the-key
   :id-key-function id-gen-function
   :is-nipped is-nipped
   :tasks tasks})

(defn expected-ranges [task-ranges]
  (map :expected-range task-ranges))

(defn interleave-ranges [the-vec]
  (rest (reductions #(
    hash-map :from (:to %1 %1) :to %2) the-vec)))

(defn job-splits [task-ranges max-size]
  (->> task-ranges
       (map #(vector (:from %1) (:to %1)))
       flatten
       distinct
       sort
       interleave-ranges
       (map #(shorten-range %1 max-size))
       (into [])))

(defn advance-map-reduce-job [the-job max-interval]
  (let [max-int (or max-interval 128)
        curr-key (or (get-key (:count-key the-job)) 0)
        total-range {:from 0 :to curr-key}
        task-ranges (map-task-ranges (:tasks the-job)
                                     total-range)
        expected (expected-ranges task-ranges)
        splits (job-splits expected max-interval)]
    (doseq [i splits]
      (let [ctx (map-reduce-task-context
                  i
                  (:id-key-function the-job)
                  (:is-nipped the-job false))
            task-range-todo (map-task-ranges (:tasks the-job) i)]
        (process-map-reduce-task-context ctx task-range-todo)))))

