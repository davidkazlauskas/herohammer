(ns the-hero-hammer.shared_context
  (:require [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.db_context :refer :all]
            [taoensso.nippy :as nippy]
            [co.paralleluniverse.pulsar.core :as pulsar])
  (:import [co.paralleluniverse.fibers Fiber]))

(defmacro n-min [n] (* 1000 60 n))

(defn process-questions [map-red-job]
  (let [max-units 128]
    (advance-map-reduce-job map-red-job max-units)))

(defn schedule-question-processing [map-red-job]
  (pulsar/spawn-fiber process-questions map-red-job))

(defn main-job [map-red-job proc-delay-ms]
  (while true
    (schedule-question-processing map-red-job)
    (Fiber/sleep proc-delay-ms)))

(defn gen-jobs [map-red-job delay-ms]
  (fn [] (pulsar/spawn-fiber main-job map-red-job delay-ms)))

(defn generate-matchup-tasks [argmap the-pair]
  "Argmap:
  :generate-filter-matchup-question-count ->
    generate question count key for filter and matchup
  "
  (let [generate-filter-matchup-question-count
        (:generate-filter-matchup-question-count argmap)]
    (into []
      (filter some?
        (for [question (questions-full)
              flt (filters-full)]
          (let [first-occ (get-question-first-time (:id question))]
            (if first-occ {:save-key-func
               (generate-filter-matchup-question-count
                 the-pair
                 (:id question)
                 (:id flt))
             :map-function (fn [the-val]
                             (let [the-map
                                   (->> (:answers the-val)
                                        (partition 2)
                                        (map #(into [] %1))
                                        (into {}))
                                   our-val (get the-map (:id question))
                                   ]
                               our-val))
             :initial-reduce (fn [the-val]
                               (let [the-victim
                                 (long-array
                                  (count
                                    (:options question)))]
                               (if the-val
                                 (dotimes [i (count the-val)]
                                   (aset the-victim i (nth the-val i))))
                               the-victim))
             :final-reduce (fn [the-val] (vec the-val))
             :reduce-function (fn [old-val curr]
                                (if (some? curr)
                                  (inc-arr-index-longs
                                    old-val curr))
                                old-val)
             :initial-range {:from 0 :to 0}
           })))))))

(defn matchup-pair-map-reduce-job [argmap the-pair]
  "Argmap:
  :count-key-func -> function to generate count from pair
  :generate-matchup-question-id -> function to generate matchup question id
    takes 2 args, pair + filter
  "
  (let [count-key-func (:count-key-func argmap)
        generate-matchup-question-id (:generate-matchup-question-id argmap)]
  {:count-key (count-key-func the-pair)
   :id-key-function (partial generate-matchup-question-id the-pair)
   :is-nipped true
   :tasks (generate-matchup-tasks argmap the-pair)}))

(defn process-matchup-pair [argmap pair max-chunk]
  (let [the-job (matchup-pair-map-reduce-job argmap pair)]
    (advance-map-reduce-job the-job max-chunk)))

(defn generic-processing-job-final-reduce
  "Argmap:
  :glob-question-key -> global key for questions (to db)
  :id-key-gen -> function with 1 arg (id) to get glob question id
  :max-proc -> maximum questions to process at a time
  "
  [argmap]
  (let [glob-question-key (:glob-question-key argmap)
        id-key-gen (:id-key-gen argmap)
        max-proc (:max-proc argmap 128)
        final-reduce-proc-job (:final-reduce-job argmap)]
    {:save-key-func glob-question-key
     :map-function id-key-gen
     :initial-reduce (fn [the-val]
                        (java.util.ArrayList.))
     :final-reduce final-reduce-proc-job
     :reduce-function (fn [the-val mapped]
                         (.add the-val mapped)
                         the-val)}))

(defn generic-processing-job [argmap]
  (let [max-proc (:max-proc argmap 128)]
   (generic-processing-job-final-reduce
    (merge argmap
           {:final-reduce-job
             (fn [the-val]
               (let [dist (distinct the-val)]
                 (doseq [i dist]
                   (process-matchup-pair argmap i max-proc))))}))))

(defn most-popular-question-sort [the-arr]
  (sort-by #(if %1 (:count %1) 0) > the-arr))

(defn distinct-java-array-generic [the-arr aggregate]
  (let [the-vec (vec the-arr)
        grouped (group-by :key the-vec)
        gfiltered (filter #(some? (get % 0)) grouped)
        mapped (mapv
         #(hash-map :key (nth %1 0)
                    :count (apply aggregate (map :count (nth %1 1))))
         gfiltered)
        dcount (count mapped)]
    (dotimes [n (count the-arr)]
      (let [to-set (if (< n dcount)
                     (nth mapped n) nil)]
        (aset the-arr n to-set))))
  the-arr)

(defn distinct-java-array [the-arr]
  (distinct-java-array-generic the-arr max))

(defn distinct-java-array-sum [the-arr]
  (distinct-java-array-generic the-arr +))

(defn most-popular-matchups-proc-job
  "Argmap:
  :most-popular-matchups-key -> key for most popular matchups
  :turn-key-to-uniq-matchup -> key to unique matchup function
  :distinct-sort-function -> sorting function
  :matchup-question-count-func -> matchup question count function
  "
  [argmap]
   (let [most-pop-matchups-key (:most-popular-matchups-key argmap)
         key-to-uniq-matchup (:turn-key-to-uniq-matchup argmap)
         distinct-func (:distinct-sort-function argmap distinct-java-array)
         get-matchup-question-count-func
           (:matchup-question-count-func argmap get-matchup-question-count)]
     {:save-key-func most-pop-matchups-key
       :map-function (fn [the-val]
                       (let [matchup (matchup-pair-from-key the-val)
                             qcount (get-matchup-question-count-func matchup)]
                       {:count qcount
                        :key (key-to-uniq-matchup the-val)}))
       :initial-reduce (fn [the-val]
                         (let [the-arr (object-array 11)]
                           (if the-val
                             (do
                               (dotimes [i (count the-val)]
                                 (aset the-arr i (nth the-val i)))
                               (most-popular-question-sort the-arr)
                               ))
                           the-arr))
       :final-reduce (fn [the-arr]
                       (vec the-arr))
       :reduce-function (fn [the-arr mapped]
                          (aset the-arr 10 mapped)
                          (distinct-func the-arr)
                          (most-popular-question-sort the-arr)
                          the-arr)
      }))

(defn sum-all-matchups-generic [filter-id matchups]
  (let [q-filt-func (fn-question-filter-count)
        q-ids (mapv :id (questions-full))
        the-keys (for [i (questions-full)]
          (let [the-id (:id i)
                gen-keys (->> matchups
                   (mapv #(q-filt-func % the-id filter-id)))]
            {:question i :batch gen-keys}))]
    (->> the-keys
         (mapv (fn [key-batch]
            (let [the-q (:question key-batch)
                  the-batch (:batch key-batch)
                  batch-queried (get-key-batch the-batch)
                  extract (->> batch-queried
                               (filterv some?)
                               (mapv #(:val (nippy/thaw %)))
                               (#(if (empty? %1)
                                  [(empty-q-vec the-q)] %1))
                               (apply mapv +))]
              {:question the-q
               :sum extract}))))))

(defn sum-all-matchups-with-hero [hero-num filter-id]
  (let [matchups
        (->> (range (count (heroes-full)))
         (mapv #(hash-map :user hero-num :opponent %)))]
   (sum-all-matchups-generic filter-id matchups)))

(defn sum-all-matchups-with-opponent [opponent-num filter-id]
  (let [matchups
        (->> (range (count (heroes-full)))
         (mapv #(hash-map :user % :opponent opponent-num)))]
   (sum-all-matchups-generic filter-id matchups)))

(defn update-all-hero-stats-generic [hero-vec sum-func key-gen-func]
  (for [flt (filters-full)
        hero hero-vec]
    (let [flt-id (:id flt)
          sum-res (sum-func hero flt-id)]
      (doseq [i sum-res]
        (let [q-id (get-in i [:question :id])
              r-count (:sum i)
              frozen (nippy/freeze r-count)
              the-key (key-gen-func hero q-id flt-id)]
          (set-key the-key frozen))))))

(defn update-all-hero-stats [hero-vec]
  (update-all-hero-stats-generic
    hero-vec sum-all-matchups-with-hero
    (fn-hero-question-filter-count)))

(defn update-all-opponent-stats [hero-vec]
  (update-all-hero-stats-generic
    hero-vec sum-all-matchups-with-opponent
    (fn-opponent-question-filter-count)))

(defn generic-processing-job-final-reduce-distinct [argmap the-func]
  (generic-processing-job-final-reduce
    (merge argmap
           {:final-reduce-job
            (fn [the-val]
              (let [dist (into [] (distinct the-val))]
                (the-func dist)))
            })))

(defn generic-processing-job-proc-hero-counts [argmap]
  (generic-processing-job-final-reduce-distinct
    argmap update-all-hero-stats))

(defn generic-processing-job-proc-opponent-counts [argmap]
  (generic-processing-job-final-reduce-distinct
    argmap update-all-opponent-stats))

(defn all-relevant-hero-data [filter-id]
  (mapv #(hash-map :data (fetch-relevant-hero-data %1 filter-id) :hero-id %1)
        (range (count (heroes-full)))))

(defn highest-group-comp [ans-by left right]
  (let [ratio-left (:ratio left)
        ratio-right (:ratio right)]
   (if (not (= ratio-left ratio-right))
     (> ratio-left ratio-right)
     (> (get-in left [:answers ans-by])
        (get-in right [:answers ans-by])))))

(defn highest-for-group [data-set answer amount]
  (let [comp-func (partial highest-group-comp answer)
        ratios-mapped
         (mapv #(let [ans (:answers %1)
                      exp-count (nth ans answer)
                      full-sum (apply + ans)
                      divisor (if (> full-sum 0) full-sum 1)
                      the-ratio (/ exp-count divisor)]
                 (assoc %1 :ratio the-ratio))
          data-set)]
   (into [] (take amount (sort comp-func ratios-mapped)))))

(defn group-by-single-question [all-rel-data the-question]
  (let [the-id (:id the-question)]
    (mapv #(let [h-id (:hero-id %1)
                 data (:data %1)
                 this-q-ans (nth data the-id)
                 raw-ans (:answers this-q-ans)]
             {:hero-id h-id :answers raw-ans}
            ) all-rel-data)))

(defn full-update-for-questions []
  (for [key-gen-func (fn-question-filter-top-answer-count)
        flt (filters-full)]
    (let [flt-id (:id flt)
          the-dataset (all-relevant-hero-data flt-id)]
      (doseq [q (questions-full)]
        (let [qid (:id q)
              options (:options q)
              grouped-single (group-by-single-question the-dataset q)
              to-save (mapv #(highest-for-group grouped-single %1 10)
                        (range (count options)))
              nipped (nippy/freeze to-save)
              to-store-key (key-gen-func qid flt-id)]
          (set-key to-store-key nipped))))))
