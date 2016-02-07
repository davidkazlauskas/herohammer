(ns the-hero-hammer.shared_context
  (:require [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.hh_context :refer :all]
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

(defn generic-processing-job
  "Argmap:
  :glob-question-key -> global key for questions (to db)
  :id-key-gen -> function with 1 arg (id) to get glob question id
  :max-proc -> maximum questions to process at a time
  "
  [argmap]
  (let [glob-question-key (:glob-question-key argmap)
        id-key-gen (:id-key-gen argmap)
        max-proc (:max-proc argmap 128)]
    {:save-key-func glob-question-key
     :map-function id-key-gen
     :initial-reduce (fn [the-val]
                        (java.util.ArrayList.))
     :final-reduce (fn [the-val] (let [dist (distinct the-val)]
                                    (doseq [i dist]
                                      (process-matchup-pair argmap i max-proc)))
                      nil)
     :reduce-function (fn [the-val mapped]
                         (.add the-val mapped)
                         the-val)}))

(defn most-popular-question-sort [the-arr]
  (sort-by #(if %1 (:count %1) 0) > the-arr))

(defn distinct-java-array [the-arr]
  (let [the-vec (vec the-arr)
        grouped (group-by :key the-vec)
        gfiltered (filter #(some? (get % 0)) grouped)
        mapped (mapv
         #(hash-map :key (nth %1 0)
                    :count (apply + (map :count (nth %1 1))))
         gfiltered)
        dcount (count mapped)]
    (dotimes [n (count the-arr)]
      (let [to-set (if (< n dcount)
                     (nth mapped n) nil)]
        (aset the-arr n to-set))))
  the-arr)

(defn most-popular-matchups-proc-job
  "Argmap:
  :most-popular-matchups-key -> key for most popular matchups
  :turn-key-to-uniq-matchup -> key to unique matchup function
  "
  [argmap]
   (let [most-pop-matchups-key (:most-popular-matchups-key argmap)
         key-to-uniq-matcup (:turn-key-to-uniq-matchup argmap)]
     {:save-key-func most-pop-matchups-key
       :map-function (fn [the-val]
                       (let [matchup (matchup-pair-from-key the-val)
                             qcount (get-matchup-question-count matchup)]
                       {:count qcount
                        :key (key-to-uniq-matcup the-val)}))
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
                          (distinct-java-array the-arr)
                          (most-popular-question-sort the-arr)
                          the-arr)
      })
  )
