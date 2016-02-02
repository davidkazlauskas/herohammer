(ns the-hero-hammer.shared_context
  (:require [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [co.paralleluniverse.pulsar.core :as pulsar]))

(defn process-questions [map-red-job]
  (let [max-units 128]
    (advance-map-reduce-job map-red-job max-units)))

(defn schedule-question-processing [map-red-job]
  (pulsar/spawn-fiber process-questions map-red-job))

(defn main-job [map-red-job proc-delay-ms]
  (while true
    (schedule-question-processing map-red-job)
    (pulsar/sleep proc-delay-ms)))

(defn gen-jobs [map-red-job]
  (fn [] (pulsar/spawn-fiber main-job map-red-job)))

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
             :initial-range {:from first-occ :to first-occ}
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
