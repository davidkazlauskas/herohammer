(ns the-hero-hammer.hh_process
  (:require [the-hero-hammer.hh_context :refer :all]))

(defn global-question-count []
  (get-key ((fn-global-question-count))))

(defn get-first-occourences-of-questions [questions]
  (->> questions
       (map question-index)
       (map get-question-first-time)
       (into [])))

(defn new-filter-count [questions]
  (if (= nil questions)
    {:from 0 :to 0}
    (let [max-occourence
          (first (max
            (get-first-occourences-of-questions
                 questions)))]
      (if (some? max-occourence)
        {:from max-occourence :to max-occourence}
        nil))))

(defn get-filter-questions [filter-id]
  (->> (all-filters)
       (filter #(= (:id %) filter-id))
       (map #(:required-questions %))
       (first)))

(defn fetch-filter-new-or-empy
  "Create filter record if not exists."
  [matchup-pair question-id filter-id]
  (let [curr (get-matchup-filter-count matchup-pair question-id filter-id)]
    (if (= nil curr)
      (new-filter-count
        (get-filter-questions filter-id))
      curr)))

(defn get-all-filters-for-matchup
  [hero-user-id hero-opponent-id]
  (->> (range (count (all-filters)))
       (map (partial
            fetch-filter-new-or-empy
            hero-user-id hero-opponent-id))
       (into [])))

(defn process-single-pair [currmax to-process]
  (let [the-filters (apply get-all-filters-for-matchup to-process)]
    (let [paired (zip-counts-with-filters the-filters currmax)]
      ; paired -> [ [ <count> <metadata> <expected range> ] .. ]
      (let [freqs (filter-frequencies paired)]
        (process-according-to-frequences freqs paired to-process)))))

(defn process-pairs [to-process]
  (let [currmax (global-question-count)]
    (into [] (map
       (partial process-single-pair currmax)
       to-process))))
