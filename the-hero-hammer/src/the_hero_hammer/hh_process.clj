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
    {:from 0 :to 0 :count 0}
    (let [max-occourence
          (first (max
            (get-first-occourences-of-questions
                 questions)))]
      (if (some? max-occourence)
        {:from max-occourence
         :to max-occourence :count 0}
        nil))))

(defn get-filter-questions [filter-id]
  (->> (all-filters)
       (filter #(= (:id %) filter-id))
       (map #(:required-questions %))
       (first)))

(defn filter-frequencies [filters-pending]
  (->> filters-pending
       (map :expected-rng)
       frequencies
       (sort-by #(get % 1) >)
       (into [])))

(defn fetch-filter-new-or-empy
  "Create filter record if not exists."
  [matchup-pair question-id filter-id]
  (let [curr (get-matchup-filter-count matchup-pair question-id filter-id)]
    (if (= nil curr)
      (new-filter-count
        (get-filter-questions filter-id))
      curr)))

(defn extract-ids-from-cross
  "Extract ids [<qid> <filterid>]"
  [q-and-filter-cross]
  [(:id (:question q-and-filter-cross))
   (:id (:filter q-and-filter-cross))])

(defn get-all-filters-for-matchup
  [matchup-pair currmax]
  (->> (questions-filters-cross)
       (map #(assoc %1 :count
            (apply fetch-filter-new-or-empy
            matchup-pair (extract-ids-from-cross %1))))
       (map #(assoc %1 :expected-rng
            ((:expected (:filter %1))
             currmax (:count %1))))
       (into [])))

(defn process-single-pair [currmax to-process]
  (let [matchup-pair (vec-to-matchup to-process)
        the-filters (get-all-filters-for-matchup
                      matchup-pair currmax)]
  ; the-filters -> [ { :question :filter :expected-rng :count } .. ]
      (let [freqs (filter-frequencies the-filters)]
        (process-according-to-frequences
          freqs the-filters matchup-pair))))

(defn process-pairs [to-process]
  (let [currmax (global-question-count)]
    (into [] (map
       (partial process-single-pair currmax)
       to-process))))
