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

(defn shortened-range [old limit]
  (if (> (- (:to old) (:from old)) limit)
    {:from (:from old) :to (+ (:from old) limit)}
    {:from (:from old) :to (:to old)}))

(defn answer-vec-to-map [ans-vec]
  (->> ans-vec
       (partition 2)
       (map (partial apply vector))
       (into {})))

(defn sum-up-filters
  [filtered frequency to-sum matchup-pair limit]
  (let [range-to-get (shortened-range
                     frequency limit)
        questions (get-n-questions-matchup-id
                    matchup-pair range-to-get)]
    (map-indexed #(
       (let [partitioned (answer-vec-to-map (:answers %1))])
    ) filtered)
  (- (:to range-to-get) (:from range-to-get))))

(defn process-frequency
  "Process single frequency with filter"
  [freqency filters matchup-pair limit]
  (let [filtered
        (into [] (filter
           #(= (:expected-rng %) (nth freqency 0))
           filters))
        victim-array
          {:counts (make-array Long/TYPE (count filtered))
           :traversed (make-array Long/TYPE (count filtered))}]
    (println "fret" freqency matchup-pair)
    (sum-up-filters
      filtered (nth freqency 0)
      victim-array matchup-pair limit)))

(defn proc-chunk-size [] 128)

(defn process-according-to-frequences
  "Process according to frequences,
  first greatest, then the rest"
  [frequences the-filters to-process]
  ;(for [x frequences]
      ;(process-n-questions (get x 0) the-filters
        ;(get to-process 0) (get to-process 1)))
    (loop [to-process-lim (proc-chunk-size) i 0]
      (if (and
            (< i (count frequences))
            (> to-process-lim 0))
        (recur (- to-process-lim
          (process-frequency
            (nth frequences i)
            the-filters
            to-process
            to-process-lim))
          (inc i))))
    frequences
  )

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
