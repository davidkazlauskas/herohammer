(ns the-hero-hammer.hh_process
  (:require [the-hero-hammer.hh_context :refer :all]))

(defn global-question-count []
  (get-key ((fn-global-question-count))))

(defn global-question-proc []
  (get-key ((fn-global-question-proc))))

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
    {:from 0 :to 0 :count
     (new-count-vector answer-count)}
    (let [max-occourence
          (first (max
            (get-first-occourences-of-questions
                 questions)))]
      (if (some? max-occourence)
        {:from max-occourence
         :to max-occourence
         :count (new-count-vector answer-count)}
        nil))))

(defn get-filter-questions [filter-id]
  (->> (filters-full)
       (filter #(= (:id %) filter-id))
       (map #(:required-questions %))
       (first)))

(defn filter-frequencies [filters-pending]
  (->> filters-pending
       (map :expected-rng)
       frequencies
       (filter #(> 0 (- (:to %1) (:from %1))))
       (sort-by #(get % 1) >)
       (into [])))

(defn fetch-filter-new-or-empty
  "Create filter record if not exists."
  [matchup-pair question-id filter-id answer-count]
  (let [curr (get-matchup-filter-count
              matchup-pair question-id filter-id)]
    (if (= nil curr)
      (new-filter-count
        (get-filter-questions filter-id) answer-count)
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
            (apply fetch-filter-new-or-empty
            (flatten
              [matchup-pair (extract-ids-from-cross %1)
              (count (get-in %1 [:question :options]))]))))
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

(defn inc-arr-index-longs [arr idx]
  (aset ^longs arr idx (inc (aget ^longs arr idx))))

(defn sum-up-filters
  [filtered frequency to-sum matchup-pair limit]
  (let [range-to-get (shortened-range
                     frequency limit)
        questions (get-n-questions-matchup-id
                    matchup-pair range-to-get)]
    (doall
      (map-indexed #(let [mapped (answer-vec-to-map (:answers %2))]
         (doseq [iter (range (count filtered))]
           (let [i (nth filtered iter)
                 my-key (get mapped
                   (get-in i [:question :id]))]
             (if (some? my-key)
               (let [filter-arg {
                      :answer-map mapped
                      :date (:date %2)
                      :globid (:globid %2)
                      :currid (+ (:from range-to-get) %1)
                      :curr-range {
                         :from (get-in i [:count :from])
                         :to (+
                           (get-in i [:count :to])
                           (aget ^longs (:traversed to-sum) iter))}
                    }]
                  (if (= (get-in filter-arg [:curr-range :to])
                         (:currid filter-arg))
                    (let [filter-key
                          ((get-in i
                             [:filter :process-question])
                           filter-arg)]
                      (inc-arr-index-longs
                        (:traversed to-sum) iter)
                      (if (= filter-key (count-in))
                        (inc-arr-index-longs
                          (aget (:counts to-sum)
                             iter) my-key))
                      )))))))
         questions))
  (- (:to range-to-get) (:from range-to-get))))

(defn make-count-array [filters]
  (let [result (object-array (count filters))]
    (doall (map-indexed #(aset
       result %1 (make-array Long/TYPE
        (count (get-in %2 [:question :options])))
    ) filters))
    result))

(defn post-proc-results [summed filters]
  (->> filters
       (map-indexed #(let [curr-cnt
                           (get-in %2 [:count :count])]
           (assoc-in %2 [:count]
             {:count (into [] (for [i (range (count curr-cnt))]
               (+ (nth curr-cnt i)
                  (aget ^longs (aget (:counts summed) %1) i))))
              :from (get-in %2 [:count :from])
              :to (+ (get-in %2 [:count :to])
                     (aget ^longs (:traversed summed) %1))
             })))
       (into [])))

(defn save-post-proc [filters matchup-pair]
  (->> filters
       (map #(let [curr-cnt (get %1 :count)]
         (set-matchup-filter-count
           matchup-pair
           (get-in %1 [:question :id])
           (get-in %1 [:filter :id])
           curr-cnt)))
       doall))

(defn process-frequency
  "Process single frequency with filter"
  [freqency filters matchup-pair limit]
  (let [filtered
        (into [] (filter
           #(= (:expected-rng %) (nth freqency 0))
           filters))
        victim-array
          {:counts (make-count-array filtered)
           :traversed (make-array Long/TYPE (count filtered))}]
    (let [res (sum-up-filters
      filtered (nth freqency 0)
      victim-array matchup-pair limit)]
      (save-post-proc
        (post-proc-results
          victim-array filters)
        matchup-pair)
      res)))

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
