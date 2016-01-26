(ns the-hero-hammer.hh_process
  (:require [the-hero-hammer.hh_context :refer :all]))

(defn global-question-count []
  (get-key ((fn-global-question-count))))

(defn global-question-proc []
  (get-key ((fn-global-question-proc))))

(defn set-global-question-proc [the-proc]
  (set-key ((fn-global-question-proc)) the-proc))

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
    (let [first-occ (get-first-occourences-of-questions
                      questions)
          max-occourence
          (first (max first-occ))]
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
       (filter #(> (range-size (get %1 0)) 0))
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

(defn expected-rng-for-filter [currmax flt-map]
  (assoc flt-map :expected-rng
            ((get-in flt-map [:filter :expected])
             currmax (:count flt-map))))

(defn count-for-filter [matchup-pair flt-map]
  (assoc flt-map :count
    (apply fetch-filter-new-or-empty
      (flatten
        [matchup-pair (extract-ids-from-cross flt-map)
        (count (get-in flt-map [:question :options]))]))))

(defn get-all-filters-for-matchup
  [matchup-pair currmax]
  (->> (questions-filters-cross)
       (map (partial count-for-filter currmax))
       (map (partial expected-rng-for-filter currmax))
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

(defn matchup-question-count [matchup-pair]
  (or
    (get-key ((fn-matchup-question-count) matchup-pair))
    0))

(defn process-single-pair [matchup-pair]
  (let [currmax (matchup-question-count matchup-pair)
        the-filters (get-all-filters-for-matchup
                      matchup-pair currmax)]
  ; the-filters -> [ { :question :filter :expected-rng :count } .. ]
      (let [freqs (filter-frequencies the-filters)]
        (process-according-to-frequences
          freqs the-filters matchup-pair))))

(defn process-pairs [to-process]
  (into [] (map
     (partial process-single-pair)
     to-process)))

(defn fetch-global-and-pair
  [the-range]
  (let [the-keys (get-n-global-questions the-range)
        matchups (->> the-keys
                      (map matchup-pair-from-key)
                      (distinct)
                      (into []))]
    matchups))

(defn fetch-glob-to-proc-range []
  (let [glob-cnt (or (global-question-count) 0)
        proc (or (global-question-proc) 0)
        proc-diff (- glob-cnt proc)]
    (if (> proc-diff 0)
      (let [range-to-get {:from proc :to glob-cnt}
            final-proc (shorten-range
                         range-to-get
                         (proc-chunk-size))]
        final-proc)
      nil)))

(defn process-pending
  "Process all pending questions in db"
  []
  (let [final-proc (fetch-glob-to-proc-range)]
    (if (some? final-proc)
      (let [the-pairs (fetch-global-and-pair final-proc)
            out-res (process-pairs the-pairs)]
        (set-global-question-proc (:to final-proc))
        out-res)
        "NOTHING_TO_PROCESS")))
