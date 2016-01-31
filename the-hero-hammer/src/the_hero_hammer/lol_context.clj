(ns the-hero-hammer.lol_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.storage :as stor]
            [taoensso.nippy :as nippy]
            [co.paralleluniverse.pulsar.core :as pulsar]))

(defn main-filter []
  {:id 0
   :expected max-available-range
   :process-question (fn [args]
     (count-in))
   :required-questions ["poking"]
   :full-name "All matches"
   })

; run jerb every 5 minutes
(defn proc-delay-ms [] (* 1000 * 60 * 5))

(declare get-map-reduce-job)

(defn process-questions []
  (let [max-units 128
        job (get-map-reduce-job max-units)]
    (advance-map-reduce-job job max-units)))

(defn schedule-question-processing []
  (pulsar/spawn-fiber process-questions))

(defn main-job []
  (while true
    (schedule-question-processing)
    (pulsar/sleep (proc-delay-ms))))

(defn gen-jobs []
  (fn [] (pulsar/spawn-fiber main-job)))

(def ^:dynamic *all-filters-lol*
  [(main-filter)])

(defn lol-filters [] *all-filters-lol*)

(defn lol-generate-global-question-key [the-id]
  ["lol" "glob-question" the-id])

(defn lol-generate-global-question-count []
  ["lol" "glob-question-count" "count"])

(defn lol-generate-global-question-proc []
  ["lol" "glob-question-count" "proc"])

(defn lol-generate-most-popular-matchups []
  ["lol" "glob-question-count" "most-popular"])

(defn lol-generate-matchup-question-count
  "Matchup pair - {:user 7 :opponent 7}"
  [matchup-pair]
  ["lol" "matchup-question-count"
   (clojure.string/join "-" (pair-vec matchup-pair))])

(defn lol-generate-matchup-question-id
  "Generate question id"
  [matchup-pair id]
  ["lol" "matchup-questions"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair) id]))])

(defn lol-matchup-pair-from-key [the-key]
  (let [arr (re-find #"(\d+)-(\d+)-" (nth the-key 2))]
    {:user (Integer. (nth arr 1))
     :opponent (Integer. (nth arr 2))}))

(defn lol-generate-matchup-comment-count
  "Matchup pair - {:user 7 :opponent 7}"
  [matchup-pair]
  ["lol" "matchup-comment-count"
   (clojure.string/join "-" (pair-vec matchup-pair))])

(defn lol-generate-matchup-comment-id
  "Generate question id"
  [matchup-pair id]
  ["lol" "matchup-comments"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair) id]))])

(defn lol-generate-question-first-time-key [the-id]
  ["lol" "question-first-time" the-id])

(defn lol-generate-filter-matchup-question-count
  "Generate matchup question count for question
  And filter."
  [matchup-pair question-id filter-id]
  ["lol" "question-matchup-filter-count"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair)
               question-id filter-id]))])

(defn generate-matchup-tasks [the-pair]
  (into []
    (filter some?
      (for [question (questions-full)
            flt (filters-full)]
        (let [first-occ (get-question-first-time (:id question))]
          (if first-occ {:save-key-func
             (lol-generate-filter-matchup-question-count
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
         }))))))

(defn matchup-pair-map-reduce-job [the-pair]
  {:count-key (lol-generate-matchup-question-count the-pair)
   :id-key-function (partial lol-generate-matchup-question-id the-pair)
   :is-nipped true
   :tasks (generate-matchup-tasks the-pair)})

(defn process-matchup-pair [pair max-chunk]
  (let [the-job (matchup-pair-map-reduce-job pair)]
    (advance-map-reduce-job the-job max-chunk)))

(defn most-popular-question-sort [the-arr]
  (sort-by #(if %1 (:count %1) 0) > the-arr))

(defn drop-tail-from-key [the-key]
  (clojure.string/replace the-key #"^(\d+)-(\d+)-.*$" "$1-$2"))

(defn distinct-java-array [the-arr]
  (let [dist (into [] (distinct (vec the-arr)))
        dcount (count dist)]
    (dotimes [n (count the-arr)]
      (let [to-set (if (< n dcount)
                     (nth dist n) nil)]
        (aset the-arr n to-set))))
  the-arr)

(defn gen-map-reduce-tasks-global [max-proc]
  [{:save-key-func (lol-generate-global-question-proc)
    :map-function lol-matchup-pair-from-key
    :initial-reduce (fn [the-val]
                      (java.util.ArrayList.))
    :final-reduce (fn [the-val] (let [dist (distinct the-val)]
                                  (doseq [i dist]
                                    (process-matchup-pair i max-proc)))
                    nil)
    :reduce-function (fn [the-val mapped]
                       (.add the-val mapped)
                       the-val)
    }
    {:save-key-func (lol-generate-most-popular-matchups)
     :map-function (fn [the-val]
                     (let [matchup (matchup-pair-from-key the-val)
                           qcount (get-matchup-question-count matchup)]
                     {:count qcount
                      :key (drop-tail-from-key (nth the-val 2))}))
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
    }
   ])

(defn get-map-reduce-job [max-proc]
  {:count-key (lol-generate-global-question-count)
   :id-key-function lol-generate-global-question-key
   :is-nipped true
   :tasks (gen-map-reduce-tasks-global max-proc)})

(defmacro all-questions-lol []
  (questions-m
    ("mtype" "Match solo queue or ranked?"
           "Solo" "Ranked")
    ("ladder" "What is your ranking?"
              "Bronze" "Silver" "Gold" "Platinum"
              "Diamond" "Master" "Challenger")
    ("poking" "I got poked a lot" "yes" "no")
    ("poking-o" "I poked my opponent a lot" "yes" "no")
    ("sustain" "I could not sustain well" "yes" "no")
    ("sustain-o" "My opponent could not sustain me" "yes" "no")
    ("first-blood" "I got first blood" "yes" "no")
    ("cc" "I got a lot of cc (stuns/slows)" "yes" "no")
    ("gank" "I was ganked quite often" "yes" "no")
    ("last-hit" "I had trouble last hitting minions" "yes" "no")
    ))

(defn hero-squares-lol []
  [
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/c/cc/AatroxSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/1/18/AhriSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/a/a5/AkaliSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/3/34/AlistarSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/2/26/AmumuSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/0/01/AniviaSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/6f/AnnieSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/1/1a/AoShinSquare.jpg"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/4/4a/AsheSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/f/f7/AzirSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/69/BardSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/2/2e/BeatriceSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/5/5b/BlitzcrankSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/a/ab/BrandSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/b/b6/BraumSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/6/6c/BristleSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/e/e6/CaitlynSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/c/ca/CassiopeiaSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/c/cc/ChoGathSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/3/3d/CorkiSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/5/54/DariusSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/9/90/DianaSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/d/d7/DravenSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/b/b0/DrMundoSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/e/ef/EkkoSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/9/91/EliseSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/5/5b/EvelynnSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/c/c3/EzrealSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/7/7c/FiddlesticksSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/d/d2/FioraSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/d/db/FizzSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/4/40/GalioSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/f/fe/GangplankSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/e/ea/GarenSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/6b/GnarSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/6/67/GragasSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/2/26/GravesSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/4/4c/HecarimSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/58/HeimerdingerSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/a/a7/IllaoiSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/7/72/IreliaSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/3/3f/JannaSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/7/72/JarvanIVSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/0/0f/JaxSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/a/aa/JayceSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/8/8b/JhinSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/e/e2/JinxSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/a/aa/KalistaSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/4/4f/KarmaSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/e/e1/KarthusSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/57/KassadinSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/a/ae/KatarinaSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/b/bd/KayleSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/6/69/KennenSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/b/b5/KhaZixSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/6e/KindredSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/e/ed/KogMawSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/f/f1/LeBlancSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/b/bf/LeeSinSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/54/LeonaSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/3/36/LissandraSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/1/1e/LucianSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/9/91/LuluSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/0/01/LuxSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/6f/MalphiteSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/a/a7/MalzaharSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/6/64/MaokaiSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/f/fc/MasterYiSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/a/a2/Mega_GnarSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/9/9d/MissFortuneSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/4/4d/MordekaiserSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/d/d8/MorganaSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/d/d2/NamiSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/5/58/NasusSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/b/b5/NautilusSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/7/7c/NidaleeSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/65/NocturneSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/e/ef/NunuSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/2/2b/OlafSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/b/b0/OriannaSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/9/9b/PantheonSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/e/e3/PoppySquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/7/7c/QuinnSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/7/7c/RammusSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/3/3e/Rek%27SaiSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/f/fc/RenektonSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/f/f1/RengarSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/4/46/RivenSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/1/13/RumbleSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/2/28/RyzeSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/9/93/SejuaniSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/9/93/ShacoSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/d/d3/ShenSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/f/f6/ShyvanaSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/9/96/SingedSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/6/61/SionSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/e/e1/SivirSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/8/80/SkarnerSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/56/SonaSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/8/8d/SorakaSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/f/f2/SpiritGuardUdyrSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/8/8c/SwainSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/6/65/SyndraSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/0/03/Tahm_KenchSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/f/f9/TalonSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/c/c4/TaricSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/0/04/TeemoSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/7/7b/ThreshSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/0/06/TristanaSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/c/c4/TrundleSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/5/5f/TryndamereSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/1/1f/TwistedFateSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/7/79/TwitchSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/d/d1/UdyrSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/53/UrfSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/8/88/UrgotSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/5/58/ValorSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/c/c2/VarusSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/9/95/VayneSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/8/8f/VeigarSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/0/05/Vel%27KozSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/a/a3/ViktorSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/c/c0/ViSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/7/75/VladimirSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/f/f9/VolibearSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/4/42/WarwickSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/e/ef/WillumpSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/6/6d/WukongSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/7/7a/XerathSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/b/bf/XinZhaoSquare.png"
  "http://vignette4.wikia.nocookie.net/leagueoflegends/images/c/c0/YasuoSquare.png"
  "http://vignette1.wikia.nocookie.net/leagueoflegends/images/d/d8/YorickSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/8/81/ZacSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/b/bb/ZedSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/5/55/ZiggsSquare.png"
  "http://vignette2.wikia.nocookie.net/leagueoflegends/images/a/ac/ZileanSquare.png"
  "http://vignette3.wikia.nocookie.net/leagueoflegends/images/1/1a/ZyraSquare.png"
]
)

(defn lowercase-replace [the-link]
  (clojure.string/replace (clojure.string/lower-case the-link)
                          "%27" ""))

(defn square-for-hero [hero-full-name]
  (let [idx (hero-name-full-to-short hero-full-name)]
    (->> (hero-squares-lol)
         (map-indexed #(vector %1 (lowercase-replace %2) %2))
         (map #(hash-map :idx (nth %1 0)
                         :rgx (re-find (re-pattern idx) (nth %1 1))
                         :full (nth %1 2)))
         (filter #(some? (:rgx %1)))
         (first)
         (:full))))

(defn all-heroes-lol []
  [
  "Aatrox"
  "Ahri"
  "Akali"
  "Alistar"
  "Amumu"
  "Anivia"
  "Annie"
  "Ashe"
  "Azir"
  "Bard"
  "Blitzcrank"
  "Brand"
  "Braum"
  "Caitlyn"
  "Cassiopeia"
  "Cho'Gath"
  "Corki"
  "Darius"
  "Diana"
  "Dr. Mundo"
  "Draven"
  "Ekko"
  "Elise"
  "Evelynn"
  "Ezreal"
  "Fiddlesticks"
  "Fiora"
  "Fizz"
  "Galio"
  "Gangplank"
  "Garen"
  "Gnar"
  "Gragas"
  "Graves"
  "Hecarim"
  "Heimerdinger"
  "Illaoi"
  "Irelia"
  "Janna"
  "Jarvan IV"
  "Jax"
  "Jayce"
  "Jinx"
  "Kalista"
  "Karma"
  "Karthus"
  "Kassadin"
  "Katarina"
  "Kayle"
  "Kennen"
  "Kha'Zix"
  "Kindred"
  "Kog'Maw"
  "LeBlanc"
  "Lee Sin"
  "Leona"
  "Lissandra"
  "Lucian"
  "Lulu"
  "Lux"
  "Malphite"
  "Malzahar"
  "Maokai"
  "Master Yi"
  "Miss Fortune"
  "Mordekaiser"
  "Morgana"
  "Nami"
  "Nasus"
  "Nautilus"
  "Nidalee"
  "Nocturne"
  "Nunu"
  "Olaf"
  "Orianna"
  "Pantheon"
  "Poppy"
  "Quinn"
  "Rammus"
  "Rek'Sai"
  "Renekton"
  "Rengar"
  "Riven"
  "Rumble"
  "Ryze"
  "Sejuani"
  "Shaco"
  "Shen"
  "Shyvana"
  "Singed"
  "Sion"
  "Sivir"
  "Skarner"
  "Sona"
  "Soraka"
  "Swain"
  "Syndra"
  "Tahm Kench"
  "Talon"
  "Taric"
  "Teemo"
  "Thresh"
  "Tristana"
  "Trundle"
  "Tryndamere"
  "Twisted Fate"
  "Twitch"
  "Udyr"
  "Urgot"
  "Varus"
  "Vayne"
  "Veigar"
  "Vel'Koz"
  "Vi"
  "Viktor"
  "Vladimir"
  "Volibear"
  "Warwick"
  "Wukong"
  "Xerath"
  "Xin Zhao"
  "Yasuo"
  "Yorick"
  "Zac"
  "Zed"
  "Ziggs"
  "Zilean"
  "Zyra"
  ]
)

(defn squares-for-heroes []
  (into [] (map square-for-hero (all-heroes-lol))))

(def ^:dynamic *all-questions-lol*
  (all-questions-lol))

(def ^:dynamic *shortname-to-index-question-lol*
  (question-index-shortnames *all-questions-lol*))

(def ^:dynamic *hh-context-lol*
  {
   :dbinfo {
     :get-key stor/get-key
     :set-key stor/set-key
     :set-if-not-exists the-hero-hammer.storage/set-if-not-exists
   }
   :filters {
     :full *all-filters-lol*
   }
   :queries {
     :glob-question-count lol-generate-global-question-count
     :glob-question-proc lol-generate-global-question-proc
     :glob-question-id lol-generate-global-question-key
     :question-first-time lol-generate-question-first-time-key
     :matchup-question-count lol-generate-matchup-question-count
     :matchup-question-id lol-generate-matchup-question-id
     :matchup-comment-count lol-generate-matchup-comment-count
     :matchup-comment-id lol-generate-matchup-comment-id
     :matchup-filter-count lol-generate-filter-matchup-question-count
     :matchup-most-popular-global lol-generate-most-popular-matchups
   }
   :questions {
     :full *all-questions-lol*
     :short-to-index *shortname-to-index-question-lol*
     :cross-question-filter
       (cross-questions-and-filters
         *all-questions-lol* *all-filters-lol*)
   }
   :heroes {
     :full (all-heroes-lol)
     :short-to-index (heroes-full-to-short (all-heroes-lol))
     :shortnames (heroes-shortnames (all-heroes-lol))
     :squares (squares-for-heroes)
   }
   :util {
     :matchup-pair-from-key lol-matchup-pair-from-key
   }
   :jobs (gen-jobs)
  })

