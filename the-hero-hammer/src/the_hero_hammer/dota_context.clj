(ns the-hero-hammer.dota_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.storage :as stor]
            [the-hero-hammer.shared_context :as scon]
            [taoensso.nippy :as nippy]
            [co.paralleluniverse.pulsar.core :as pulsar]))

(defn main-filter []
  {:id 0
   :expected max-available-range
   :process-question (fn [args]
     (count-in))
   :full-name "All matches"
   })

(def ^:dynamic *all-filters-dota*
  [(main-filter)])

(defn dota-filters [] *all-filters-dota*)

(defn dota-generate-global-question-key [the-id]
  ["dota" "glob-question" the-id])

(defn dota-generate-global-question-count []
  ["dota" "glob-question-count" "count"])

(defn dota-generate-global-question-proc []
  ["dota" "glob-question-count" "proc"])

(defn dota-generate-most-popular-matchups []
  ["dota" "glob-question-count" "most-popular"])

(defn dota-generate-matchup-question-count
  "Matchup pair - {:user 7 :opponent 7}"
  [matchup-pair]
  ["dota" "matchup-question-count"
   (clojure.string/join "-" (pair-vec matchup-pair))])

(defn dota-generate-matchup-question-id
  "Generate question id"
  [matchup-pair id]
  ["dota" "matchup-questions"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair) id]))])

(defn dota-matchup-pair-from-key [the-key]
  (let [arr (re-find #"(\d+)-(\d+)-" (nth the-key 2))]
    {:user (Integer. (nth arr 1))
     :opponent (Integer. (nth arr 2))}))

(defn dota-generate-matchup-comment-count
  "Matchup pair - {:user 7 :opponent 7}"
  [matchup-pair]
  ["dota" "matchup-comment-count"
   (clojure.string/join "-" (pair-vec matchup-pair))])

(defn dota-generate-matchup-comment-id
  "Generate question id"
  [matchup-pair id]
  ["dota" "matchup-comments"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair) id]))])

(defn dota-generate-question-first-time-key [the-id]
  ["dota" "question-first-time" the-id])

(defn dota-generate-filter-matchup-question-count
  "Generate matchup question count for question
  And filter."
  [matchup-pair question-id filter-id]
  ["dota" "question-matchup-filter-count"
   (clojure.string/join "-"
     (flatten [(pair-vec matchup-pair)
               question-id filter-id]))])

(defn drop-tail-from-key [the-key]
  (clojure.string/replace (nth the-key 2) #"^(\d+)-(\d+)-.*$" "$1-$2"))

(defn gen-map-reduce-tasks-global [max-proc]
  (let [dota-args
        {; generic
         :glob-question-key (dota-generate-global-question-proc)
         :id-key-gen dota-matchup-pair-from-key
         :max-proc 128
         :count-key-func dota-generate-matchup-question-count
         :generate-matchup-question-id dota-generate-matchup-question-id
         :generate-filter-matchup-question-count dota-generate-filter-matchup-question-count

         ; most popular
         :most-popular-matchups-key (dota-generate-most-popular-matchups)
         :turn-key-to-uniq-matchup drop-tail-from-key
         }]
      ;:glob-question-key -> global key for questions (to db)
      ;:id-key-gen -> function with 1 arg (id) to get glob question id
      ;:max-proc -> maximum questions to process at a time
      ;:count-key-func -> function to generate count from pair
      ;:generate-matchup-question-id -> function to generate matchup question id
      ;  takes 2 args, pair + filter
      ;"Argmap:
      ;:generate-filter-matchup-question-count ->
        ;generate question count key for filter and matchup
      ;"
    [; generic processing
     (scon/generic-processing-job dota-args)
     ; most popular questions
     (scon/most-popular-matchups-proc-job dota-args)
     ]))

(defn get-map-reduce-job [max-proc]
  {:count-key (dota-generate-global-question-count)
   :id-key-function dota-generate-global-question-key
   :is-nipped true
   :tasks (gen-map-reduce-tasks-global max-proc)})

(defn process-questions []
  (scon/process-questions (get-map-reduce-job 128)))

(defmacro all-questions-dota []
  (questions-m
    ("end-res" "Won or lost?"
           "Won" "Lost")
    ("mtype" "Match solo queue or ranked?"
           "Solo" "Ranked")
    ("ladder" "What is your ranking?"
              "Bronze" "Silver" "Gold" "Platinum"
              "Diamond" "Master" "Challenger")
    ("position" "What was your position?"
                "Top" "Mid" "ADC" "Support" "Jungler")
    ("poking" "I got poked a lot" "yes" "no")
    ("poking-o" "I poked my opponent a lot" "yes" "no")
    ("sustain" "I could not sustain well" "yes" "no")
    ("sustain-o" "My opponent could not sustain me" "yes" "no")
    ("first-blood" "I got first blood" "yes" "no")
    ("lane-to-me" "Lane was often pushed up to me" "yes" "no")
    ("lane-to-opp" "I often pushed lane to opponent" "yes" "no")
    ("cc" "I got a lot of cc (stuns/slows)" "yes" "no")
    ("gank" "I was ganked quite often" "yes" "no")
    ("last-hit" "I had trouble last hitting minions" "yes" "no")
    ("aggro-me" "I could play very aggresively against my opponent" "yes" "no")
    ("with-team" "I needed team before I could make some moves" "yes" "no")
    ("strong-alone" "I felt very strong alone" "yes" "no")
    ("must-def" "I had no choice but to play defensively" "yes" "no")
    ("carried" "I carried the game" "yes" "no")
    ("retards" "My team was full of retards" "yes" "no")
    ("fed-me" "I got pretty fed" "yes" "no")
    ("ganked-payback" "If I got ganked at least I could kill someone before death" "yes" "no")
    ("wave-clear" "Clearing waves was a breeze" "yes" "no")
    ))

(defn hero-squares-dota []
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
    (->> (hero-squares-dota)
         (map-indexed #(vector %1 (lowercase-replace %2) %2))
         (map #(hash-map :idx (nth %1 0)
                         :rgx (re-find (re-pattern idx) (nth %1 1))
                         :full (nth %1 2)))
         (filter #(some? (:rgx %1)))
         (first)
         (:full))))

(defn full-data-dota []
  [
  {:name "Earthshaker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/earthshaker_vert.jpg"}
  {:name "Sven" :image "http://cdn.dota2.com/apps/dota2/images/heroes/sven_vert.jpg"}
  {:name "Tiny" :image "http://cdn.dota2.com/apps/dota2/images/heroes/tiny_vert.jpg"}
  {:name "Kunkka" :image "http://cdn.dota2.com/apps/dota2/images/heroes/kunkka_vert.jpg"}
  {:name "Beastmaster" :image "http://cdn.dota2.com/apps/dota2/images/heroes/beastmaster_vert.jpg"}
  {:name "Dragon Knight" :image "http://cdn.dota2.com/apps/dota2/images/heroes/dragon_knight_vert.jpg"}
  {:name "Clockwerk" :image "http://cdn.dota2.com/apps/dota2/images/heroes/rattletrap_vert.jpg"}
  {:name "Omniknight" :image "http://cdn.dota2.com/apps/dota2/images/heroes/omniknight_vert.jpg"}
  {:name "Huskar" :image "http://cdn.dota2.com/apps/dota2/images/heroes/huskar_vert.jpg"}
  {:name "Alchemist" :image "http://cdn.dota2.com/apps/dota2/images/heroes/alchemist_vert.jpg"}
  {:name "Brewmaster" :image "http://cdn.dota2.com/apps/dota2/images/heroes/brewmaster_vert.jpg"}
  {:name "Treant Protector" :image "http://cdn.dota2.com/apps/dota2/images/heroes/treant_vert.jpg"}
  {:name "Io" :image "http://cdn.dota2.com/apps/dota2/images/heroes/wisp_vert.jpg"}
  {:name "Centaur Warrunner" :image "http://cdn.dota2.com/apps/dota2/images/heroes/centaur_vert.jpg"}
  {:name "Timbersaw" :image "http://cdn.dota2.com/apps/dota2/images/heroes/shredder_vert.jpg"}
  {:name "Bristleback" :image "http://cdn.dota2.com/apps/dota2/images/heroes/bristleback_vert.jpg"}
  {:name "Tusk" :image "http://cdn.dota2.com/apps/dota2/images/heroes/tusk_vert.jpg"}
  {:name "Elder Titan" :image "http://cdn.dota2.com/apps/dota2/images/heroes/elder_titan_vert.jpg"}
  {:name "Legion Commander" :image "http://cdn.dota2.com/apps/dota2/images/heroes/legion_commander_vert.jpg"}
  {:name "Earth Spirit" :image "http://cdn.dota2.com/apps/dota2/images/heroes/earth_spirit_vert.jpg"}
  {:name "Phoenix" :image "http://cdn.dota2.com/apps/dota2/images/heroes/phoenix_vert.jpg"}
  {:name "Anti-Mage" :image "http://cdn.dota2.com/apps/dota2/images/heroes/antimage_vert.jpg"}
  {:name "Drow Ranger" :image "http://cdn.dota2.com/apps/dota2/images/heroes/drow_ranger_vert.jpg"}
  {:name "Juggernaut" :image "http://cdn.dota2.com/apps/dota2/images/heroes/juggernaut_vert.jpg"}
  {:name "Mirana" :image "http://cdn.dota2.com/apps/dota2/images/heroes/mirana_vert.jpg"}
  {:name "Morphling" :image "http://cdn.dota2.com/apps/dota2/images/heroes/morphling_vert.jpg"}
  {:name "Phantom Lancer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/phantom_lancer_vert.jpg"}
  {:name "Vengeful Spirit" :image "http://cdn.dota2.com/apps/dota2/images/heroes/vengefulspirit_vert.jpg"}
  {:name "Riki" :image "http://cdn.dota2.com/apps/dota2/images/heroes/riki_vert.jpg"}
  {:name "Sniper" :image "http://cdn.dota2.com/apps/dota2/images/heroes/sniper_vert.jpg"}
  {:name "Templar Assassin" :image "http://cdn.dota2.com/apps/dota2/images/heroes/templar_assassin_vert.jpg"}
  {:name "Luna" :image "http://cdn.dota2.com/apps/dota2/images/heroes/luna_vert.jpg"}
  {:name "Bounty Hunter" :image "http://cdn.dota2.com/apps/dota2/images/heroes/bounty_hunter_vert.jpg"}
  {:name "Ursa" :image "http://cdn.dota2.com/apps/dota2/images/heroes/ursa_vert.jpg"}
  {:name "Gyrocopter" :image "http://cdn.dota2.com/apps/dota2/images/heroes/gyrocopter_vert.jpg"}
  {:name "Lone Druid" :image "http://cdn.dota2.com/apps/dota2/images/heroes/lone_druid_vert.jpg"}
  {:name "Naga Siren" :image "http://cdn.dota2.com/apps/dota2/images/heroes/naga_siren_vert.jpg"}
  {:name "Troll Warlord" :image "http://cdn.dota2.com/apps/dota2/images/heroes/troll_warlord_vert.jpg"}
  {:name "Ember Spirit" :image "http://cdn.dota2.com/apps/dota2/images/heroes/ember_spirit_vert.jpg"}
  {:name "Arc Warden" :image "http://cdn.dota2.com/apps/dota2/images/heroes/arc_warden_vert.jpg"}
  {:name "Crystal Maiden" :image "http://cdn.dota2.com/apps/dota2/images/heroes/crystal_maiden_vert.jpg"}
  {:name "Puck" :image "http://cdn.dota2.com/apps/dota2/images/heroes/puck_vert.jpg"}
  {:name "Storm Spirit" :image "http://cdn.dota2.com/apps/dota2/images/heroes/storm_spirit_vert.jpg"}
  {:name "Windranger" :image "http://cdn.dota2.com/apps/dota2/images/heroes/windrunner_vert.jpg"}
  {:name "Zeus" :image "http://cdn.dota2.com/apps/dota2/images/heroes/zuus_vert.jpg"}
  {:name "Lina" :image "http://cdn.dota2.com/apps/dota2/images/heroes/lina_vert.jpg"}
  {:name "Shadow Shaman" :image "http://cdn.dota2.com/apps/dota2/images/heroes/shadow_shaman_vert.jpg"}
  {:name "Tinker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/tinker_vert.jpg"}
  {:name "Natures Prophet" :image "http://cdn.dota2.com/apps/dota2/images/heroes/furion_vert.jpg"}
  {:name "Enchantress" :image "http://cdn.dota2.com/apps/dota2/images/heroes/enchantress_vert.jpg"}
  {:name "Jakiro" :image "http://cdn.dota2.com/apps/dota2/images/heroes/jakiro_vert.jpg"}
  {:name "Chen" :image "http://cdn.dota2.com/apps/dota2/images/heroes/chen_vert.jpg"}
  {:name "Silencer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/silencer_vert.jpg"}
  {:name "Ogre Magi" :image "http://cdn.dota2.com/apps/dota2/images/heroes/ogre_magi_vert.jpg"}
  {:name "Rubick" :image "http://cdn.dota2.com/apps/dota2/images/heroes/rubick_vert.jpg"}
  {:name "Disruptor" :image "http://cdn.dota2.com/apps/dota2/images/heroes/disruptor_vert.jpg"}
  {:name "Keeper of the Light" :image "http://cdn.dota2.com/apps/dota2/images/heroes/keeper_of_the_light_vert.jpg"}
  {:name "Skywrath Mage" :image "http://cdn.dota2.com/apps/dota2/images/heroes/skywrath_mage_vert.jpg"}
  {:name "Oracle" :image "http://cdn.dota2.com/apps/dota2/images/heroes/oracle_vert.jpg"}
  {:name "Techies" :image "http://cdn.dota2.com/apps/dota2/images/heroes/techies_vert.jpg"}
  {:name "Axe" :image "http://cdn.dota2.com/apps/dota2/images/heroes/axe_vert.jpg"}
  {:name "Pudge" :image "http://cdn.dota2.com/apps/dota2/images/heroes/pudge_vert.jpg"}
  {:name "Sand King" :image "http://cdn.dota2.com/apps/dota2/images/heroes/sand_king_vert.jpg"}
  {:name "Slardar" :image "http://cdn.dota2.com/apps/dota2/images/heroes/slardar_vert.jpg"}
  {:name "Tidehunter" :image "http://cdn.dota2.com/apps/dota2/images/heroes/tidehunter_vert.jpg"}
  {:name "Wraith King" :image "http://cdn.dota2.com/apps/dota2/images/heroes/skeleton_king_vert.jpg"}
  {:name "Lifestealer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/life_stealer_vert.jpg"}
  {:name "Night Stalker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/night_stalker_vert.jpg"}
  {:name "Doom" :image "http://cdn.dota2.com/apps/dota2/images/heroes/doom_bringer_vert.jpg"}
  {:name "Spirit Breaker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/spirit_breaker_vert.jpg"}
  {:name "Lycan" :image "http://cdn.dota2.com/apps/dota2/images/heroes/lycan_vert.jpg"}
  {:name "Chaos Knight" :image "http://cdn.dota2.com/apps/dota2/images/heroes/chaos_knight_vert.jpg"}
  {:name "Undying" :image "http://cdn.dota2.com/apps/dota2/images/heroes/undying_vert.jpg"}
  {:name "Magnus" :image "http://cdn.dota2.com/apps/dota2/images/heroes/magnataur_vert.jpg"}
  {:name "Abaddon" :image "http://cdn.dota2.com/apps/dota2/images/heroes/abaddon_vert.jpg"}
  {:name "Bloodseeker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/bloodseeker_vert.jpg"}
  {:name "Shadow Fiend" :image "http://cdn.dota2.com/apps/dota2/images/heroes/nevermore_vert.jpg"}
  {:name "Razor" :image "http://cdn.dota2.com/apps/dota2/images/heroes/razor_vert.jpg"}
  {:name "Venomancer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/venomancer_vert.jpg"}
  {:name "Faceless Void" :image "http://cdn.dota2.com/apps/dota2/images/heroes/faceless_void_vert.jpg"}
  {:name "Phantom Assassin" :image "http://cdn.dota2.com/apps/dota2/images/heroes/phantom_assassin_vert.jpg"}
  {:name "Viper" :image "http://cdn.dota2.com/apps/dota2/images/heroes/viper_vert.jpg"}
  {:name "Clinkz" :image "http://cdn.dota2.com/apps/dota2/images/heroes/clinkz_vert.jpg"}
  {:name "Broodmother" :image "http://cdn.dota2.com/apps/dota2/images/heroes/broodmother_vert.jpg"}
  {:name "Weaver" :image "http://cdn.dota2.com/apps/dota2/images/heroes/weaver_vert.jpg"}
  {:name "Spectre" :image "http://cdn.dota2.com/apps/dota2/images/heroes/spectre_vert.jpg"}
  {:name "Meepo" :image "http://cdn.dota2.com/apps/dota2/images/heroes/meepo_vert.jpg"}
  {:name "Nyx Assassin" :image "http://cdn.dota2.com/apps/dota2/images/heroes/nyx_assassin_vert.jpg"}
  {:name "Slark" :image "http://cdn.dota2.com/apps/dota2/images/heroes/slark_vert.jpg"}
  {:name "Medusa" :image "http://cdn.dota2.com/apps/dota2/images/heroes/medusa_vert.jpg"}
  {:name "Terrorblade" :image "http://cdn.dota2.com/apps/dota2/images/heroes/terrorblade_vert.jpg"}
  {:name "Bane" :image "http://cdn.dota2.com/apps/dota2/images/heroes/bane_vert.jpg"}
  {:name "Lich" :image "http://cdn.dota2.com/apps/dota2/images/heroes/lich_vert.jpg"}
  {:name "Lion" :image "http://cdn.dota2.com/apps/dota2/images/heroes/lion_vert.jpg"}
  {:name "Witch Doctor" :image "http://cdn.dota2.com/apps/dota2/images/heroes/witch_doctor_vert.jpg"}
  {:name "Enigma" :image "http://cdn.dota2.com/apps/dota2/images/heroes/enigma_vert.jpg"}
  {:name "Necrophos" :image "http://cdn.dota2.com/apps/dota2/images/heroes/necrolyte_vert.jpg"}
  {:name "Warlock" :image "http://cdn.dota2.com/apps/dota2/images/heroes/warlock_vert.jpg"}
  {:name "Queen of Pain" :image "http://cdn.dota2.com/apps/dota2/images/heroes/queenofpain_vert.jpg"}
  {:name "Death Prophet" :image "http://cdn.dota2.com/apps/dota2/images/heroes/death_prophet_vert.jpg"}
  {:name "Pugna" :image "http://cdn.dota2.com/apps/dota2/images/heroes/pugna_vert.jpg"}
  {:name "Dazzle" :image "http://cdn.dota2.com/apps/dota2/images/heroes/dazzle_vert.jpg"}
  {:name "Leshrac" :image "http://cdn.dota2.com/apps/dota2/images/heroes/leshrac_vert.jpg"}
  {:name "Dark Seer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/dark_seer_vert.jpg"}
  {:name "Batrider" :image "http://cdn.dota2.com/apps/dota2/images/heroes/batrider_vert.jpg"}
  {:name "Ancient Apparition" :image "http://cdn.dota2.com/apps/dota2/images/heroes/ancient_apparition_vert.jpg"}
  {:name "Invoker" :image "http://cdn.dota2.com/apps/dota2/images/heroes/invoker_vert.jpg"}
  {:name "Outworld Devourer" :image "http://cdn.dota2.com/apps/dota2/images/heroes/obsidian_destroyer_vert.jpg"}
  {:name "Shadow Demon" :image "http://cdn.dota2.com/apps/dota2/images/heroes/shadow_demon_vert.jpg"}
  {:name "Visage" :image "http://cdn.dota2.com/apps/dota2/images/heroes/visage_vert.jpg"}
  {:name "Winter Wyvern" :image "http://cdn.dota2.com/apps/dota2/images/heroes/winter_wyvern_vert.jpg"}
  ]
  )

(defn all-heroes-dota []
  (mapv :name (full-data-dota)))

(defn squares-for-heroes []
  (mapv :image (full-data-dota)))

(def ^:dynamic *all-questions-dota*
  (all-questions-dota))

(def ^:dynamic *shortname-to-index-question-dota*
  (question-index-shortnames *all-questions-dota*))

(def ^:dynamic *hh-context-dota*
  {
   :filters {
     :full *all-filters-dota*
   }
   :queries {
     :glob-question-count dota-generate-global-question-count
     :glob-question-proc dota-generate-global-question-proc
     :glob-question-id dota-generate-global-question-key
     :question-first-time dota-generate-question-first-time-key
     :matchup-question-count dota-generate-matchup-question-count
     :matchup-question-id dota-generate-matchup-question-id
     :matchup-comment-count dota-generate-matchup-comment-count
     :matchup-comment-id dota-generate-matchup-comment-id
     :matchup-filter-count dota-generate-filter-matchup-question-count
     :matchup-most-popular-global dota-generate-most-popular-matchups
   }
   :questions {
     :full *all-questions-dota*
     :short-to-index *shortname-to-index-question-dota*
     :cross-question-filter
       (cross-questions-and-filters
         *all-questions-dota* *all-filters-dota*)
   }
   :heroes {
     :full (all-heroes-dota)
     :short-to-index (heroes-full-to-short (all-heroes-dota))
     :shortnames (heroes-shortnames (all-heroes-dota))
     :squares (squares-for-heroes)
   }
   :util {
     :matchup-pair-from-key dota-matchup-pair-from-key
   }
   :jobs (scon/gen-jobs (get-map-reduce-job 128))
  })

