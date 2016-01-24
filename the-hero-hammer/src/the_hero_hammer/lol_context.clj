(ns the-hero-hammer.lol_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [taoensso.nippy :as nippy]))

(defn main-filter []
  {:id 0
   :expected max-available-range
   :process-question (fn [curr-id rec]
     (cond
       (in-range curr-id rec) (ignore)
       (ahead-of-range curr-id rec) (drop-out)
       (end-of-range curr-id rec) (count-in)))
   :required-questions ["poking"]
   })

(defn lol-filters []
  [(main-filter)])

(defn lol-generate-global-question-key [the-id]
  ["lol" "glob-question" the-id])

(defn lol-generate-global-question-count []
  ["lol" "glob-question-count" "count"])

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

(def ^:dynamic *hh-context-lol*
  {
   :dbinfo {
     :get-key the-hero-hammer.storage/get-key
     :set-key the-hero-hammer.storage/set-key
     :set-if-not-exists the-hero-hammer.storage/set-if-not-exists
   }
   :filters (lol-filters)
   :queries {
     :glob-question-count lol-generate-global-question-count
     :glob-question-id lol-generate-global-question-key
     :question-first-time lol-generate-question-first-time-key
     :matchup-question-count lol-generate-matchup-question-count
     :matchup-question-id lol-generate-matchup-question-id
     :matchup-comment-count lol-generate-matchup-comment-count
     :matchup-comment-id lol-generate-matchup-comment-id
   }
   :questions {
     :full (all-questions-lol)
   }
   :heroes {
     :full (all-heroes-lol)
     :short-to-index (heroes-full-to-short (all-heroes-lol))
     :shortnames (heroes-shortnames (all-heroes-lol))
   }
  })

