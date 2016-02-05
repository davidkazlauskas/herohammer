(ns the-hero-hammer.dota_context
  (:require [the-hero-hammer.questions :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
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

(defn full-data-dota []
  (into [] (sort-by :name [
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
  ]))
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
   :jobs (scon/gen-jobs (get-map-reduce-job 128) (scon/n-min 5))
  })

