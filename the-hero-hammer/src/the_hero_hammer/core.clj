(ns the-hero-hammer.core
  (:gen-class)
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.db_context :refer :all]
            [the-hero-hammer.lol_context :as lctx]
            [the-hero-hammer.dota_context :as dctx]
            [the-hero-hammer.storage_ram :as sram]
            [the-hero-hammer.storage_as :as saes]
            [org.httpkit.server :refer [run-server]]
            [cljs.build.api :as cljsbld]
            [clojure.data.json :as json]
            [clj-http.client :as hclient]
            [print.foo :as dp])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.cookies :as cook]))


(def ^:dynamic *html-context* nil)

(defn env-num [var-name]
  (let [prelim (System/getenv var-name)]
    (if prelim (Integer. prelim))))

(defn relevant-storage-ctx []
  (let [use-ram (System/getenv "HH_MOCK_DB")]
    (if use-ram
      (fn [] sram/*storage-ram-context*)
      (fn [] saes/*storage-aes-context*))))

(defmacro lol-ctx [& args]
  `(binding [*ctx-get-func* (fn [] lctx/*hh-context-lol*)
             *get-db-context* (relevant-storage-ctx)
             *html-context* *html-context-lol*]
     ~@args
   ))

(defmacro dota-ctx [& args]
  `(binding [*ctx-get-func* (fn [] dctx/*hh-context-dota*)
             *get-db-context* (relevant-storage-ctx)
             *html-context* *html-context-dota*]
     ~@args
   ))

(defn get-hero-squares []
  (get-in (*ctx-get-func*) [:heroes :squares]))

(defn generate-javascript-hero-squares []
  (let [squares (get-hero-squares)]
    (let [the-arr (->> squares
         (map #(str "'" %1 "'"))
         (interpose ",")
         clojure.string/join)]
      (str "[" the-arr "]"))))

(defn round-percent-ratio [rat]
  (Math/round (* 100 (float rat))))

(defn highest-percent-part [the-vec]
  (let [sum (apply + the-vec)
        divisor (if (= 0 sum) 1 sum)]
    (->> the-vec
         first
         (#(round-percent-ratio (/ %1 divisor))))))

(defmacro recaptcha-post-url [] "https://www.google.com/recaptcha/api/siteverify")
(defmacro my-recaptcha-key [] "6Lc87xYTAAAAAPV2x9CEC8fZ68l_QEh3eYR_Wu5s")
(defmacro my-recaptcha-key-sec [] "6Lc87xYTAAAAAMqxM_wvkgRXzaWsh2RPXU5Fmrc9")

(defmacro ze-analytics []
  (html [:script
    "(function (i,s,o,g,r,a,m) {i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
    "(i[r].q=i[r].q|| []).push (arguments)},i [r].l=1*new Date ();a=s.createElement(o),"
    "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
    "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
    "ga ('create', 'UA-73343353-1', 'auto');"
     "ga ('send', 'pageview');"
    "</script>"]))

(defmacro fb-commentsdk []
  (str "<div id=\"fb-root\"></div>"
    "<script>(function(d, s, id) {"
    "var js, fjs = d.getElementsByTagName(s)[0];"
    "if (d.getElementById(id)) return;"
    "js = d.createElement(s); js.id = id;"
    "js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.5\";"
    "fjs.parentNode.insertBefore(js, fjs);"
    "}(document, 'script', 'facebook-jssdk'));</script>"))

(defmacro email-link []
  (html [:p
         "(If there is something not covered here "
         "feel free to send suggestions "
         [:a {:href "mailto:questions@herohammer.io?Subject=Question%20suggestion"} "to this email"]
         ")"]))

(defn render-recaptcha []
  (html [:div {:class "g-recaptcha"
               :data-sitekey (my-recaptcha-key)}]))

(def ^:dynamic *html-context-lol*
  (lol-ctx
    {:main-page-for-ctx "/lol"
     :main-page-by-hero "/lol-by-hero"
     :main-page-by-opponent "/lol-by-opponent"
     :main-page-by-question "/lol-by-question"
     :question-post-link "/questions-post-lol"
     :question-get-link "/questions-lol"
     :matchup-link-start "/matchup-lol"
     :hero-link-start "/hero-lol"
     :opponent-link-start "/opponent-lol"
     :question-link-start "/question-stats-lol"
     :add-record-link-start "/questions-lol"
     :record-link-start "/show-record-lol"
     :registration-link "/questions-lol"
     :rand-comments-link "/comments-lol/random"
     :recent-comments-link "/comments-lol/recent"
     :current-tab "main"
     :squares-javascript (generate-javascript-hero-squares)
     :question-sort-function
       (fn [the-q]
         (let [sname (:shortname the-q)
               opt-vec (get-in the-q [:answers])]
         (cond (= "end-res" sname) 201
               (= "mtype" sname) 200
               (= "ladder" sname) 199
               (= "position" sname) 198
               :else (highest-percent-part opt-vec))))
     :radio-set
       (into #{} (map #(:shortname %1)
         (questions-full)))
     }))

(def ^:dynamic *html-context-dota*
  (dota-ctx
    {:main-page-for-ctx "/dota"
     :main-page-by-hero "/dota-by-hero"
     :main-page-by-opponent "/dota-by-opponent"
     :main-page-by-question "/dota-by-question"
     :question-post-link "/questions-post-dota"
     :question-get-link "/questions-dota"
     :matchup-link-start "/matchup-dota"
     :hero-link-start "/hero-dota"
     :opponent-link-start "/opponent-dota"
     :question-link-start "/question-stats-dota"
     :add-record-link-start "/questions-dota"
     :record-link-start "/show-record-dota"
     :registration-link "/questions-dota"
     :rand-comments-link "/comments-dota/random"
     :recent-comments-link "/comments-dota/recent"
     :current-tab "main"
     :squares-javascript (generate-javascript-hero-squares)
     :question-sort-function
       (fn [the-q]
         (let [sname (:shortname the-q)
               opt-vec (get-in the-q [:answers])]
         (cond (= "end-res" sname) 201
               (= "mtype" sname) 200
               (= "ladder" sname) 199
               (= "position" sname) 198
               :else (highest-percent-part opt-vec))))
     :radio-set
       (into #{} (map #(:shortname %1)
         (questions-full)))
     }))

(def ^:dynamic *html-context* nil)

(defn html-context []
  *html-context*)

(defmacro rebind-html-context [new-val ops]
  `(binding [*html-context* ~new-val]
     ~ops))

(defn parse-int [s]
   (if s
     (let [try-parse (re-find  #"\d+" s)]
       (if try-parse (Integer. try-parse)))))

(defn main-page-link []
  (:main-page-for-ctx (html-context)))

(defn by-hero-link []
  (:main-page-by-hero (html-context)))

(defn by-opponent-link []
  (:main-page-by-opponent (html-context)))

(defn by-question-link []
  (:main-page-by-question (html-context)))

(defn current-tab []
  (:current-tab (html-context)))

(defn form-to-data [form]
  {
   :hero-user (parse-int (get form "hero-user"))
   :hero-opponent (parse-int (get form "hero-opponent"))
   :comment (get form "user-comment")
   :answers (->> (questions-full)
                 (map :shortname)
                 (map-indexed #(vector %1
                   (parse-int (get form %2))))
                 (filter #(some? (get %1 1)))
                 (into []))
   }
  )

(defn render-question [q form-data]
  (let [shortname (:shortname q)
        sel (get form-data shortname)
        sel-no (if sel (parse-int sel))]
    (html
   [:div {:class "text-center"} [:p (:question q)]
   [:div {:class "form-inline"}
    (map-indexed
      #(let [prelim-attr
             {:style "margin-left: 7px; margin-right: 7px;"
                  :type "radio"
                  :value %1
                  :name shortname}]
          [:label
                [:input
                 (if
                   (not (and sel-no (= sel-no %1)))
                   prelim-attr
                   (assoc prelim-attr :checked "checked"))
                  %2]])
        (:options q))]]
   [:br])))

(defn rand-header []
  (rand-nth
    ["Find out if you're about to get whooped... In seconds!"
     "Where your whining matters!"]))

(defn navbar [args]
  (html [:nav {:class "navbar navbar-default"}
         [:div {:class "container"}
          [:div {:class "navbar-header"}
           [:a {:class "navbar-brand" :href "/"} "The Hero Hammer"]]
          [:div {:class "collapse navbar-collapse"}
           [:ul {:class "nav navbar-nav"}
            [:li [:a {:href "/dota"} "DotA 2"]]
            [:li [:a {:href "/lol"} "League of Legends"]]
            ]
           [:ul {:class "nav navbar-nav navbar-right"}
            [:li [:h4 (rand-header)]]
            ]
           ]
          ]
         ]))

(defn ze-social-share-stuff []
  (html [:script {:type "text/javascript"
                  :src "https://ws.sharethis.com/button/buttons.js"}]
        [:script {:type "text/javascript"}
         (str "stLight.options({publisher: \"93e52f14-0d56-4edf-9dbe-b73a97dfecc6 \", "
         "doNotHash: false, doNotCopy: false, hashAddressBar: false});")]
        ))

(defn q-post-link [] (:question-post-link (html-context)))
(defn q-get-link [] (:question-get-link (html-context)))

(defn default-q-get-link []
  (or (q-get-link) (dota-ctx (q-get-link))))

(defn wrap-html [towrap]
  (html [:html
         [:head
          [:title "The Hero Hammer - where whining becomes useful"]
          [:link {:rel "stylesheet"
                  :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
                  :integrity "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"
                  :crossorigin "anonymous"}]
          [:link {:rel "stylesheet"
                  :href "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.6/simplex/bootstrap.min.css"
                  :crossorigin "anonymous"}]
          [:script {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
                    :integrity "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"
                    :crossorigin "anonymous"}]
          [:script {:src "/resources/js/main.js"}]
          [:script {:src "https://www.google.com/recaptcha/api.js"}]
          (ze-analytics)
          (fb-commentsdk)
          (ze-social-share-stuff)
          ]
         [:body
          (navbar {})
          [:div {:class "container"}
           [:div {:class "row"}
            [:div {:class "col-md-2"}]
            [:div {:class "col-md-8"} towrap]
            [:div {:class "col-md-2"}]
            ]
           [:div {:class "panel"}
            [:div {:style "margin-bottom: 20px;"
                   :class "panel-footer text-center"}
             [:span "The Hero Hammer 2016"]]]
           [:div {:class "navbar navbar-default navbar-fixed-bottom"}
            [:style ".stMainServices {height: 30px !important;}"]
            [:div {:class "container text-center"}
             [:span "Enjoying this site? Consider "
              [:a {:href (default-q-get-link)} "donating a record"]
              " or sharing with others: "]
              [:span {:class "st_facebook_hcount"
                      :displayText "Facebook"}]
              [:span {:class "st_twitter_hcount"
                      :displayText "Twitter"}]
             ]
            ]
           ]]
         ]))

(defn index
  "meow"
  []
  (wrap-html [:div {:class "panel"}
              [:div {:class "panel-default"} [:h1 "Welcome!"
              ]
               [:p {:class "lead"} "This website aims to provide quick "
                "and structured info about what you're about "
                "to experience in the cruel moba world. "
                "Matchup data is collected from users where "
                "they expressed what happened by answering questions "
                "about particular match. Pick the moba you're interested "
                "in, pick heroes and see how it is for particular matchup."
                ]
               [:div {:class "text-center"}
                [:div {:class "btn-group"}
                  [:a {:class "btn btn-default"
                       :style "width: 160px;"
                       :href "/dota"} "DotA 2"]
                  [:a {:class "btn btn-default"
                       :style  "width: 160px;"
                       :href "/lol"} "League of Legends"]
                  ]
                ]
               ]]))

(defn update-hero-squares-js-func []
  "the_hero_hammer.js_client.updateHeroSquares();")

(defn show-10-random-comments-js-func []
  "the_hero_hammer.js_client.show10RandomComments();")

(defn show-10-recent-comments-js-func []
  "the_hero_hammer.js_client.show10RecentComments();")

(defn go-to-gero-stats-js-func []
  "the_hero_hammer.js_client.goToQuestionStats();")

(defn get-radio-set []
  (:radio-set (html-context)))

(defn update-hero-squares-script []
  (html [:script (update-hero-squares-js-func)]))

(defn hero-dropdown [select-id args]
  (let [selected (:selected args)
        style (:style args)]
    (html [:select
           {:class "form-control"
            :id select-id
            :name select-id
            :onchange (update-hero-squares-js-func)
            :style style}
         (map-indexed
           #(do
              (html [:option (if (= selected %1)
                             {:selected "" :value %1}
                             {:value %1}) %2]))
           (heroes-full))
         ])))

(defn filter-dropdown [select-id]
  (html [:select {:class "form-control"
                  :id select-id
                  }
         (map
           #(html [:option
                   {:value (:id %1)}
                   (:full-name %1)
                   ])
           (filters-full))
         ]))

(defmacro question-sign-img []
  "http://res.cloudinary.com/ddclns6x6/image/upload/v1454935454/question-mark_ikjovc.png")

(defn hero-icon [thumb-id src]
  (html [:img {:width 120 :height 120
               :src src
               :style "margin-left: 10px; margin-right: 10px;"
               :id thumb-id}]))

(defn reg-and-show-buttons []
  (html [:div {:style "margin-top: 20px;" :class "text-center"}
         [:div {:class "btn-group"}
           [:a {:onclick "the_hero_hammer.js_client.goToNewRecord();"
                :style "width: 120px;"
                :class "btn btn-default" } "Add record"]
           [:a {:onclick "the_hero_hammer.js_client.goToMatchup();"
                :style "width: 120px;"
                :class "btn btn-default" } "Show results"]]]))

(defn context-js-variables []
  (html [:script "registrationLink = '"
         (:registration-link (html-context))
         "'; matchupLink = '"
         (:matchup-link-start (html-context))
         "'; addRecordLink = '"
         (:add-record-link-start (html-context))
         "'; heroStatsLink = '"
         (:hero-link-start (html-context))
         "'; opponentStatsLink = '"
         (:opponent-link-start (html-context))
         "'; questionStatsLink = '"
         (:question-link-start (html-context))
         "'; heroSquares = "
         (:squares-javascript (html-context))
         ";"]))

(defn render-hero-pair [args]
    (html [:div {:style "padding-top: 20px; padding-bottom: 20px;"
                 :class "text-center"}
           (hero-icon "thumb-user" (:src-user args ""))
           (hero-icon "thumb-opponent" (:src-opp args ""))]))

(defn generate-hero-selection [args]
  (html [:div {:class "text-center"}
         [:label {:style "text-align: left; width: 130px;"
                  :for "hero-user"}
          "Your hero"]
         [:label {:style "text-align: right; width: 130px;"
                  :for "hero-opponent"}
          "Opponent hero"]]
        [:div {:class "form-inline text-center"}
         (hero-dropdown "hero-user"
                        {:selected
                         (:selected-user args)})
         (hero-dropdown "hero-opponent"
                        {:selected
                         (:selected-opponent args)})
         ]
        (render-hero-pair {})
        (update-hero-squares-script)))

(defn single-matchup-listing [the-key index]
  (let [mkey (:key the-key)
        hn-user (:hn-user the-key)
        hn-opp (:hn-opp the-key)
        sq-user (:sq-user the-key)
        sq-opp (:sq-opp the-key)
        link (:link the-key)
        ]
   (html [:tr {:onclick
               (str "window.location = '" link "';")
               :style "cursor: pointer;"
               }
         (if index [:td {:style "width: 30px;"} "#" index])
         [:td {:style "width: 110px;"}
          [:img {:height "32" :width "32" :src sq-user}]
          [:span
           {:style "margin-left: 10px; margin-right: 10px;"}
           "vs."]
          [:img {:height "32" :width "32" :src sq-opp}]]
         [:td {:class "text-center"} [:span
               {:style "font-size: 12px;"} hn-user " vs. " hn-opp]]
         ])))

(defn render-most-popular-generic [the-vec args func-each]
  (let [draw-numbers (:numbers args true)]
   (for [i (range (count the-vec))]
    (html [:table
           {:class
           "table table-hover table-striped table-condensed"}
           (func-each
             (nth the-vec i)
             (if draw-numbers (inc i)))]))))

(defn render-most-popular [the-vec args]
  (render-most-popular-generic
    the-vec args single-matchup-listing))

(defn game-stats-render [context-vars]
  (let [most-pop (:global-most-popular context-vars)
        most-rec (:global-most-recent context-vars)]
    (if most-pop
      (html [:div {:style "margin-top: 20px;"
                   :class "row text-center"}
              [:div {:class "col-md-6"}
               [:h4 "Most popular matchups"]
               (render-most-popular most-pop {})]
              [:div {:class "col-md-6"}
               [:h4 "Most recent records"]
               (render-most-popular most-rec {:numbers false})
               ]]))))

(defn bootstrap-msg [the-val classes glyphs]
  (html [:div {:class (str "alert " classes)}
         [:span {:class (str "glyphicon " glyphs)
                 :aria-hidden "true"}]
         [:span {:class "sr-only"} "Error:"]
         the-val]))

(defn bootstrap-error [the-val]
  (bootstrap-msg the-val "alert-danger" "glyphicon-exclamation-sign"))

(defn bootstrap-successs [the-val]
  (bootstrap-msg the-val "alert-success" "glyphicon-ok"))

(defmacro is-main-tab [to-check the-var]
  `(if (= ~the-var ~to-check) {:class "active"}))

(defn wrap-in-panel [inner-html]
  (let [curr-tab (current-tab)
        mpage (main-page-link)
        byhero (by-hero-link)
        byopponent (by-opponent-link)
        byquestion (by-question-link)]
   (html [:div {:class "text-center"}
   [:div {:class "row"}
    [:div {:class "col-md-2"}]
    [:div {:class "col-md-8"}
     [:ul {:class "nav nav-tabs nav-justified"
         :style "margin-bottom: 15px;"}
     [:li (is-main-tab "main" curr-tab)
      [:a {:href mpage} "By matchup"]]
     [:li (is-main-tab "by-hero" curr-tab)
      [:a {:href byhero} "By hero"]]
     [:li (is-main-tab "by-opponent" curr-tab)
      [:a {:href byopponent} "By opponent"]]
     [:li (is-main-tab "by-question" curr-tab)
      [:a {:href byquestion} "By question"]]
     ]]
    [:div {:class "col-md-2"}]]
   inner-html])))

(defn generic-registration-page [context-vars req]
  (let [good-msg (get-in req [:cookies "q-praise" :value])]
    (html
      (context-js-variables)
      (if good-msg (bootstrap-successs good-msg))
      (wrap-in-panel
        (html
          [:div {:class "form-group"}
         (generate-hero-selection {})
         [:div {:class "form-inline text-center"}
          [:label {:for "user-filter"} "Filter to use"]
          [:br]
          (filter-dropdown "user-filter")]
         (reg-and-show-buttons)
         ]))
       (game-stats-render context-vars))))

(defn hero-pair-from-part-key [the-key]
  (if (not (clojure.string/blank? the-key))
   (let [findings (re-find #"(\d+)-(\d+)" the-key)
        hu (Integer. (nth findings 1))
        ho (Integer. (nth findings 2))]
    (gen-matchup hu ho))))

(defn gen-link-matchup-filter [matchup filter-id]
  (str (:matchup-link-start (html-context))
    "/" (:user matchup) "-" (:opponent matchup) "-" filter-id))

(defn gen-link-hero-filter [hero filter-id]
  (str (:hero-link-start (html-context))
    "/" hero "-" filter-id))

(defn gen-link-opponent-filter [hero filter-id]
  (str (:opponent-link-start (html-context))
    "/" hero "-" filter-id))

(defn gen-link-question [qid-tail]
  (str (:record-link-start (html-context))
    "/" qid-tail))

(defn wrap-most-popular-data [most-pop]
  (let [h-full (heroes-full)
        squares (get-hero-squares)]
   (filterv some?
     (for [i most-pop]
       (if i (let [the-key (:key i)
             split (hero-pair-from-part-key the-key)
             hu (:user split)
             ho (:opponent split)
             hn-user (nth h-full hu)
             hn-opp (nth h-full ho)
             sq-user (nth squares hu)
             sq-opp (nth squares ho)]
         (-> i
           (assoc :matchup split)
           (assoc :hn-user hn-user)
           (assoc :hn-opp hn-opp)
           (assoc :sq-user sq-user)
           (assoc :sq-opp sq-opp)
           (assoc :link (gen-link-matchup-filter split 0))
           )))))))

(defn wrap-most-recent-data [most-rec]
  (if most-rec
    (let [h-full (heroes-full)
          squares (get-hero-squares)
          revved (reverse most-rec)]
    (for [i revved]
      (let [split (matchup-pair-from-key i)
            key-tail (nth i 2)
            hu (:user split)
            ho (:opponent split)
            hn-user (nth h-full hu)
            hn-opp (nth h-full ho)
            sq-user (nth squares hu)
            sq-opp (nth squares ho)]
            (-> {}
             (assoc :key i)
             (assoc :matchup split)
             (assoc :hn-user hn-user)
             (assoc :hn-opp hn-opp)
             (assoc :sq-user sq-user)
             (assoc :sq-opp sq-opp)
             (assoc :link (gen-link-question key-tail))))))))

(defn generic-main-page [req]
  (let [context-vars {
            :global-most-popular
              (wrap-most-popular-data
                (get-most-popular-matchups-global))
            :global-most-recent
              (wrap-most-recent-data
                 (get-most-recent-questions 10))
            }]
        (wrap-html (generic-registration-page context-vars req))))

(defn dota2-page [req]
  (dota-ctx (generic-main-page req)))

(defn render-single-most-popular-hero [arg index]
  (let [link (:link arg)
        hn-user (:hn-user arg)
        sq-user (:sq-user arg)]
    (html [:tr {:onclick
               (str "window.location = '" link "';")
                 :style "cursor: pointer;"}
           (if index [:td {:style "width: 30px;"} "#" index])
           [:td {:style "width: 32px;"}
            [:img {:height "32" :width "32" :src sq-user}]]
           [:td {:class "text-center"} [:span
                 {:style "font-size: 12px;"} hn-user]]])))

(defn render-most-popular-heroes [the-vec]
  (render-most-popular-generic
    the-vec {:numbers true}
    render-single-most-popular-hero))

(defn wrap-most-popular-heroes [the-data]
  (let [filtered (filterv some? the-data)
        heroes (heroes-full)
        squares (get-hero-squares)]
   (->> filtered
       (mapv
         (fn [the-count]
           (let [the-id (:key the-count)]
            (merge the-count
                  {:hn-user (nth heroes the-id)
                   :sq-user (nth squares the-id)
                   :link (gen-link-hero-filter the-id 0)
                   })))))))

(defn generic-by-hero-page [req]
  (let [rel-data (get-most-popular-heroes-global)
        wrapped (wrap-most-popular-heroes rel-data)]
   (wrap-html
    (wrap-in-panel
      (html
        (context-js-variables)
        [:div {:class "input-group col-md-12"}
           [:h4 "View stats by user hero"]
           [:div {:class "row"
                  :style "margin-bottom: 7px;"}
             [:div {:class "col-md-4"}]
             [:div {:class "col-md-4"}
              (hero-dropdown "hero-user"
                             {})]
             [:div {:class "col-md-4"}]]
           [:div {:class "row"} (hero-icon "thumb-user" "")]
           [:a {:onclick "the_hero_hammer.js_client.goToHeroStats();"
                :class "btn btn-default"
                :style "margin-top: 10px;"}
            "Show stats"]
           [:div {:class "row"}
            [:div {:class "col-md-3"}]
            [:div {:class "col-md-6"}
             [:h4 "Most popular user heroes"]
             (render-most-popular-heroes wrapped)]
            [:div {:class "col-md-3"}]]]
        (update-hero-squares-script))))))

(defn wrap-most-popular-opponents [the-data]
  (let [filtered (filterv some? the-data)
        heroes (heroes-full)
        squares (get-hero-squares)]
   (->> filtered
       (mapv
         (fn [the-count]
           (let [the-id (:key the-count)]
            (merge the-count
                  {:hn-user (nth heroes the-id)
                   :sq-user (nth squares the-id)
                   :link (gen-link-opponent-filter the-id 0)
                   })))))))

(defn generic-by-opponent-page [req]
  (let [rel-data (get-most-popular-opponents-global)
        wrapped (wrap-most-popular-opponents rel-data)]
   (wrap-html
    (wrap-in-panel
      (html
        (context-js-variables)
        [:div {:class "input-group col-md-12"}
           [:h4 "View stats by opponent"]
           [:div {:class "row"
                  :style "margin-bottom: 7px;"}
             [:div {:class "col-md-4"}]
             [:div {:class "col-md-4"}
              (hero-dropdown "hero-opponent"
                             {})]
             [:div {:class "col-md-4"}]]
           [:div {:class "row"} (hero-icon "thumb-opponent" "")]
           [:a {:onclick "the_hero_hammer.js_client.goToOpponentStats();"
                :class "btn btn-default"
                :style "margin-top: 10px;"}
            "Show stats"]
           [:div {:class "row"}
            [:div {:class "col-md-3"}]
            [:div {:class "col-md-6"}
             [:h4 "Most popular opponent heroes"]
             (render-most-popular-heroes wrapped)]
            [:div {:class "col-md-3"}]]]
        (update-hero-squares-script))))))

(defn dota2-by-hero-page [req]
  (dota-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-hero")
      (generic-by-hero-page req))))

(defn lol-by-hero-page [req]
  (lol-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-hero")
      (generic-by-hero-page req))))

(defn dota2-by-opponent-page [req]
  (dota-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-opponent")
      (generic-by-opponent-page req))))

(defn lol-by-opponent-page [req]
  (lol-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-opponent")
      (generic-by-opponent-page req))))

(defn question-dropdown []
  (html
    [:select {:id "question-view-selection"
              :class "form-control"}
     (for [q (questions-full)]
       [:option {:value (:id q)} (:question q)]
       )]))

(defn generic-by-question-page [req]
  (wrap-html
    (wrap-in-panel
      (html
        (context-js-variables)
        [:h4 "View stats by question for user hero"]
        [:div {:class "row"}
         [:div {:class "col-md-2"}]
         [:div {:class "col-md-8"}
          (question-dropdown)]
         [:div {:class "col-md-2"}]]
        [:div {:class "row text-center"
               :style "margin-top: 10px; margin-bottom: 10px;"}
         [:button {:type "button"
                   :class "btn btn-default"
                   :onclick (go-to-gero-stats-js-func)}
              "View results"]]
            ))))

(defn dota2-by-question-page [req]
  (dota-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-question")
      (generic-by-question-page req))))

(defn lol-by-question-page [req]
  (lol-ctx
    (rebind-html-context
      (assoc (html-context) :current-tab "by-question")
      (generic-by-question-page req))))

(defn lol-page [req]
  (lol-ctx (generic-main-page req)))

(defn generic-render-questions
  [matchup req]
  (let [split (hero-pair-from-part-key matchup)
                     valid-error (get-in req [:cookies "q-error" :value])
                     form-data (get-in req [:cookies "q-forms" :value])
                     forms-des (if form-data (json/read-str form-data))
                     comm-data (if forms-des (get forms-des "user-comment" ""))
                     hu (:user split)
                     ho (:opponent split)
                     body (wrap-html
                       (html
                         (context-js-variables)
                         (if valid-error
                           (bootstrap-error valid-error ))
                         [:form {:id "questions-form"
                               :method "POST" :action (q-post-link)}
                        (generate-hero-selection
                          (if split {:selected-user hu
                                     :selected-opponent ho}))
                        [:div {:class "container-fluid input-group"}
                         (map #(render-question %1 forms-des) (questions-full))]
                        [:div {:classs "container"}
                         [:div {:class "row text-center"}
                          (email-link)
                          [:p "Your comment"]
                          [:textarea {:name "user-comment"
                                    :rows 4 :cols 50}
                           comm-data]]
                         [:div {:class "col-md-12"
                                :style (str "margin-left: 28%; margin-right: 28%;"
                                            "margin-top: 20px; margin-bottom: 20px;")
                                } (render-recaptcha)]
                         [:div {:class "row text-center"
                                :style "margin-top: 10px;"}
                          [:input {:class "btn btn-success"
                                   :type "submit"
                                   :value "Submit record"}]]]
                        ]))]
                   body))

(defn lol-render-questions
  ([matchup req]
    (lol-ctx (generic-render-questions matchup req)))
  ([req] (lol-render-questions nil req)))

(defn dota-render-questions
  ([matchup req]
    (dota-ctx (generic-render-questions matchup req)))
  ([req] (dota-render-questions nil req)))

(defn matchup-data-split [the-str]
  (let [findings (re-find #"(\d+)+-(\d+)-(\d+)" the-str)]
    [{:user (Integer. (nth findings 1))
      :opponent (Integer. (nth findings 2))}
      (Integer. (nth findings 3))]))

(defn hero-data-split [the-str]
  (let [findings (re-find #"(\d+)+-(\d+)" the-str)]
    {:hero (Integer. (nth findings 1))
     :filter (Integer. (nth findings 2))}))

(defn render-answers-single [pairs]
  (let [questions (questions-full)]
    (html [:table {:class "table table-condensed"}
       (for [i pairs]
         (let [question (nth questions (nth i 0))
               options (:options question)]
            [:tr
              [:td (:question question)]
              [:td (nth options (nth i 1))]
            ]))
       ])))

(defn date-from-unix [timestamp]
  (.format
    (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm:ss")
    (java.util.Date. (* 1000 timestamp))))

(defn render-vs-title [hero-user hero-opponent]
  (html [:div {:class "col-md-12 text-center"}
         [:h2 hero-user " vs. " hero-opponent]]))

(defn generic-show-record [id]
  (wrap-html
    (let [split (matchup-data-split id)
          matchup (nth split 0)
          rec-id (nth split 1)
          data-ans (get-answers-with-comment matchup rec-id)
          comm (:comment data-ans)
          date (:date data-ans)
          parted (partition 2 (:answers data-ans))
          squares (get-hero-squares)
          heroes (heroes-full)
          hu (:user matchup)
          ho (:opponent matchup)
          name-user (nth heroes hu)
          name-opp (nth heroes ho)
          square-user (nth squares hu)
          square-opp (nth squares ho)]
      (html
        (if data-ans
          (html
            (render-vs-title name-user name-opp)
            (render-hero-pair
            {:src-user square-user :src-opp square-opp})
          [:p "Date: " (date-from-unix date)]
          (render-answers-single parted)
          [:div {:class "row text-center"}
           [:div {:class "panel text-center col-md-12"}
            (if comm (html [:h4 "User comment"]
              [:p {:style "font-size: 17px;"} comm]))
            [:a {:class "btn btn-default"
                 :href (gen-link-matchup-filter matchup 0)}
             "Go to matchup stats"]
            ]])
          [:h3 "No such question."])))))

(defn lol-show-record [id]
  (lol-ctx (generic-show-record id)))

(defn dota-show-record [id]
  (dota-ctx (generic-show-record id)))

(defn bold-upper-text [the-text]
  (html [:p {:style "color: black; font-weight: bold;"}
         (clojure.string/upper-case the-text)]))

(defn divide-100 [the-vec]
  (let [sum (apply + the-vec)
        sz (count the-vec)
        res (int-array sz)
        until (dec sz)]
    (if (> sum 0)
      (loop [i 0 remainder 100]
        (let [islast (>= i until)
              curr (nth the-vec i)
              prelim (round-percent-ratio (/ curr sum))
              to-save (if (or islast (> prelim remainder))
                        remainder prelim)]
          (aset res i to-save)
          (if (not islast)
            (recur (inc i) (- remainder prelim))))))
    (vec res)))

(defn render-question-progress-bar [the-vec options]
  (let [the-sum (reduce + the-vec)
        div-by (if (= 0 the-sum) 1 the-sum)
        percent-wise (divide-100 the-vec)
        prog-bars (map #(html
                     [:div {:class (str
                                     "progress-bar "
                                     (if (= (mod %1 2) 0)
                                       "progress-bar-success"
                                       "progress-bar-info"))
                            :style (str "width: "
                                     (nth percent-wise %1)
                                   "%;")
                            }
                      (if (> %2 0) (bold-upper-text (str %3 " (" %2 ")")))]
                   ) (range (count the-vec))
                       the-vec options)
        ]
    (html [:div {:class "progress"}
          prog-bars
          ])))

(defn sum-counts [qdata]
  (apply + (get-in qdata [:answers])))

(defn render-single-question [qdata]
  (html [:p {:class "text-center"
             :style "font-weight: bold"}
            (:question qdata)]
        (render-question-progress-bar
          (get-in qdata [:answers])
          (get-in qdata [:options]))
        [:p {:class "text-center"}
         (str (sum-counts qdata) " samples.")]))

(defn export-matchup-data-to-js [user opp]
  (html [:script
         "heroUser = " user ";"
         "heroOpponent = " opp ";"
         "randCommentsLink = '" (:rand-comments-link (html-context)) "';"
         "recentCommentsLink = '" (:recent-comments-link (html-context)) "';"
         ]))

(defn gen-fb-comments [url numposts]
  (let [ze-num (or numposts 5)]
    (html [:div {:class "row"}
        [:div {:class "col-md-12 text-center"
               :id "fb-comments-placeholder"}
         (str "<div class=\"fb-comments\" "
              "data-href=\"" url "\" "
              "data-numposts=\"" ze-num "\"></div>")]])))

(defn show-comments-button-group []
  (html [:div {:class "row"}
               [:div {:class "col-md-12 text-center"}
                [:div {:class "btn-group"}
                 [:button {:type "button"
                           :class "btn btn-default"
                           :onclick (show-10-random-comments-js-func)
                           }
                  "Show random comments"]
                 [:button {:type "button"
                           :class "btn btn-default"
                           :onclick (show-10-recent-comments-js-func)
                           }
                  "Show recent comments"]
                 ]]]))

(defn comments-placeholder []
  (html [:div {:class "row"}
         [:div {:class "col-md-12 text-center"
                :id "comments-placeholder"}]]))

(defn full-url [tail]
  (str "https://herohammer.io" tail))

(defn generic-render-matchup-data [id]
  (let [[matchup filter-id]
            (matchup-data-split id)
            matchup-full-link
              (full-url (gen-link-matchup-filter matchup filter-id))
            sort-func (:question-sort-function (html-context))
            rel-data (fetch-relevant-matchup-data
                      matchup filter-id)
            mapped (mapv
                     #(assoc %1 :answers (get-in %1 [:counts :val]))
                     rel-data)
            sorted-data (into [] (sort-by sort-func > mapped))
            squares (get-hero-squares)
            heroes (heroes-full)
            hu (:user matchup)
            ho (:opponent matchup)
            hn-hu (nth heroes hu)
            hn-ho (nth heroes ho)
            sq-hu (nth squares hu)
            sq-ho (nth squares ho)]
          (wrap-html
            (html
              (export-matchup-data-to-js hu ho)
              (render-vs-title hn-hu hn-ho)
              (render-hero-pair
                {:src-user sq-hu :src-opp sq-ho})
                  [:ul {:class "list-group"}
              (->> sorted-data
                   (map render-single-question)
                   (map #(html
                           [:li {:class "list-group-item"}
                                %1])))]
              (show-comments-button-group)
              (comments-placeholder)
              (gen-fb-comments matchup-full-link 5)))))

(defn generic-render-hero-data [id]
  (let [the-data (hero-data-split id)
        the-hero (:hero the-data)
        the-filter (:filter the-data)
        hero-full-link (full-url (gen-link-hero-filter the-hero the-filter))
        sort-func (:question-sort-function (html-context))
        rel-data (fetch-relevant-hero-data the-hero the-filter)
        sorted-data (into [] (sort-by sort-func > rel-data))
        squares (get-hero-squares)
        heroes (heroes-full)
        hn (nth heroes the-hero)
        hs (nth squares the-hero)]
          (wrap-html
            (html
              (export-matchup-data-to-js the-hero -1)
              (render-vs-title hn "any")
              (render-hero-pair
                {:src-user hs :src-opp (question-sign-img)})
                  [:ul {:class "list-group"}
              (->> sorted-data
                   (map render-single-question)
                   (map #(html
                           [:li {:class "list-group-item"}
                                %1])))]
              (gen-fb-comments hero-full-link 5)))))

(defn generic-render-opponent-data [id]
  (let [the-data (hero-data-split id)
        the-hero (:hero the-data)
        the-filter (:filter the-data)
        hero-full-link (full-url (gen-link-opponent-filter the-hero the-filter))
        sort-func (:question-sort-function (html-context))
        rel-data (fetch-relevant-opponent-data the-hero the-filter)
        sorted-data (into [] (sort-by sort-func > rel-data))
        squares (get-hero-squares)
        heroes (heroes-full)
        hn (nth heroes the-hero)
        hs (nth squares the-hero)]
          (wrap-html
            (html
              (export-matchup-data-to-js the-hero -1)
              (render-vs-title "any" hn)
              (render-hero-pair
                {:src-user (question-sign-img) :src-opp hs})
                  [:ul {:class "list-group"}
              (->> sorted-data
                   (map render-single-question)
                   (map #(html
                           [:li {:class "list-group-item"}
                                %1])))]
              (gen-fb-comments hero-full-link 5)))))

(defn lol-render-matchup-data [id]
  (lol-ctx (generic-render-matchup-data id)))

(defn dota-render-matchup-data [id]
  (dota-ctx (generic-render-matchup-data id)))

(defn lol-render-hero-data [id]
  (lol-ctx (generic-render-hero-data id)))

(defn dota-render-hero-data [id]
  (dota-ctx (generic-render-hero-data id)))

(defn lol-render-opponent-data [id]
  (lol-ctx (generic-render-opponent-data id)))

(defn dota-render-opponent-data [id]
  (dota-ctx (generic-render-opponent-data id)))

(defn split-question-and-filter [the-str]
  (let [findings (re-find #"(\d+)+-(\d+)" the-str)]
    {:question (Integer. (nth findings 1))
     :filter (Integer. (nth findings 2))}))

(defn map-hero-icon-and-name-to-data [heroes squares the-data]
  (mapv #(let [this-idx (:hero-id %1)
               the-name (nth heroes this-idx)
               the-square (nth squares this-idx)]
           (merge %1
                {:hero-name the-name
                 :hero-square the-square}
                )) the-data))

(defn wrap-question-data [question the-data]
  (let [opts (:options question)
        heroes (heroes-full)
        squares (get-hero-squares)
        ans-range (range (count opts))
        ]
    (mapv #(hash-map
             :answer (nth opts %1)
             :index %1
             :data (map-hero-icon-and-name-to-data heroes squares %2))
          ans-range the-data)))

(defn render-answers [the-data]
  (for [i the-data]
   (let [curr-idx (:index i)
         curr-ans (:answer i)
         curr-data (:data i)]
    (html [:div {:class "row text-center"}
          [:h4 "Answer: " curr-ans]
          [:table {:class
                   "table table-hover table-striped table-condensed"}
           (map-indexed
             (fn [index data]
              (let [answers (:answers data)
                   hero-square (:hero-square data)
                   hero-name (:hero-name data)
                   ratio (:ratio data)
                   curr-count (nth answers curr-idx)
                   sum-ans (apply + answers)]
               [:tr
                 [:td {:style "width: 30px;"} "#" (inc index)]
                 [:td {:style "width: 50px;"}
                  [:img {:width 32
                         :height 32
                         :src hero-square}]]
                 [:td [:span hero-name]]
                 [:td [:span (round-percent-ratio ratio) "% ("
                       curr-count " out of " sum-ans " samples)"]]
               ]))
             curr-data)
           ]
          ]))))

(defn generic-render-question-data [id]
  (let [the-split (split-question-and-filter id)
        question-id (:question the-split)
        flt-id (:filter the-split)
        full-q (questions-full)
        this-q (nth full-q question-id)
        q-val (:question this-q)
        rel-data (fetch-relevant-question-data
                   question-id flt-id)
        wrapped (wrap-question-data this-q rel-data)
        ]
   (wrap-html (html
                [:div {:class "row text-center"}
                 [:h3 "Stats for question:"]]
                [:div {:class "row text-center"}
                 [:h4 q-val]]
                (render-answers wrapped))
              )))

(defn lol-render-question-data [id]
  (lol-ctx (generic-render-question-data id)))

(defn dota-render-question-data [id]
  (dota-ctx (generic-render-question-data id)))

(defn random-range [to-make max-num]
  (loop [the-set #{}]
    (if (or (>= (count the-set) to-make) (>= (count the-set) max-num))
      the-set
      (recur (conj the-set (rand-int max-num))))))

(defn generic-random-comments [id]
  (let [split (hero-pair-from-part-key id)
            comm-count (get-comments-count split)
            rnd-nums (into [] (random-range 10 comm-count))
            data (get-comments-by-id split rnd-nums)]
        (json/write-str data)))

(defn generic-recent-comments [id]
  (let [split (hero-pair-from-part-key id)
            comm-count (get-comments-count split)
            rnd-nums (reverse
                        (range (- comm-count 10) comm-count))
            data (get-comments-by-id split rnd-nums)]
        (json/write-str data)))

(defn lol-matchup-random-comments [id]
  (lol-ctx (generic-random-comments id)))

(defn lol-matchup-recent-comments [id]
  (lol-ctx (generic-recent-comments id)))

(defn dota-matchup-random-comments [id]
  (dota-ctx (generic-random-comments id)))

(defn dota-matchup-recent-comments [id]
  (dota-ctx (generic-recent-comments id)))

(defn question-set-similarity
  "Return percentage of values picked from user"
  [request]
  (let [cross (clojure.set/intersection (get-radio-set)
                (into #{} (keys request)))]
    (Math/round (float (* (/ (count cross) (count (get-radio-set))) 100)))))

(defmacro min-questions [] 77)
(defmacro max-comment-size [] 512)

(defn short-cookie [the-name the-value]
  {the-name {:value the-value :max-age 5}})

(defn verify-recaptcha [response ip]
  (let [answer (hclient/post (recaptcha-post-url)
        {:form-params
         {:secret (my-recaptcha-key-sec)
          :response response
          :remoteip ip}
        :accept :json} :as :json)]
    (if answer
      (let [parsed-body (json/read-str (:body answer))]
        (get parsed-body "success"))
      false)))

(defn check-answer-vec [the-vec]
  (if the-vec
    (let [questions (questions-full)
        all-good (every?
         #(let [index (get %1 0)
                answer (get %1 1)
                idx-q (get questions index)
                opts (if idx-q (:options idx-q))
                final (if opts (get opts answer))]
            (and index answer idx-q opts final)
          ) the-vec)
        uniq (into {} the-vec)
        uniq-same-size (= (count the-vec) (count uniq))]
    (and all-good uniq-same-size))
    false))

(defn check-answer-range [the-data]
  (check-answer-vec (:answers the-data)))

(defn generic-validate-answer [the-data req]
  (let [answered (question-set-similarity the-data)
        ret-err (fn [err]
          (let [cookie-err (short-cookie "q-error" err)
                cookie-forms (short-cookie "q-forms" (json/write-str the-data))
                merged (merge cookie-err cookie-forms)]
            (-> (ring.util.response/redirect (get-in req [:headers "referer"] (q-get-link)))
              (assoc :cookies merged)
              cook/cookies-response)))
        ret-succ (fn [praise]
          (-> (ring.util.response/redirect (main-page-link))
              (assoc :cookies {"q-praise" {:value praise :max-age 5}})
              cook/cookies-response))]
    (cond
      (> (min-questions) answered)
         (ret-err
          (str "Only " answered "% questions were answered."
                     " The minimum is " (min-questions) "%"))
      :else
        (let [form-data (form-to-data the-data)
              comm (:comment form-data)
              hu (:hero-user form-data)
              ho (:hero-opponent form-data)
              recapthcha (get the-data "g-recaptcha-response")]
          (cond
            (or (nil? hu)
                (nil? ho))
                (ret-err "lolwut?")
            ; these are bogus requests, don't give answer
            (not (check-answer-range form-data))
              (ret-err "Invalid answers posted.")
            (and comm (>= (count comm) (max-comment-size)))
              (ret-err (str "Comment exceeds maximum size of "
                          (max-comment-size) "."))
            ; TODO: check captcha
            (clojure.string/blank? recapthcha)
                (ret-err "Recaptcha answer is invalid.")
            :else (let [remote-ip (:remote-addr req)
                        recaptcha-ans (verify-recaptcha recapthcha remote-ip)]
                   (if recaptcha-ans
                     (do
                      (process-question form-data)
                      (ret-succ "Thank you! Your record will help everyone."))
                     (ret-err "Recaptcha answer is incorrect."))))
          ))))

(defn lol-post-questions [req]
  (lol-ctx (let [form-data (:params req)]
    (generic-validate-answer form-data req))))

(defn dota-post-questions [req]
  (dota-ctx (let [form-data (:params req)]
    (generic-validate-answer form-data req))))

(defroutes routes-lol
  (GET "/lol" [:as req] (lol-page req))
  (GET "/lol-by-hero" [:as req] (lol-by-hero-page req))
  (GET "/lol-by-opponent" [:as req] (lol-by-opponent-page req))
  (GET "/lol-by-question" [:as req] (lol-by-question-page req))
  (GET "/questions-lol/:matchup" [matchup :as req] (lol-render-questions matchup req))
  (GET "/questions-lol" [matchup :as req] (lol-render-questions req))
  (GET "/show-record-lol/:id" [id] (lol-show-record id))
  (GET "/comments-lol/random/:matchup" [matchup] (lol-matchup-random-comments matchup))
  (GET "/comments-lol/recent/:matchup" [matchup] (lol-matchup-recent-comments matchup))
  (GET "/matchup-lol/:id" [id] (lol-render-matchup-data id))
  (GET "/hero-lol/:id" [id] (lol-render-hero-data id))
  (GET "/opponent-lol/:id" [id] (lol-render-opponent-data id))
  (GET "/question-stats-lol/:id" [id] (lol-render-question-data id))
  (POST "/questions-post-lol" req (lol-post-questions req)))

(defroutes routes-dota
  (GET "/dota" [:as req] (dota2-page req))
  (GET "/dota-by-hero" [:as req] (dota2-by-hero-page req))
  (GET "/dota-by-opponent" [:as req] (dota2-by-opponent-page req))
  (GET "/dota-by-question" [:as req] (dota2-by-question-page req))
  (GET "/questions-dota/:matchup" [matchup :as req] (dota-render-questions matchup req))
  (GET "/questions-dota" [matchup :as req] (dota-render-questions req))
  (GET "/show-record-dota/:id" [id] (dota-show-record id))
  (GET "/comments-dota/random/:matchup" [matchup] (dota-matchup-random-comments matchup))
  (GET "/comments-dota/recent/:matchup" [matchup] (dota-matchup-recent-comments matchup))
  (GET "/matchup-dota/:id" [id] (dota-render-matchup-data id))
  (GET "/hero-dota/:id" [id] (dota-render-hero-data id))
  (GET "/opponent-dota/:id" [id] (dota-render-opponent-data id))
  (GET "/question-stats-dota/:id" [id] (dota-render-question-data id))
  (POST "/questions-post-dota" req (dota-post-questions req)))

(defroutes myapp
  (route/files "/resources/" {:root "resources/public/"})
  (GET "/" [] (index))
  routes-dota
  routes-lol
  (route/not-found "Page not found"))

(defn run-jobs []
  (lol-ctx
    ((get-ctx-jobs)))
  (dota-ctx
    ((get-ctx-jobs))))

; PROC EZ
;(the-hero-hammer.lol_context/process-questions)

(defn running-port []
  (or (env-num "HH_PORT") 5000))

(defn -main [& args]
  (println "Muah runnin!")
  (run-jobs)
  (-> myapp
      wrap-params
      cook/wrap-cookies
      (run-server {:port (running-port)})))
