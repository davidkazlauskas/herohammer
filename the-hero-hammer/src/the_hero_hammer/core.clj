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
            [clj-http.client :as hclient])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.cookies :as cook]))


(def ^:dynamic *html-context* nil)

(defmacro lol-ctx [& args]
  `(binding [*ctx-get-func* (fn [] lctx/*hh-context-lol*)
             ;*get-db-context* (fn [] sram/*storage-ram-context*) ; for testing
             *get-db-context* (fn [] saes/*storage-aes-context*)
             *html-context* *html-context-lol*]
     ~@args
   ))

(defmacro dota-ctx [& args]
  `(binding [*ctx-get-func* (fn [] dctx/*hh-context-dota*)
             ;*get-db-context* (fn [] sram/*storage-ram-context*) ; for testing
             *get-db-context* (fn [] saes/*storage-aes-context*)
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

(defn render-recaptcha []
  (html [:div {:class "g-recaptcha"
               :data-sitekey (my-recaptcha-key)}]))

(def ^:dynamic *html-context-lol*
  (lol-ctx
    {:main-page-for-ctx "/lol"
     :question-post-link "/questions-post-lol"
     :question-get-link "/questions-lol"
     :matchup-link-start "/matchup-lol"
     :add-record-link-start "/questions-lol"
     :record-link-start "/show-record-lol"
     :registration-link "/questions-lol"
     :rand-comments-link "/comments-lol/random"
     :recent-comments-link "/comments-lol/recent"
     :squares-javascript (generate-javascript-hero-squares)
     :question-sort-function
       (fn [the-q]
         (let [sname (:shortname the-q)
               opt-vec (get-in the-q [:counts :val])]
         (cond (= "mtype" sname) 200
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
     :question-post-link "/questions-post-dota"
     :question-get-link "/questions-dota"
     :matchup-link-start "/matchup-dota"
     :add-record-link-start "/questions-dota"
     :record-link-start "/show-record-dota"
     :registration-link "/questions-dota"
     :rand-comments-link "/comments-dota/random"
     :recent-comments-link "/comments-dota/recent"
     :squares-javascript (generate-javascript-hero-squares)
     :question-sort-function
       (fn [the-q]
         (let [sname (:shortname the-q)
               opt-vec (get-in the-q [:counts :val])]
         (cond (= "mtype" sname) 200
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

(defn parse-int [s]
   (if s
     (let [try-parse (re-find  #"\d+" s)]
       (if try-parse (Integer. try-parse)))))

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
     "Where your whining can help!"]))

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

(defn wrap-html [towrap]
  (html [:html
         [:head
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
          [:script {:src "https://www.google.com/recaptcha/api.js"}]]
         [:body
          (navbar {})
          [:div {:class "container"}
           [:div {:class "row"}
            [:div {:class "col-md-2"}]
            [:div {:class "col-md-8"} towrap]
            [:div {:class "col-md-2"}]
            ]
           [:div {:class "panel"}
            [:div {:class "panel-footer text-center"}
             [:span "The Hero Hammer 2016"]]]
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

(defn get-radio-set []
  (:radio-set (html-context)))

(defn update-hero-squares-script []
  (html [:script (update-hero-squares-js-func)]))

(defn hero-dropdown [select-id args]
  (let [selected (:selected args)]
    (html [:select {:class "form-control"
                  :id select-id
                  :name select-id
                  :onchange (update-hero-squares-js-func)}
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

(defn hero-icon [thumb-id src]
  (html [:img {:width 120 :height 120
               :src src
               :style "margin-left: 10px; margin-right: 10px;"
               :id thumb-id
               }]))

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
         [:td
          [:img {:height "32" :width "32" :src sq-user}]
          [:span
           {:style "margin-left: 10px; margin-right: 10px;"}
           "vs."]
          [:img {:height "32" :width "32" :src sq-opp}]]
         [:td [:span
               {:style "font-size: 12px;"} hn-user " vs. " hn-opp]]
         ])))

(defn render-most-popular [the-vec args]
  (let [draw-numbers (:numbers args true)]
   (for [i (range (count the-vec))]
    (html [:table
           {:class
           "table table-hover table-striped table-condensed"}
           (single-matchup-listing
             (nth the-vec i)
             (if draw-numbers (inc i)))]))))

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

(defn generic-registration-page [context-vars req]
  (let [good-msg (get-in req [:cookies "q-praise" :value])]
    (html
      (context-js-variables)
      (if good-msg (bootstrap-successs good-msg))
      [:div {:class "form-group"}
       (generate-hero-selection {})
       [:div {:class "form-inline text-center"}
        [:label {:for "user-filter"} "Filter to use"]
        [:br]
        (filter-dropdown "user-filter")]
       (reg-and-show-buttons)
       ]
       (game-stats-render context-vars))))

(defn hero-pair-from-part-key [the-key]
  (if (not (clojure.string/blank? the-key))
   (let [findings (re-find #"(\d+)-(\d+)" the-key)
        hu (Integer. (nth findings 1))
        ho (Integer. (nth findings 2))]
    (gen-matchup hu ho))))

(defn gen-link-matchup-filter [matchup filter-id]
  (str (:matchup-link-start (html-context))
    "/" (:user matchup) "-" (:opponent matchup) "-0"))

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

(defn lol-page [req]
  (lol-ctx (generic-main-page req)))

(defn q-post-link [] (:question-post-link (html-context)))
(defn q-get-link [] (:question-get-link (html-context)))

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

(defn render-answers [pairs]
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
          (render-answers parted)
          (if comm
            [:div {:class "row text-center"}
             [:div {:class "panel text-center col-md-12"}
              [:h4 "User comment"]
              [:p {:style "font-size: 17px;"} comm]
              [:a {:class "btn btn-default"
                   :href (gen-link-matchup-filter matchup 0)}
               "Go to matchup stats"]
              ]]))
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
                                     "progress-bar-striped "
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
  (apply + (get-in qdata [:counts :val])))

(defn render-single-question [qdata]
  (html [:p {:class "text-center"
             :style "font-weight: bold"}
            (:question qdata)]
        (render-question-progress-bar
          (get-in qdata [:counts :val])
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

(defn generic-render-matchup-data [id]
  (let [[matchup filter-id]
            (matchup-data-split id)
            sort-func (:question-sort-function (html-context))
            rel-data (fetch-relevant-matchup-data
                      matchup filter-id)
            sorted-data (into [] (sort-by sort-func > rel-data))
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
              (comments-placeholder)))))

(defn lol-render-matchup-data [id]
  (lol-ctx (generic-render-matchup-data id)))

(defn dota-render-matchup-data [id]
  (dota-ctx (generic-render-matchup-data id)))

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
          (-> (ring.util.response/redirect (:main-page-for-ctx (html-context)))
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
  (GET "/questions-lol/:matchup" [matchup :as req] (lol-render-questions matchup req))
  (GET "/questions-lol" [matchup :as req] (lol-render-questions req))
  (GET "/show-record-lol/:id" [id] (lol-show-record id))
  (GET "/comments-lol/random/:matchup" [matchup] (lol-matchup-random-comments matchup))
  (GET "/comments-lol/recent/:matchup" [matchup] (lol-matchup-recent-comments matchup))
  (GET "/matchup-lol/:id" [id] (lol-render-matchup-data id))
  (POST "/questions-post-lol" req (lol-post-questions req)))

(defroutes routes-dota
  (GET "/dota" [:as req] (dota2-page req))
  (GET "/questions-dota/:matchup" [matchup :as req] (dota-render-questions matchup req))
  (GET "/questions-dota" [matchup :as req] (dota-render-questions req))
  (GET "/show-record-dota/:id" [id] (dota-show-record id))
  (GET "/comments-dota" [matchup] (dota-matchup-random-comments matchup))
  (GET "/comments-dota" [matchup] (dota-matchup-recent-comments matchup))
  (GET "/matchup-dota/:id" [id] (dota-render-matchup-data id))
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

(defn -main [& args]
  (println "Muah runnin!")
  (run-jobs)
  (-> myapp
      wrap-params
      cook/wrap-cookies
      (run-server {:port 5000})))
