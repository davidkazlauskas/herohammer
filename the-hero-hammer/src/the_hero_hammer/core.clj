(ns the-hero-hammer.core
  (:gen-class)
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.db_context :refer :all]
            [the-hero-hammer.lol_context :as lctx]
            [the-hero-hammer.storage_ram :as sram]
            [org.httpkit.server :refer [run-server]]
            [cljs.build.api :as cljsbld]
            [clojure.data.json :as json])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]))


(def ^:dynamic *html-context* nil)

(defmacro lol-ctx [& args]
  `(binding [*ctx-get-func* (fn [] lctx/*hh-context-lol*)
             *db-context* (fn [] sram/*storage-ram-context*)
             *html-context* *html-context-lol*]
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

(def ^:dynamic *html-context-lol*
  (lol-ctx
    {:matchup-link-start "/matchup-lol"
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
               :else (highest-percent-part opt-vec))))
     }))

(def ^:dynamic *html-context* nil)

(defn html-context []
  *html-context*)

(def ^:dynamic *radio-set-lol*
  (lol-ctx (into #{} (map #(:shortname %1)
       (questions-full)))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

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

(defn render-question [q]
  (let [shortname (:shortname q)] (html
   [:div {:class "text-center"} [:p (:question q)]
   [:div {:class "form-inline"}
    (map-indexed #(identity
          [:label
                [:input
                 {:style "margin-left: 7px; margin-right: 7px;"
                  :type "radio"
                  :value %1
                  :name shortname
                  } %2]])
        (:options q))]]
   [:br])))

(defn navbar [args]
  (html [:nav {:class "navbar navbar-default"}
         [:div {:class "container"}
          [:div {:class "navbar-header"}
           [:a {:class "navbar-brand" :href "/"} "The Hero Hammer"]]
          [:div {:class "collapse navbar-collapse"}
           [:ul {:class "nav navbar-nav"}
            [:li [:a {:href "/dota2"} "DotA 2"]]
            [:li [:a {:href "/lol"} "League of Legends"]]
            ]
           [:ul {:class "nav navbar-nav navbar-right"}
            [:li [:h4 "Find out if you're about to get whooped... In seconds!"]]
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
          [:script {:src "/resources/js/main.js"}]]
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
                       :href "/dota2"} "DotA 2"]
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

(defn update-hero-squares-script []
  (html [:script (update-hero-squares-js-func)]))

(defn hero-dropdown [select-id]
  (html [:select {:class "form-control"
                  :id select-id
                  :onchange (update-hero-squares-js-func)}
         (map-indexed
           #(html [:option {:value %1} %2])
           (heroes-full))
         ]))

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
           [:a {:style "width: 120px;"
                :class "btn btn-default"
                :href (:registration-link (html-context))
                } "Add record"]
           [:a {:onclick "the_hero_hammer.js_client.goToMatchup();"
                :style "width: 120px;"
                :class "btn btn-default" } "Show results"]]]))

(defn context-js-variables []
  (html [:script "registrationLink = '"
         (:registration-link (html-context))
         "'; matchupLink = '"
         (:matchup-link-start (html-context))
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
         (hero-dropdown "hero-user")
         (hero-dropdown "hero-opponent")
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

(defn generic-registration-page [context-vars]
  (html
    (context-js-variables)
    [:div {:class "form-group"}
     (generate-hero-selection {})
     [:div {:class "form-inline text-center"}
      [:label {:for "user-filter"} "Filter to use"]
      [:br]
      (filter-dropdown "user-filter")]
     (reg-and-show-buttons)
     ]
     (game-stats-render context-vars)
    ))

(defn hero-pair-from-part-key [the-key]
  (let [findings (re-find #"(\d+)-(\d+)" the-key)
        hu (Integer. (nth findings 1))
        ho (Integer. (nth findings 2))]
    (gen-matchup hu ho)))

(defn gen-link-matchup-filter [matchup filter-id]
  (str (:matchup-link-start (html-context))
    "/" (:user matchup) "-" (:opponent matchup) "-0"))

(defn gen-link-question [qid-tail]
  (str (:record-link-start (html-context))
    "/" qid-tail))

(defn wrap-most-popular-data [most-pop]
  (let [h-full (heroes-full)
        squares (get-hero-squares)]
   (for [i most-pop]
     (let [the-key (:key i)
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
         )))))

(defn wrap-most-recent-data [most-rec]
  (if most-rec
    (let [h-full (heroes-full)
          squares (get-hero-squares)
          revved (reverse most-rec)
          ]
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

(defn dota2-page []
  (wrap-html [:p "meow"]))

(defn lol-page []
  (lol-ctx
    (let [context-vars {
          :global-most-popular
            (wrap-most-popular-data
              (get-most-popular-matchups-global))
          :global-most-recent
            (wrap-most-recent-data
               (get-most-recent-questions 10))
          }]
      (wrap-html (generic-registration-page context-vars)))))

(defmacro q-post-link [] "/questions-post")

(defn lol-render-questions []
  (lol-ctx (wrap-html
             (html
               (context-js-variables)
               [:form {:id "questions-form"
                     :method "POST" :action (q-post-link)}
              (generate-hero-selection {})
              [:div {:class "container-fluid input-group"}
               (map render-question (questions-full))]
              [:div {:classs "container"}
               [:div {:class "row text-center"}
                [:p "Your comment"]
                [:textarea {:name "user-comment"
                          :rows 4 :cols 50}]]
               [:div {:class "row text-center"
                      :style "margin-top: 10px;"}
                [:input {:class "btn btn-success"
                         :type "submit"
                         :value "Submit record"}]]]]))))

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
              ]]))
          [:h3 "No such question."])))))

(defn lol-show-record [id]
  (lol-ctx (generic-show-record id)))

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

(defn render-single-question [qdata]
  (html [:p {:class "text-center"
             :style "font-weight: bold"}
            (:question qdata)]
        (render-question-progress-bar
          (get-in qdata [:counts :val])
          (get-in qdata [:options]))
        [:p {:class "text-center"}
         (str (range-size (:counts qdata)) " samples.")]))

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

(defn lol-render-matchup-data [id]
  (lol-ctx
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
            (comments-placeholder))))))

(defn random-range [to-make max-num]
  (loop [the-set #{}]
    (if (or (>= (count the-set) to-make) (>= (count the-set) max-num))
      the-set
      (recur (conj the-set (rand-int max-num))))))

(defn lol-matchup-random-comments [id]
  (lol-ctx
    (let [split (hero-pair-from-part-key id)
          comm-count (get-comments-count split)
          rnd-nums (into [] (random-range 10 comm-count))
          data (get-comments-by-id split rnd-nums)]
      (json/write-str data))))

(defn lol-matchup-recent-comments [id]
  (lol-ctx
    (let [split (hero-pair-from-part-key id)
          comm-count (get-comments-count split)
          rnd-nums (reverse
                      (range (- comm-count 10) comm-count))
          data (get-comments-by-id split rnd-nums)]
      (json/write-str data))))

(defn lol-question-set-similarity
  "Return percentage of values picked from user"
  [request]
  (let [cross (clojure.set/intersection *radio-set-lol*
                (into #{} (keys request)))]
    (* (/ (count cross) (count *radio-set-lol*)) 100)))

(defmacro min-questions [] 77)

(defn lol-post-questions [req]
  (lol-ctx (let [answered (lol-question-set-similarity req)]
    (if (> (min-questions) answered)
      (do
      (str "Only " answered "% of questions were answered."
           " Minimum is " (min-questions) "%."))
      (do
        (process-question (form-to-data req))
          (html [:p (map #(html [:p %1]) req)]))
      )
    )))

(defroutes myapp
  (route/files "/resources/" {:root "resources/public/"})
  (GET "/" [] (index))
  (GET "/dota2" [] (dota2-page))
  (GET "/lol" [] (lol-page))
  (GET "/questions-lol" [] (lol-render-questions))
  (GET "/show-record-lol/:id" [id] (lol-show-record id))
  (GET "/comments-lol/random/:matchup" [matchup] (lol-matchup-random-comments matchup))
  (GET "/comments-lol/recent/:matchup" [matchup] (lol-matchup-recent-comments matchup))
  (GET "/matchup-lol/:id" [id] (lol-render-matchup-data id))
  (POST (q-post-link) {params :params} (lol-post-questions params)
  (route/not-found "Page not found")))

(defn run-jobs []
  (lol-ctx
    ((get-ctx-jobs))))

(defn -main [& args]
  (println "Muah runnin!")
  (run-jobs)
  (run-server (wrap-params myapp) {:port 5000}))
