(ns the-hero-hammer.core
  (:gen-class)
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.lol_context :as lctx]
            [org.httpkit.server :refer [run-server]]
            [cljs.build.api :as cljsbld])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]))


(def ^:dynamic *html-context* nil)

(defmacro lol-ctx [& args]
  `(binding [*ctx-get-func* (fn [] lctx/*hh-context-lol*)
             *html-context* *html-context-lol*]
     ~@args
   ))

(defn generate-javascript-hero-squares []
  (let [squares (get-in (*ctx-get-func*) [:heroes :squares])]
    (->> squares
         (map #(str "'" %1 "'"))
         (interpose ",")
         (clojure.string/join)
         #(str "heroSquares = [" %1 "];"))))

(def ^:dynamic *html-context-lol*
  (lol-ctx
    {:matchup-link-start "/matchup-lol"
     :registration-link "/questions-lol"
     :squares-javascript (generate-javascript-hero-squares)
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
          [:script {:src "resources/js/main.js"}]]
         [:body
          (navbar {})
          [:div {:class "container"}
           [:div {:class "row"}
            [:div {:class "col-md-2"}]
            [:div {:class "col-md-8"} towrap]
            [:div {:class "col-md-2"}]
            ]]]
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

(defn hero-dropdown [select-id]
  (html [:select {:class "form-control"
                  :id select-id}
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

(defn hero-icon [hero-id]
  (html [:img {:width 120 :height 120
               :src "http://vignette1.wikia.nocookie.net/leagueoflegends/images/1/18/AhriSquare.png"
               :style "margin-left: 10px; margin-right: 10px;"
               }])
  )

(defn reg-and-show-buttons []
  (html [:div {:style "margin-top: 20px;" :class "text-center"}
         [:div {:class "btn-group"}
           [:a {:style "width: 120px;"
                :class "btn btn-default" } "Add record"]
           [:a {:onclick "the_hero_hammer.js_client.goToMatchup();"
                :style "width: 120px;"
                :class "btn btn-default" } "Show results"]]]))

(defn context-js-variables []
  (html [:script "registrationLink = '"
         (:registration-link (html-context))
         "'; matchupLink = '"
         (:matchup-link-start (html-context))
         "';"]))

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
        [:div {:style "padding-top: 20px; padding-bottom: 20px;"
               :class "text-center"}
         (hero-icon "meow")
         (hero-icon "meow")]))

(defn generic-registration-page []
  (html
    (context-js-variables)
    [:div {:class "form-group"}
     (generate-hero-selection {})
     [:div {:class "form-inline text-center"}
      [:label {:for "user-filter"} "Filter to use"]
      [:br]
      (filter-dropdown "user-filter")]
     (reg-and-show-buttons)
     ]))

(defn dota2-page []
  (wrap-html [:p "meow"]))

(defn lol-page []
  (lol-ctx
    (wrap-html (generic-registration-page))))

(defmacro q-post-link [] "/questions-post")

(defn lol-render-questions []
  (lol-ctx (wrap-html [:form {:id "questions-form"
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
                         :value "Submit record"}]]]])))

(defn matchup-data-split [the-str]
  (let [findings (re-find #"(\d+)+-(\d+)-(\d+)" the-str)]
    [{:user (Integer. (nth findings 1))
      :opponent (Integer. (nth findings 2))}
      (Integer. (nth findings 3))]))

(defn ratio-percent [rat]
  (format "%.0f%%" (* 100 (float rat))))

(defn bold-upper-text [the-text]
  (html [:p {:style "color: black; font-weight: bold;"}
         (clojure.string/upper-case the-text)]))

(defn render-question-progress-bar [the-vec options]
  (let [the-sum (reduce + the-vec)
        div-by (if (= 0 the-sum) 1 the-sum)
        prog-bars (map #(html
                     [:div {:class (str
                                     "progress-bar "
                                     "progress-bar-striped "
                                     (if (= (mod %1 2) 0)
                                       "progress-bar-success"
                                       "progress-bar-info"))
                            :style (str "width: "
                                     (ratio-percent (/ %2 div-by))
                                   ";")
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
          (get-in qdata [:counts :count])
          (get-in qdata [:options]))
        [:p {:class "text-center"}
         (str (range-size (:counts qdata)) " samples.")]))

(defn lol-render-matchup-data [id]
  (lol-ctx
    (let [[matchup filter-id]
          (matchup-data-split id)
          rel-data (fetch-relevant-matchup-data
                    matchup filter-id)]
        (wrap-html [:ul {:class "list-group"}
          (->> rel-data
               (map render-single-question)
               (map #(html
                       [:li {:class "list-group-item"}
                            %1])))]))))

(defn lol-question-set-similarity
  "Return percentage of values picked from user"
  [request]
  (let [cross (clojure.set/intersection *radio-set-lol*
                (into #{} (keys request)))]
    (* (/ (count cross) (count *radio-set-lol*)) 100)))

(defmacro min-questions [] 77)

(defn lol-post-questions [req]
  (lol-ctx (let [answered (lol-question-set-similarity req)]
    (println "ans" answered)
    (if (> (min-questions) answered)
      (do
        (println req)
      (str "Only " answered "% of questions were answered."
           " Minimum is " (min-questions) "%."))
      (do (println "Saving question!")
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
  (GET "/matchup-lol/:id" [id] (lol-render-matchup-data id))
  (POST (q-post-link) {params :params} (lol-post-questions params)
  (route/not-found "Page not found")))

(defn -main [& args]
  (println "Muah runnin!")
  (run-server (wrap-params myapp) {:port 5000}))
