(ns the-hero-hammer.core
  (:gen-class)
  (:require [compojure.core :refer :all]
            [the-hero-hammer.hh_context :refer :all]
            [the-hero-hammer.hh_process :refer :all]
            [the-hero-hammer.lol_context :as lctx]
            [org.httpkit.server :refer [run-server]])
  (:use hiccup.core
        [ring.middleware.params :only [wrap-params]]))

(defmacro lol-ctx [& args]
  `(binding [*ctx-get-func* (fn [] lctx/*hh-context-lol*)]
     ~@args
   ))

(def ^:dynamic *radio-set-lol*
  (lol-ctx (into #{} (map #(:shortname %1)
       (questions-full)))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn form-to-data [form]
  {
   :hero-user (parse-int (get form "user-hero"))
   :hero-opponent (parse-int (get form "opponent-hero"))
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
    [:p (:question q)]
   (map-indexed #(identity
         [:input {:type "radio" :value
                  %1
                  :name shortname
                  } %2])
        (:options q))
   [:br])))

(defn navbar [args]
  (html [:nav {:class "navbar navbar-default"}]))

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
                    :crossorigin "anonymous"}]]
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
  (wrap-html [:h1 "Dazlow!"]))

(defmacro q-post-link [] "/questions-post")

(defn lol-render-questions []
  (lol-ctx (wrap-html [:form {:id "questions-form"
                     :method "POST" :action (q-post-link)}
              [:select {:name "user-hero"}
               (map-indexed #(html
                   [:option {:value %1} %2]
                   ) (heroes-full))
               ]
              [:select {:name "opponent-hero"}
               (map-indexed #(html
                   [:option {:value %1} %2]
                   ) (heroes-full))
               ]
              (map render-question (questions-full))
              [:textarea {:name "user-comment"
                          :rows 4 :cols 50}]
              [:input {:type "submit"
                       :value "Submit record"}]])))

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
  (GET "/" [] (index))
  (GET "/questions-lol" [] (lol-render-questions))
  (GET "/matchup-lol/:id" [id] (lol-render-matchup-data id))
  (POST (q-post-link) {params :params} (lol-post-questions params)))

(defn -main [& args]
  (println "Muah runnin!")
  (run-server (wrap-params myapp) {:port 5000}))
