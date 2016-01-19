(ns the-hero-hammer.questions)

(defn question [id shortname question & options]
   {:id id
    :shortname shortname
    :question question
    :options (into [] options)})

(defn questions [& args]
  (into [] (map-indexed
     #(apply question %1 %2) args)))

(defmacro questions-m [& args]
  (apply questions args))

(defn get-question [questionshort args]
  (println args)
  (->> args
       (map-indexed
          #(if (= (get %2 :shortname) questionshort) %1 nil))
       (filter some?)
       (first)))

(defn get-question-and-answer-id [questionshort answer questions]
  (let [qid (get-question questionshort questions)]
  [qid
   (->> (get (get (questions) qid) :options)
     (map-indexed #(if (= %2 answer) %1 nil))
     (filter some?)
     (first))]))

(defmacro get-question-and-answer-id-m
  [questionshort answer questions]
  (get-question-and-answer-id questionshort answer))
