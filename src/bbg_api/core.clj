(ns bbg-api.core
  (:gen-class)
  (:require [clojure.xml :as xml])
  (:require  [clojure.edn :as edn]))

(defn get-collection-and-write-to-file []
  (spit "resources/ddmits.clj" (xml/parse "https://boardgamegeek.com/xmlapi/collection/ddmits")))

(defn read-collection-from-file []
  (edn/read-string (slurp "resources/ddmits.clj")))

(defn get-games-from-collection [collection]
  (collection :content))

(defn game-my-rating [game]
  (let [rating (-> game
                   :content
                   (nth 4)
                   :content
                   first
                   :attrs
                   :value
                   read-string)]
    (if (number? rating) rating nil)))

(defn game-rating [game]
  (let [rating (-> game
                   :content
                   (nth 4)
                   :content
                   first
                   :content
                   (nth 2)
                   :attrs
                   :value
                   read-string)]
    (if (number? rating) rating nil)))

(defn has-rating? [game]
  (not (nil? (game-rating game))))

(defn game-name [game]
  (-> game
      :content
      first
      :content
      first))

(defn game-better? [g1 g2]
  (> (game-rating g1) (game-rating g2)))

(defn game-attributes [game]
  (-> game
      :content
      (nth 4)
      :attrs))

(defn game-attribute [game]
  (fn [key]
    (let [attr (game-attributes game)
          value (attr key)]
      (if value (read-string value) nil))))

"Filters"
(defn playingtime-between? [min max]
  (fn [game]
    (let [time ((game-attribute game) :playingtime)]
      (and
       (number? time)
       (>= time min)
       (<= time max)))))

(defn with-number-of-players? [num]
  (fn [game] (and
              (>= ((game-attribute game) :maxplayers) num)
              (<= ((game-attribute game) :minplayers) num))))

(defn rating-higher-than? [rating]
  (fn [game] (and (has-rating? game) (>= (game-rating game) rating))))

;; (defn and-filters
;;   "Composes filters into one."
;;   [filter1 filter2]
;;   (fn [game] (and
;;               (filter1 game)
;;               (filter2 game))))

(defn and-filters
  "Composes filters into one."
  [& filters]
  (fn [game] (reduce
              #(and %1 %2)
              ((apply juxt filters) game))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (get-collection-and-write-to-file)
  (def collection (get-games-from-collection (read-collection-from-file)))
  (def game (first collection))
  (def game (nth collection 1))
  game
  (game-name game)
  (game-rating game)
  (has-rating? game)

  ((playingtime-between? 20 90) game)
  (clojure.pprint/pp)

  "names of top 10 games in collection"
  (map game-name
       (take 10
             (sort game-better? collection)))

  (take 10
        (map game-name
             (sort game-better?
                   (filter (with-number-of-players? 2) collection))))

  (map game-name
       (sort game-better?
             (filter
              (and-filters
               (with-number-of-players? 4)
               (playingtime-between? 0 60)
               (rating-higher-than? 7.5))
              collection)))
  ;;
  )

