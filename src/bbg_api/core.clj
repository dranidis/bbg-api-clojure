(ns bbg-api.core
  (:gen-class)
  (:require [clojure.xml :as xml]
            [clojure.string :as s]))

;; 
;; Collection API 
;; 
(defn fetch-collection-and-write-to-file [user-name]
  (spit "resources/collection.clj"
        (xml/parse (str "https://boardgamegeek.com/xmlapi/collection/" user-name))))

(defn read-collection-from-file []
  (:content (read-string (slurp "resources/collection.clj"))))


(defn game-id [game]
  (get-in game [:attrs :objectid]))

;; 
;; Game api
;; 
(defn api-read-game [game-id]
  (Thread/sleep 1000)
  (-> (xml/parse (str "https://boardgamegeek.com/xmlapi/boardgame/" game-id))
      :content
      first))

(defn get-games-and-write-to-file [collection]
  (spit "resources/games.clj"
        (reduce
         #(assoc %1 (get-in %2 [:attrs :objectid]) %2)
         {}
         (map api-read-game (map game-id collection)))))

(defn read-games-from-file []
  (read-string (slurp "resources/games.clj")))

;; 
;; Fields
;; 
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

(defn game-name [game]
  (-> game
      :content
      first
      :content
      first))

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

"Num of players"
(defn polls-with-num-of-players-for-game [game]
  (let [tag-list (game :content)
        tag-poll (filter (fn [x] (= (x :tag) :poll)) tag-list)
        recommended (filter (fn [x] (= (get-in x [:attrs :name]) "suggested_numplayers")) tag-poll)]
    ((first recommended) :content)))

(defn votes-best-rating-per-players [data]
  (let [players (get-in data [:attrs :numplayers])
        total-votes (apply + (map (fn [x] (read-string (get-in x [:attrs :numvotes]))) (data :content)))
        best-votes (read-string (get-in (first (data :content)) [:attrs :numvotes]))
        best-perc (if (= 0 best-votes) 0 (/ best-votes total-votes))]
    {:players players :best-voting best-votes :best-perc best-perc}))

(defn best-with-num-of-players [games collection-game]
  (let [game (games (game-id collection-game))
        votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game game))]
    (read-string ((apply max-key :best-perc votes) :players))))

;; 
;; Sorters
;; 
(defn game-better? [g1 g2]
  (> (game-rating g1) (game-rating g2)))

(defn game-shorter? [g1 g2]
  (< ((game-attribute g1) :playingtime) ((game-attribute g2) :playingtime)))

(defn game-shorter-and-better? [g1 g2]
  (> (/ (game-rating g1) ((game-attribute g1) :playingtime))
     (/ (game-rating g2) ((game-attribute g2) :playingtime))))

;; 
;; Filters
;; 
(defn has-name [name]
  (fn [game]
    (s/includes? (game-name game) name)))

(defn is-best-with-num-of-players [games num]
  (fn [game]
    (= num (best-with-num-of-players games game))))

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
  (fn [game]
    (let [game-rating (game-rating game)]
      (and (not (nil? game-rating)) (>= game-rating rating)))))

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

;; collection

  ; fetch my collection from the api and write it to file in resources
  (fetch-collection-and-write-to-file "ddmits")

  ; read my collection from the file
  (def collection (read-collection-from-file))
  (def game (first collection))
  (def game (nth collection 1))
  game
  (game-name game)
  (game-id game)
  (game-rating game)

  ; read a game from api
  (def pl (api-read-game "161936"))
  (def tta (api-read-game "182028"))
  (def mar (api-read-game "276025"))
  (game-id mar)

  ; read games from file
  (def games (read-games-from-file))
  games
  (count games)
  (keys games)

  (def mar (first (filter (has-name "Maracaibo") collection)))
  mar
  (game-id mar)
  (games (game-id mar))
  (best-with-num-of-players games mar)
  ((is-best-with-num-of-players games 3) mar)

  (def game (games (game-id mar)))
  (polls-with-num-of-players-for-game game)
  (map votes-best-rating-per-players (polls-with-num-of-players-for-game game))
  (def votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game game)))
  votes
  ((apply max-key :best-perc votes) :players)


  (map game-name
       (sort game-better?
             (filter
              (and-filters
               (with-number-of-players? 3)
               (is-best-with-num-of-players games 3)
               (playingtime-between? 0 240)
               (rating-higher-than? 4))
              collection)))

  (clojure.pprint/pp)




  ((playingtime-between? 20 90) game)

  "names of top 10 games in collection"
  (map game-name
       (take 10
             (sort game-better? collection)))

  (def pl (first (sort game-better? collection)))
  pl
  (take 10
        (map game-name
             (sort game-better?
                   (filter (with-number-of-players? 6) collection))))

  (def subcollection (take 3 collection))

  (count collection)

  (get-games-and-write-to-file collection)


  (count collection)
  (count (keys games))
  (games "22038")

  (game-id (games "12"))

  (is-best-with-num-of-players games 2)
  tta
  ((is-best-with-num-of-players games 2) pl)

  (best-with-num-of-players games tta)

  (game-id tta)
  (games (game-id tta))
  (def game (games (game-id tta)))
  game

  game-id

  ;(apply  api-read-game (map game-id subcollection))

  (take 10
        (map game-name
             (sort game-better?
                   (filter (is-best-with-num-of-players 2) subcollection))))

  (is-best-with-num-of-players 2)
  (best-with-num-of-players tta)

  (map game-name
       (sort game-shorter-and-better?
             (filter
              (and-filters
               (with-number-of-players? 4)
               (playingtime-between? 0 240)
               (rating-higher-than? 4))
              collection)))

  (map (juxt game-name game-id game-rating)
       (sort game-shorter-and-better?
             (filter
              (and-filters
               (with-number-of-players? 2)
               (playingtime-between? 30 240)
               (rating-higher-than? 4))
              collection)))

  (assoc {} :a 1)

  (defn fake-fetch []
    (Thread/sleep 5000)
    "Ready!")

  (fake-fetch)

  ;;
  )

