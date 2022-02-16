(ns bbg-api.core
  (:gen-class)
  (:require [clojure.xml :as xml]
            [clojure.string :as s]))


;; 
;; Fields
;; 
(defn game-my-rating [collection-game]
  (let [rating (-> collection-game
                   :content
                   (nth 4)
                   :content
                   first
                   :attrs
                   :value
                   read-string)]
    (if (number? rating) rating nil)))

(defn game-rating [collection-game]
  (let [rating (-> collection-game
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

(defn game-name [collection-game]
  (-> collection-game
      :content
      first
      :content
      first))

(defn game-attributes [collection-game]
  (-> collection-game
      :content
      (nth 4)
      :attrs))

(defn game-attribute [collection-game]
  (fn [key]
    (let [attr (game-attributes collection-game)
          value (attr key)]
      (if value (read-string value) nil))))

(defn game-playingtime [collection-game]
  ((game-attribute collection-game) :playingtime))

(defn game-maxplayers [collection-game]
  ((game-attribute collection-game) :maxplayers))

(defn game-minplayers [collection-game]
  ((game-attribute collection-game) :minplayers))

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
        best-perc (if (= 0 total-votes) 0 (/ best-votes total-votes))

        recommended-votes (read-string (get-in (second (data :content)) [:attrs :numvotes]))
        recommended-perc (if (= 0 total-votes) 0 (/ recommended-votes total-votes))
        not-recommended-votes (read-string (get-in (last (data :content)) [:attrs :numvotes]))
        not-recommended-perc (if (= 0 total-votes) 0 (/ not-recommended-votes total-votes))]
    {:players players
     :best-votes best-votes :best-perc best-perc
     :recommended-votes recommended-votes :recommended-perc recommended-perc
     :not-recommended-votes not-recommended-votes :not-recommended-perc not-recommended-perc}))

(defn best-with-num-of-players [games collection-game]
  (let [game (games (game-id collection-game))
        votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game game))]
    ((apply max-key :best-perc votes) :players)))

(defn recommended-best-perc-players [games collection-game players]
  (let [game (games (game-id collection-game))
        _ (println "recommended" (game-name collection-game))
        votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game game))
        percentages (map (fn [r] (double (+ (r :recommended-perc) (r :best-perc)))) votes)]
    ;; Assuming all games have rating for 1 player
    (if (>= (count percentages) players)
      (nth percentages (dec players))
      (last percentages))))

;; 
;; Sorters
;; 
(defn game-better? [g1 g2]
  (> (game-rating g1) (game-rating g2)))

(defn game-shorter? [g1 g2]
  (< (game-playingtime g1) (game-playingtime g2)))

(defn game-shorter-and-better? [g1 g2]
  (> (/ (game-rating g1) (game-playingtime g1))
     (/ (game-rating g2) (game-playingtime g2))))

;; 
;; Filters
;; 
(defn has-name [name]
  (fn [game]
    (s/includes? (game-name game) name)))

(defn is-best-with-num-of-players [games num]
  (fn [game]
    (let [best-string (best-with-num-of-players games game)]
      (if (s/includes? best-string "+")
        ; remove '+' from the '4+'
        (<= num (read-string (s/join "" (drop-last best-string))))
        (= num (read-string best-string))))))

(defn is-playable-with-num-of-players [games num threshold]
  (fn [game]
    (> (recommended-best-perc-players games game num) threshold)))

(defn playingtime-between? [min max]
  (fn [game]
    (let [time (game-playingtime game)]
      (and
       (number? time)
       (>= time min)
       (<= time max)))))

(defn with-number-of-players? [num]
  (fn [game] (and
              (>= (game-maxplayers game) num)
              (<= (game-minplayers game) num))))

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
  (game-name mar)
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

  (map (fn [r] (double (+ (r :recommended-perc) (r :best-perc)))) votes)


  (map
   (juxt game-name game-rating)
   (sort game-better?
         (filter
          (and-filters
           (with-number-of-players? 3)
           (is-best-with-num-of-players games 3)
           (playingtime-between? 0 120)
           (rating-higher-than? 6))
          collection)))

  (def gb (games "210108"))
  (polls-with-num-of-players-for-game gb)
  (def votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game gb)))
  (apply max-key :best-perc votes)

  (def gbc (first (filter (has-name "Grand Bazaar") collection)))
  (best-with-num-of-players games gbc)
  ((is-best-with-num-of-players games 3) gbc)

  (def game (games (game-id mar)))
  (polls-with-num-of-players-for-game game)
  (def votes
    (map votes-best-rating-per-players (polls-with-num-of-players-for-game game)))

  (map (fn [r] (+ (r :recommended-perc) (r :best-perc))) votes)
  (nth (map (fn [r] (+ (r :recommended-perc) (r :best-perc))) votes) 3)

  ((nth (map
         (fn [r] (+ (r :recommended-perc) (r :best-perc))) votes)
        (dec 3)))


  (recommended-best-perc-players games mar 3)
  (def game10 (collection 10))
  (game-name game10)
  (recommended-best-perc-players games game10 4)
  ((is-playable-with-num-of-players games 4 0.90) game10)

  (def subcollection (take 1 collection))

  ; throws exception
  (map (juxt game-name game-id)
       (sort game-better?
             (filter
              (and-filters
               (with-number-of-players? 5)
               (is-playable-with-num-of-players games 5 0.50)
               (playingtime-between? 0 240))
              collection)))

  (def game10 (collection 10))
  (def tfm (games "167791"))
  (recommended-best-perc-players games tfm 5)
  ((is-playable-with-num-of-players games 4 0.90) game10)


  (clojure.pprint/pp)

  "names of top 10 games in collection"
  (map game-name
       (take 10
             (sort game-better? collection)))

  (take 10
        (map game-name
             (sort game-better?
                   (filter (with-number-of-players? 6) collection))))


  ;(apply  api-read-game (map game-id subcollection))

  (take 10
        (map game-name
             (sort game-better?
                   (filter (is-best-with-num-of-players games 2) collection))))

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

  (game-id (collection 0))
  (game-id (games 0))
  ;;
  )
