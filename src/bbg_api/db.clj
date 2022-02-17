(ns bbg-api.db
  (:require [bbg-api.core :as g]
            [clojure.pprint :as pp]))

(defn collection-game->game
  [games collection-game]
  (let [game-id (g/game-id collection-game)
        votes (map g/votes-best-rating-per-players
                   (g/polls-with-num-of-players-for-game (games game-id)))]
    {:id game-id
     :name (g/game-name collection-game)
     :rating (g/game-rating collection-game)
     :my-rating (g/game-my-rating collection-game)
     :minplayers ((g/game-attribute collection-game) :minplayers)
     :maxplayers ((g/game-attribute collection-game) :maxplayers)
     :playingtime ((g/game-attribute collection-game) :playingtime)
     :votes votes}))

(defn make-db-from-collection-and-games
  [collection games]
  (let [all (map #(collection-game->game games %) collection)
        all-db (reduce
                #(assoc %1 (:id %2) %2)
                {} all)]
    (spit "resources/db.clj" (with-out-str (pp/pprint all-db)))))

(defn make-db 
  []
  (make-db-from-collection-and-games 
   (g/read-collection-from-file) 
   (g/read-games-from-file)))

(defn read-db
  []
  (read-string (slurp "resources/db.clj")))

(comment
  ;; (def collection (g/read-collection-from-file))
  ;; (def games (g/read-games-from-file))
  ;; (make-db-from-collection-and-games collection games)

  (make-db)

  (def db (read-db))
  (db "25613")
  (pp/pp)
  (def collection (vals db))

  (take 2 collection)
  (clojure.pprint/pp)
  ;
  )