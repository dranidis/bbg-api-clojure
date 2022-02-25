(ns bbg-api.db
  (:require [clojure.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.string :as s]))
;; 
;; Fields accessors from API XML
;; 
(defn- game-id [game]
  (get-in game [:attrs :objectid]))

(defn- game-my-rating [collection-game]
  (let [rating (-> collection-game
                   :content
                   (nth 4)
                   :content
                   first
                   :attrs
                   :value
                   read-string)]
    (if (number? rating) rating nil)))

(defn- game-rating [collection-game]
  (let [rating (-> collection-game
                   :content
                   (nth 4)
                   :content
                   first
                   :content
                   (second)
                   :attrs
                   :value
                   read-string)]
    (if (number? rating) rating nil)))

(defn- game-name [collection-game]
  (-> collection-game
      :content
      first
      :content
      first))

(defn- game-attributes [collection-game]
  (-> collection-game
      :content
      (nth 4)
      :attrs))

(defn- game-attribute [collection-game]
  (fn [key]
    (let [attr (game-attributes collection-game)
          value (attr key)]
      (if value (read-string value) nil))))

(comment
  ;

(defn parse-fn 
  [{:keys [tag]}]
  tag)
1
   ;
  )




;; (defn game-playingtime [collection-game]
;;   ((game-attribute collection-game) :playingtime))

;; (defn game-maxplayers [collection-game]
;;   ((game-attribute collection-game) :maxplayers))

;; (defn game-minplayers [collection-game]
;;   ((game-attribute collection-game) :minplayers))

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
;; Collection API 
;; 
(defn fetch-collection-and-write-to-file [user-name]
  (spit "resources/collection.clj"
        (xml/parse (str "https://boardgamegeek.com/xmlapi/collection/" user-name))))

(defn read-collection-from-file []
  (:content (read-string (slurp "resources/collection.clj"))))

;; 
;; Functions for numbers of players
;; 
(defn- polls-with-num-of-players-for-game [game]
  (let [tag-list (game :content)
        tag-poll (filter (fn [x] (= (x :tag) :poll)) tag-list)
        recommended (filter (fn [x] (= (get-in x [:attrs :name]) "suggested_numplayers")) tag-poll)]
    ((first recommended) :content)))

(defn- votes-best-rating-per-players [data]
  (let [players (get-in data [:attrs :numplayers])
        total-votes (apply + (map (fn [x] (read-string (get-in x [:attrs :numvotes]))) (data :content)))
        best-votes (read-string (get-in (first (data :content)) [:attrs :numvotes]))
        best-perc (if (= 0 total-votes) 0 (double (/ best-votes total-votes)))

        recommended-votes (read-string (get-in (second (data :content)) [:attrs :numvotes]))
        recommended-perc (if (= 0 total-votes) 0 (double (/ recommended-votes total-votes)))
        not-recommended-votes (read-string (get-in (last (data :content)) [:attrs :numvotes]))
        not-recommended-perc (if (= 0 total-votes) 0 (double (/ not-recommended-votes total-votes)))]
    {:players players
     :best-votes best-votes :best-perc best-perc
     :recommended-votes recommended-votes :recommended-perc recommended-perc
     :not-recommended-votes not-recommended-votes :not-recommended-perc not-recommended-perc}))


;; 
;; API - DB
;; 
(defn- collection-game->game
  [games collection-game]
  (let [game-id (game-id collection-game)
        votes (into [] (map votes-best-rating-per-players
                   (polls-with-num-of-players-for-game (games game-id))))]
    {:id game-id
     :name (game-name collection-game)
     :rating (game-rating collection-game)
     :my-rating (game-my-rating collection-game)
     :minplayers ((game-attribute collection-game) :minplayers)
     :maxplayers ((game-attribute collection-game) :maxplayers)
     :playingtime ((game-attribute collection-game) :playingtime)
     :votes votes}))

(defn- make-db-from-collection-and-games
  [collection games]
  (let [all (map #(collection-game->game games %) collection)
        all-db (reduce
                #(assoc %1 (:id %2) %2)
                {} all)]
    (spit "resources/db.clj" (with-out-str (pp/pprint all-db)))))

(defn make-db
  []
  (make-db-from-collection-and-games
   (read-collection-from-file)
   (read-games-from-file)))

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

  (def collection (read-collection-from-file))
  (first collection)
  
  (filter (fn [game]
    (s/includes? (game-name game) "Caverna")) collection)
  
  (def db (read-db))
  (db "102794")
  
  (map :name collection)
  

  (clojure.pprint/pp)
  ;
  )