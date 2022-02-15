(ns bbg-api.core
  (:gen-class)
  (:require [clojure.xml :as xml])
  (:require  [clojure.edn :as edn]))

(defn get-collection-and-write-to-file []
  (spit "resources/ddmits.clj" 
        (xml/parse "https://boardgamegeek.com/xmlapi/collection/ddmits")))

 
(defn api-read-game [game-id]
  (-> (xml/parse (str "https://boardgamegeek.com/xmlapi/boardgame/" game-id))
      :content
      first))
 

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

(defn game-id [game]
  (get-in game [:attrs :objectid]))

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

"Num of players"
  (defn polls-with-num-of-players-for-game [game]
    (let [tag-list (game :content)
          tag-poll (filter (fn [x] (= (x :tag) :poll)) tag-list)
          recommended (filter (fn [x] (= (get-in x [:attrs :name]) "suggested_numplayers")) tag-poll)]
      ((first recommended) :content)))


(defn votes-best-rating-per-players [data]
  (let [players (get-in data [:attrs :numplayers])
        total-votes (apply + (map (fn [x] (read-string (get-in x [:attrs :numvotes]))) (data :content)))
        best-votes (read-string (get-in (first (data :content)) [:attrs :numvotes]))]

    {:players players :best-voting best-votes :best-perc (/ best-votes total-votes)}))

(defn best-with-num-of-players [game]
  (let [votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game game))]
    (read-string ((apply max-key :best-perc votes) :players))))



"Filters"
(defn is-best-with-num-of-players [num]
  (fn [game]
    (= num (best-with-num-of-players game))))

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
  (def pl (api-read-game "161936"))
  (def tta (api-read-game "182028"))

  pl

  (def list-pl (-> pl
                   :content))

  (def list (filter (fn [x] (= (x :tag) :poll)) list-pl))
  (def list (filter (fn [x] (= (get-in x [:attrs :name]) "suggested_numplayers")) list))
  list




  (polls-with-num-of-players-for-game pl)
  (map votes-best-rating-per-players (polls-with-num-of-players-for-game pl))
  (def votes (map votes-best-rating-per-players (polls-with-num-of-players-for-game pl)))
  ((apply max-key :best-perc votes) :players)

  (best-with-num-of-players pl)
  (best-with-num-of-players tta)
  ((is-best-with-num-of-players 3) pl)

  (clojure.pprint/pp)

  (nth list-pl 20)

  (get-collection-and-write-to-file)
  (def collection (get-games-from-collection (read-collection-from-file)))
  (def game (first collection))
  (def game (nth collection 1))
  game
  (game-name game)
  (game-rating game)
  (has-rating? game)

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
                   (filter (with-number-of-players? 2) collection))))




  (def subcollection (take 10 collection))
  game-id

  (apply  api-read-game (map game-id subcollection))


  (take 10
        (map game-name
             (sort game-better?
                   (filter (is-best-with-num-of-players 2) subcollection))))

  (is-best-with-num-of-players 2)
  (best-with-num-of-players tta)

  (map game-name
       (sort game-better?
             (filter
              (and-filters
               (with-number-of-players? 4)
               (playingtime-between? 0 60)
               (rating-higher-than? 7.5))
              collection)))

  (def all (reduce #(assoc %1 (%2 :id) %2) {} [{:id 11 :body "1"} {:id 12 :body "2"}  {:id 13 :body "3"}]))
  (all 13)
  

  (reduce #(assoc %1 :id (%2 :id) :val %2) {} [{:id 1 :body "1"}])

  (+ 1 1)



  ;;
  )

