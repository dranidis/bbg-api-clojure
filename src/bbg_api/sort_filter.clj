(ns bbg-api.sort-filter 
  (:require [bbg-api.db :as db]
            [clojure.pprint :as pp]))


(defn all-games
  []
  (vals (db/read-db)))

(comment
  
 (db/make-db)
 (def db-mem (all-games))
  
  ;
 )