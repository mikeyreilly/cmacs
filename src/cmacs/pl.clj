(ns cmacs.pg
  (:require
   [com.rpl.specter :as s :refer :all]
   [clojure.pprint :refer :all]))

(defn print-results [x]
  (pprint x))

(defn print-manual [x]
  (pprint x))

(print-manual
 (->> {:a 1 :b 2 :c 3}
      (map (fn [[k v]] [k (inc v)]))
      (into {})))

(defn map-vals [afn m]
  (->> m
       (map (fn [[k v]] [k (afn v)]))
       (into {})))

(print-results
 (map-vals inc {:a 1 :b 2 :c 3}))

(print-results (transform [ALL LAST] inc {:a 1 :b 2 :c 3}))

(def MAP_VALS [ALL LAST])

(print-results (transform MAP_VALS inc {:a 1 :b 2 :c 3}))
