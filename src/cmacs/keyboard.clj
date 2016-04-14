(ns cmacs.keyboard
  (:require [clojure.string :as str])
  (:import [java.awt.event InputEvent]
           [java.lang.reflect Modifier]))

(def all-modifiers-mask 960)

(defrecord KeyStroke [key-code modifiers])

(defn matches [key-stroke key-event]
  (and (= (:key-code key-stroke) (.getKeyCode key-event) )
       (= (:modifiers key-stroke) (bit-and all-modifiers-mask (.getModifiersEx key-event)))))

(defn ^:private modifier "convert a string like 'C-S' to
  InputEvent/CTRL_DOWN_MASK | InputEvent/SHIFT_DOWN_MASK"
  [c]
  (case c
    \- 0
    \C InputEvent/CTRL_DOWN_MASK
    \S InputEvent/SHIFT_DOWN_MASK
    \s InputEvent/META_DOWN_MASK
    \M InputEvent/ALT_DOWN_MASK))

(defn ^:private modifiers [s] (reduce bit-or (map modifier s)))

(def ^:private lookup-keycode
  (memoize
    (fn [s] (-> java.awt.event.KeyEvent
                (.getDeclaredField (str "VK_" (.toUpperCase s))) (.getInt nil)))))

(def ^:private lookup-name
  (memoize
    (fn [keyCode]
      (let [expected_modifiers
            (bit-or Modifier/PUBLIC Modifier/STATIC Modifier/FINAL)
            fields  (.getDeclaredFields java.awt.event.KeyEvent)
            f (first (filter #(and
                                (= expected_modifiers (.getModifiers %))
                                (= Integer/TYPE (.getType %))
                                (= keyCode (.getInt % nil))
                                (= (-> (.getName %) (.beginsWith "VK_")))) fields))]
        (if f (.substring (.getName f) 3) "UNKNOWN")))))

(defn kbd
  "converts a string to a KeyStroke"
  [s]
  (vec (for [w (str/split s #" ")]
         (let [dash-index (.lastIndexOf w (int \-))]
           (case dash-index
             -1 (KeyStroke. (lookup-keycode w) 0)
             (KeyStroke.
               (lookup-keycode (.substring w (inc dash-index)))
               (modifiers (.substring w 0 dash-index))))))))

(defn KeyStroke->String [ks]
  (let [m (:modifiers ks)]
    (str
      (apply str (flatten
                   (for [x "CSsM"
                         :let [y (when (pos? (bit-and m (modifier x))) [x \-])]]
                     y)))
      (lookup-name (:key-code ks)))))


(defn KeyEvent->KeyStroke[e]
  (KeyStroke.
    (.getKeyCode e)
    (bit-and (.getModifiersEx e) all-modifiers-mask)))
