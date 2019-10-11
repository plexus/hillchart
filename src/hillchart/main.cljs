(ns hillchart.main
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defonce state (r/atom {}))

(def hill-curve [#_M [20 70] ;; start
                 #_C [40 70] [70 30] [90 30] ;; control/control/end
                 [110 30] [140 70] [160 70]]) ;; control/control/end

(def colors
  {:black [44 44 40]
   :white [241 242 238]
   :gray1 [200 205 202]
   :gray2 [163 160 150]
   :gray3 [118 120 115]
   :warm-gray [82 81 69]
   :blue1 [134 190 215]
   :blue2 [89 145 193]
   :blue3 [60 98 165]
   :green1 [150 178 107]
   :green2 [96 130 84]
   :sand [230 216 166]
   :yellow [227 207 85]
   :tan [213 173 134]
   :red [211 72 44]
   :orange1 [215 124 75]
   :orange2 [214 159 77]
   :brown1 [149 112 75]
   :brown2 [121 62 40]
   :brown3 [61 29 44]})

(def dot-colors [:blue1 :brown2 :green2 #_:gray1 #_:sand :tan #_:white #_:gray3 #_:yellow
                 :brown1 :brown3 :green1 :blue2 :orange1 :orange2 #_:gray2 :red :blue3 #_:warm-gray #_:black])

(defn rgb->css [[r g b]]
  (str "rgb(" r "," g "," b ")"))

(defn xy->js [[x y]]
  #js {:x x :y y})

(defn xys->js [points]
  (apply array (map xy->js points)))

(defn xy->svg [[x y]]
  (str x "," y))

(defn xys->svg [points]
  (str/join " " (map xy->svg points)))

(defn js->xy [^:js obj]
  [(.-x obj) (.-y obj)])

(defn nearest-point-on-curve [point curve]
  (let [^:js obj (js/jsBezier.nearestPointOnCurve (xy->js point) (xys->js curve))]
    (js->xy (.-point obj))))

(defn distance-from-curve [point curve]
  (let [^:js obj (js/jsBezier.distanceFromCurve (xy->js point) (xys->js curve))]
    (.-distance obj)))

(defn point-on-curve [curve location]
  (let [^:js obj (js/jsBezier.pointOnCurve (xys->js curve) location)]
    (js->xy obj)))

(defn nearest-point-on-hill [point]
  (let [c1 (take 4 hill-curve)
        c2 (drop 3 hill-curve)
        d1 (distance-from-curve point c1)
        d2 (distance-from-curve point c2)]
    (if (< d1 d2)
      (nearest-point-on-curve point c1)
      (nearest-point-on-curve point c2))))


(defn rand-dot-color []
  (let [candidates (remove (set (map :color (:dots @state))) dot-colors)]
    (or (rand-nth candidates) (rand-nth dot-colors))))


(defn svg-pos [^:js svg e]
  (when svg
    (let [pt (.createSVGPoint svg)]
      (set! (.-x pt) (.-clientX e))
      (set! (.-y pt) (.-clientY e))
      (let [coord (.matrixTransform pt (.inverse (.getScreenCTM svg)))]
        [(.-x coord) (.-y coord)]))))

(r/render
 [(fn []
    [:div
     [:svg  (let [svg (atom nil)]
              {:width "1100px"
               :height "500px"
               :viewBox "0 0 200 100"
               :on-mouse-move (fn [e] (swap! state assoc :position (svg-pos @svg e)))
               :ref #(reset! svg %)})
      [:circle
       {:cx 10
        :cy 10
        :r 5
        :style {:fill (rgb->css (colors :red))}
        :on-click #(swap! state assoc :moving (rand-dot-color))}]

      [:line {:x1 7 :x2 13 :y1 10 :y2 10 :style {:stroke (rgb->css (colors :black))}}]
      [:line {:y1 7 :y2 13 :x1 10 :x2 10 :style {:stroke (rgb->css (colors :black))}}]

      [:path {:d (str "m " (xy->svg (first hill-curve)) " "
                      "C " (xys->svg (rest hill-curve)))
              :style {:fill "none"
                      :stroke (rgb->css (colors :black))
                      :stroke-width 1.5
                      :stroke-linecap "butt"
                      :stroke-linejoin "miter"
                      :stroke-opacity 1}}]

      (let [{:keys [moving position]} @state]
        (when (and moving position)
          [:circle (let [[x y] (nearest-point-on-hill position)]
                     {:cx x
                      :cy y
                      :r 5
                      :style {:fill (rgb->css (colors moving))
                              :opacity 0.8}
                      :on-click #(swap! state
                                        (fn [state]
                                          (-> state
                                              (update :dots conj {:color moving
                                                                  :position [x y]})
                                              (dissoc :moving))))})]))

      (doall
       (for [{:keys [color position] :as dot} (:dots @state)
             :let [[x y] position]]
         ^{:key (pr-str dot)}
         [:circle {:cx x
                   :cy y
                   :r 5
                   :style {:fill (rgb->css (colors color))
                           :opacity 0.8}
                   :on-click #(swap! state
                                     (fn [state]
                                       (-> state
                                           (update :dots (partial remove #{dot}))
                                           (assoc :moving color))))}]))]

     [:pre
      (pr-str @state) "\n"
      ]])]
 (js/document.getElementById "app"))
