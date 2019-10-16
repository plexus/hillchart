(ns hillchart.main
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(when (= "" js/document.location.hash)
  (set! js/document.location.hash (random-uuid)))

(def chart-id (subs js/document.location.hash 1))

(defonce state (r/atom {}))

(defn query-viewport-size []
  {:screen/width js/document.documentElement.clientWidth
   :screen/height js/document.documentElement.clientHeight})

(set! js/document.body.onresize (fn [_] (swap! state merge (query-viewport-size))))

(def hill-curve [#_M [15 70] ;; start
                 #_C [40 70] [70 30] [90 30] ;; control/control/end
                 [110 30] [140 70] [165 70]]) ;; control/control/end

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

(defn js->xy [^js obj]
  [(.-x obj) (.-y obj)])

(defn nearest-point-on-curve [point curve]
  (let [^js obj (js/jsBezier.nearestPointOnCurve (xy->js point) (xys->js curve))]
    {:point (js->xy (.-point obj))
     :location (.-location obj)}))

(defn distance-from-curve [point curve]
  (let [^js obj (js/jsBezier.distanceFromCurve (xy->js point) (xys->js curve))]
    (.-distance obj)))

(defn point-on-curve [curve location]
  (let [^js obj (js/jsBezier.pointOnCurve (xys->js curve) (- 1 location))] ;; for some reason jsBezier handles this location backwards
    (js->xy obj)))

(defn nearest-point-on-hill [point]
  (let [c1 (take 4 hill-curve)
        c2 (drop 3 hill-curve)
        d1 (distance-from-curve point c1)
        d2 (distance-from-curve point c2)]
    (if (< d1 d2)
      (:point (nearest-point-on-curve point c1))
      (:point (nearest-point-on-curve point c2)))))

(defn location-on-hill [point]
  (let [c1 (take 4 hill-curve)
        c2 (drop 3 hill-curve)
        d1 (distance-from-curve point c1)
        d2 (distance-from-curve point c2)]
    (if (< d1 d2)
      (/ (:location (nearest-point-on-curve point c1)) 2)
      (+ (/ (:location (nearest-point-on-curve point c2)) 2) 0.5))))

(defn point-on-hill [location]
  (let [c1 (take 4 hill-curve)
        c2 (drop 3 hill-curve)]
    (if (< location 0.5)
      (point-on-curve c1 (* location 2))
      (point-on-curve c2 (* (- location 0.5) 2)))))

(defn rand-dot-color []
  (let [existing (set (map :color (:dots @state)))
        color    (get colors (rand-nth dot-colors))]
    (if (and (some #{color} existing)
             (seq (remove existing (vals colors))))
      (recur)
      color)))

(defn svg-pos [^js svg e]
  (when svg
    (let [pt (.createSVGPoint svg)]
      (set! (.-x pt) (.-clientX e))
      (set! (.-y pt) (.-clientY e))
      (let [coord (.matrixTransform pt (.inverse (.getScreenCTM svg)))]
        [(.-x coord) (.-y coord)]))))

(declare save-doc!)

(defn add-new-dot [{:keys [position] :as state}]
  (update state :dots conj
          {:color (rand-dot-color)
           :position (location-on-hill position)
           :label "Create MVP"
           :moving? true}))

(defn add-new-dot! [_]
  (swap! state add-new-dot)
  (save-doc!))

(defn place-dot [state]
  (update state :dots (partial map #(dissoc % :moving?))))

(defn place-dot! [e]
  (swap! state place-dot)
  (save-doc!))

(defn move-dot [dot state]
  (update state
          :dots
          (partial map (fn [d]
                         (if (= d dot)
                           (assoc d :moving? true)
                           d)))))

(defn move-dot! [{:keys [color] :as dot}]
  (fn [e]
    (swap! state (partial move-dot dot))))

(defn update-position! [pos]
  (swap! state
         (fn [state]
           (update state
                   :dots
                   (partial map (fn [dot]
                                  (if (:moving? dot)
                                    (assoc dot :location (location-on-hill pos))
                                    dot)))))))

(defn Dot [{:keys [color position on-click label]}]
  (let [[x y] position]
    [:g
     [:circle {:cx x
               :cy y
               :r 5
               :style {:fill (rgb->css color)
                       :opacity 0.8}
               :on-click on-click}]
     [:text {:x (+ x 6)
             :y (+ y 0.8)
             :text-anchor "right"
             :font-family "sans-serif"
             :font-size 4
             ;;:font-weight 600
             :fill (rgb->css (colors :black))
             :style {:-webkit-user-select "none"
                     :-moz-user-select "none"}}
      label]]))

(defn Hill []
  [:path {:d (str "m " (xy->svg (first hill-curve)) " "
                  "C " (xys->svg (rest hill-curve)))
          :style {:fill "none"
                  :stroke (rgb->css (colors :black))
                  :stroke-width 1.5
                  :stroke-linecap "butt"
                  :stroke-linejoin "miter"
                  :stroke-opacity 1}}])

(defn AddDotButton []
  (let [button-x 20 button-y 20]
    [:g
     [:circle
      {:cx button-x
       :cy button-y
       :r 5
       :style {:fill (rgb->css (colors :red))}
       :on-click add-new-dot!}]

     [:line {:x1 (- button-x 3)
             :x2 (+ button-x 3)
             :y1 button-y
             :y2 button-y
             :style {:stroke (rgb->css (colors :black))}
             :on-click add-new-dot!}]
     [:line {:y1 (- button-y 3)
             :y2 (+ button-y 3)
             :x1 button-x
             :x2 button-x
             :style {:stroke (rgb->css (colors :black))}
             :on-click add-new-dot!}]]))

(defn Chart [state]
  (let [{:screen/keys [width height]} state]
    [:svg  (let [svg (atom nil)]
             {:width width
              :height (- height 100)
              :viewBox "0 0 180 80"
              :on-mouse-move (fn [e] (update-position! (svg-pos @svg e)))
              ;; :on-click place-dot!
              :ref #(reset! svg %)})

     [AddDotButton]
     [Hill]
     (doall
      (for [{:keys [color location moving? label] :as dot} (:dots state)]
        ^{:key (str location color)}
        [Dot {:color color
              ;;:label label
              :position (point-on-hill location)
              :on-click (if moving?
                          place-dot!
                          (move-dot! dot))}]))]))

(defn Main [state]
  [:div
   [Chart state]
   [:footer [:p "Hillchart.io by "
             [:a {:href "http://gaiwan.co"} "Gaiwan"]
             ", with props to Basecamp and "
             [:a {:href "https://basecamp.com/shapeup/3.4-chapter-12#work-is-like-a-hill"} "Shape Up"]
             "."]

    #_[:pre
       (pr-str @state) "\n"
       ]]])

(r/render [(fn [] [Main @state])] (js/document.getElementById "app"))

(when (exists? js/firebase)
  (def ^js db (js/firebase.firestore))
  (def ^js fs-charts (.collection db "charts"))
  (def ^js fs-doc (.doc fs-charts chart-id))

  (defn fs->state! [^js doc]
    (when (.-exists doc)
      (swap! state merge (js->clj (.data doc) :keywordize-keys true))))

  (defonce fetch-doc
    (.then
     (.get fs-doc)
     fs->state!))

  (defonce setup-listener
    (.onSnapshot fs-doc fs->state!)))

(defn save-doc! []
  (when (exists? fs-doc)
    (.set fs-doc (clj->js (select-keys @state [:dots])))))

(swap! state merge (query-viewport-size))
