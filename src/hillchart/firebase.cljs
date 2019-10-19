(ns hillchart.firebase
  (:require ["firebase-functions" :as functions]
            ["firebase-admin" :as admin]
            [hillchart.main :as main]
            [reagent.dom.server]
            [clojure.string :as str]))

(.initializeApp admin)

(defn render-chart-svg [^js req, ^js res]
  (let [doc-id (str/replace (str/replace (.-path req) ".svg" "") "/" "")
        ^js db (.firestore admin)
        ^js fs-charts (.collection db "charts")
        ^js fs-doc (.doc fs-charts doc-id)]
    (.then (.get fs-doc)
           (fn [doc]
             (.type res ".svg")
             (.send res
                    (reagent.dom.server/render-to-static-markup
                     [main/Chart (assoc (js->clj (.data doc) :keywordize-keys true)
                                   :screen/width 500
                                   :screen/height 300
                                   :static? true)]))))))


(def cloud-functions
  #js {:renderChartSVG (.onRequest functions/https render-chart-svg)})
