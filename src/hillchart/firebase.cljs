(ns hillchart.firebase
  (:require ["firebase-functions" :as functions]
            ["firebase-admin" :as admin]))

(.initializeApp admin)

(defn render-chart-svg [^js req, ^js res]
  (let [db (.firestore admin)
        fs-charts (.collection db "charts")
        fs-doc (.doc fs-charts (.. req -query -chart_id))]
    (.then (.get fs-doc)
           (fn [doc]
             (.send res (pr-str (js->clj (.data doc))))))))


(def cloud-functions
  #js {:renderChartSVG (.onRequest functions/https render-chart-svg)})
