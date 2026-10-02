(ns thermal-model
  (:require
   [scicloj.plotje.api :as pj]))

;; # Thermal plant analysis
;;
;; ## Current state

(def params
  {:ambient-temp 20.0

   :heater-lin-coef 1.0
   :sample-lin-coef 0.5

   :heater-rad-coef 4.0e-10
   :sample-rad-coef 2.0e-10})

;; ## Linear heat loss
;;
;; $$Q_{lin}=k(T - T_{amb})$$

(defn linear-loss
  [temperature ambient-temperature coefficient]
  (* coefficient
     (- temperature ambient-temperature)))

;; ## Radiative heat loss
;;
;; $$Q_{rad}=k_{rad}(T^4-T_{amb}^4)$$

(defn radiative-loss
  [temperature ambient-temperature coefficient]
  (let [temp-k (+ temperature 273.15)
        ambient-k (+ ambient-temperature 273.15)]
    (* coefficient
       (- (Math/pow temp-k 4)
          (Math/pow ambient-k 4)))))

(defn calculate-losses
  [temperatures ambient lin-coef rad-coef]
  (let [linear-losses (map #(linear-loss % ambient lin-coef) temperatures)
        radiative-losses (map #(radiative-loss % ambient rad-coef) temperatures)
        total-losses (map + linear-losses radiative-losses)]
    {:linear linear-losses
     :radiation radiative-losses
     :total total-losses}))

(defn losses->plot-data
  [temperatures losses body]
  (mapcat
   (fn [[kind values]]
     (map (fn [temperature loss]
            {:temperature temperature
             :loss loss
             :kind kind
             :body body})
          temperatures
          values))
   losses))

(def temperatures
  (range 20 201 10))

(def heater-losses
  (calculate-losses temperatures
                    (:ambient-temp params)
                    (:heater-lin-coef params)
                    (:heater-rad-coef params)))

(def sample-losses
  (calculate-losses temperatures
                    (:ambient-temp params)
                    (:sample-lin-coef params)
                    (:sample-rad-coef params)))

(def heater-loss-data
  (losses->plot-data temperatures heater-losses :heater))

(def sample-loss-data
  (losses->plot-data temperatures sample-losses :sample))

(def all-loss-data (concat heater-loss-data sample-loss-data))

(-> all-loss-data
    (pj/lay-line :temperature :loss {:color :body})
    (pj/facet :kind)
    (pj/options
     {:title "Thermal losses"
      :x-label "Temperature, °C"
      :y-label "Heat loss, W"
      :color-label "Body"}))
