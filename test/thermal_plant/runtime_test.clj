(ns thermal-plant.runtime-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [thermal-plant.runtime :as runtime]))

(def tcp-config
  {:transport :tcp
   :host "127.0.0.1"
   :port 8765})

(deftest tcp-connect-test
  (let [client (runtime/connect! tcp-config)]
    (try
      (testing "Runtime connection is ready"
        (is (= "ready"
               (get-in client [:hello :state])))

        (is (string? (:scope client)))

        (is (string? (:boot-id client))))

      (testing "Runtime can read temperature"
        (let [latest
              (runtime/latest!
               client
               {:instrument "1"
                :parameter "1"})]

          (is (= "available" (:status latest)))

          (is (= "good" (:quality latest)))

          (is (number? (:value latest)))))

      (finally
        (runtime/close! client)))))

(defn- same-reference-policy?
  "Return true when kind-specific configuration matches."
  [expected actual]
  (and (= (:kind expected) (:kind actual))
       (case (:kind expected)
         "fixed"
         (= (:value expected) (:value actual))

         "ramp"
         (and (= (:target expected) (:target actual))
              (= (:rate expected) (:rate actual)))

         false)))

(defn- restore-reference!
  "Restore the original Reference policy, if it changed."
  [client reference-id original]
  (let [current (runtime/reference! client reference-id)]
    (when-not (same-reference-policy? original current)
      (let [config
            (case (:kind original)
              "fixed"
              {:kind "fixed"
               :value (:value original)}

              "ramp"
              {:kind "ramp"
               :target (:target original)
               :rate (:rate original)}

              (throw
               (ex-info "Unsupported original Reference kind"
                        {:original original})))]
        (runtime/configure-reference!
         client
         reference-id
         (:revision current)
         config)))))

(deftest reference-retune-test
  (let [client (runtime/connect! tcp-config)
        before (atom nil)]
    (try
      (reset! before
              (runtime/reference! client "1"))

      (testing "Reference can be configured as a ramp from any initial mode"
        (let [configured
              (runtime/configure-reference!
               client
               "1"
               (:revision @before)
               {:kind "ramp"
                :target 50.0
                :rate 2.0})]
          (is (= "ramp" (:kind configured)))
          (is (= 50.0 (:target configured)))
          (is (= 2.0 (:rate configured)))

          (testing "Ramp Reference can be retuned"
            (let [retuned
                  (runtime/retune-reference!
                   client
                   "1"
                   (:revision configured)
                   55.0
                   2.0)]
              (is (= 55.0 (:target retuned)))
              (is (= 2.0 (:rate retuned)))
              (is (not= (:revision configured)
                        (:revision retuned))))))

      (finally
        ;; Cleanup errors become test failures instead of masking the
        ;; original assertion or exception.
        (when-let [original @before]
          (try
            (restore-reference! client "1" original)
            (catch Exception error
              (is false
                  (str "Unable to restore Reference: "
                       (.getMessage error)
                       " "
                       (pr-str (ex-data error)))))))

        (try
          (runtime/close! client)
          (catch Exception error
            (is false
                (str "Unable to close Runtime connection: "
                     (.getMessage error))))))))
