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
