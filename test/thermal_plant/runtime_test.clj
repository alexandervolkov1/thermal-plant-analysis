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

(deftest reference-retune-test
  (let [client (runtime/connect! tcp-config)
        before (atom nil)]

    (try
      (reset! before
              (runtime/reference! client "1"))

      (let [{:keys [revision]} @before]

        (testing "Reference can be retuned"
          (runtime/retune-reference!
           client
           "1"
           revision
           55.0
           2.0)

          (let [after (runtime/reference! client "1")]
            (is (= 55.0 (:target after)))
            (is (= 2.0 (:rate after)))
            (is (not= revision
                      (:revision after))))))

      (finally
        ;; Restore Reference if we managed to read its initial state.
        (when @before
          (let [current (runtime/reference! client "1")]
            (runtime/retune-reference!
             client
             "1"
             (:revision current)
             (:target @before)
             (:rate @before))))

        (runtime/close! client)))))
