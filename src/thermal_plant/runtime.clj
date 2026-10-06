(ns thermal-plant.runtime
  (:require
   [clojure.data.json :as json]
   [thermal-plant.ws :as ws])
  (:import
   [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
   [java.net Socket]
   [java.nio.charset StandardCharsets]))

(defn connect [host port]
  (let  [socket (Socket. host port)
         reader (BufferedReader.
                 (InputStreamReader.
                  (.getInputStream socket)
                  StandardCharsets/UTF_8))
         writer (BufferedWriter.
                 (OutputStreamWriter.
                  (.getOutputStream socket)
                  StandardCharsets/UTF_8))]
    {:socket socket
     :reader reader
     :writer writer}))

(defn close!
  [client]
  (case (:transport client)
    :tcp
    (.close ^Socket (:socket client))

    :websocket
    (ws/close! (:ws client))))

(defn send-line! [{:keys [writer]} text]
  (.write writer text)
  (.newLine writer)
  (.flush writer))

(defn read-line! [{:keys [reader]}]
  (.readLine reader))

(defn send-message! [connection message]
  (send-line! connection
              (json/write-str message)))

(defn read-message! [connection]
  (json/read-str
   (read-line! connection)
   :key-fn keyword))

(defn request! [connection message]
  (send-message! connection message)
  (read-message! connection))

(defn hello! [connection]
  (request! connection
            {:v 1
             :msg_id "hello"
             :op "hello"
             :args {:scope nil}}))

(defn open-session [host port]
  (let [connection (connect host port)
        hello-response (hello! connection)
        result (:result hello-response)]
    (assoc connection
           :scope (:scope result)
           :next-seq (:next_seq result))))

(defn latest! [connection signal]
  (request!
   connection
   {:v 1
    :msg_id "latest"
    :op "latest"
    :args {:signal signal}}))

(defn latest-value! [connection signal]
  (get-in (latest! connection signal)
          [:result :value]))

(defn discover! [connection]
  (request! connection
            {:v 1
             :msg_id "discover"
             :op "discover"
             :args {}}))

(defn instruments [discovery-response]
  (->> (get-in discovery-response [:result :records])
       (filter #(= "instrument" (:kind %)))
       (mapv #(select-keys % [:id :name]))))

(defn signals [discovery-response instrument-id]
  (->> (get-in discovery-response [:result :records])
       (filter #(and (= "signal" (:kind %))
                     (= instrument-id (:instrument %))))
       (mapv #(select-keys % [:id :name :unit :signal_kind]))))

(defn outputs [discovery-response instrument-id]
  (->> (get-in discovery-response [:result :records])
       (filter #(and (= "output" (:kind %))
                     (= instrument-id (get-in % [:id :instrument]))))
       (mapv #(select-keys % [:id :state]))))

(comment

  ;; Открыть одну сессию для интерактивной работы в REPL.
  ;; Выполнить один раз после reload namespace.
  (def session
    (open-session "127.0.0.1" 8765))

  ;; Посмотреть основные данные сессии.
  (select-keys session [:scope :next-seq])

  ;; Получить полный discovery response.
  (def discovery
    (discover! session))

  ;; Посмотреть доступные инструменты без лишних метаданных.
  (instruments discovery)

  ;; Прочитать полное последнее измерение температуры
  ;; виртуальной печи.
  (latest!
   session
   {:instrument "1"
    :parameter "1"})

  ;; Получить только числовое значение температуры.
  (latest-value!
   session
   {:instrument "1"
    :parameter "1"})

  ;; Прочитать сглаженную температуру от Native moving mean.
  (latest-value!
   session
   {:instrument "202"
    :parameter "1"})

  ;; Если discovery нужно обновить.
  (def discovery
    (discover! session))

  ;; Закрыть сессию после окончания работы.
  (close! session))
