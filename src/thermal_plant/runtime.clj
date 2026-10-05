(ns thermal-plant.runtime
  (:require
    [clojure.data.json :as json])
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

(defn close! [{:keys [socket]}]
  (.close socket))

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
