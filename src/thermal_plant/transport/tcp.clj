(ns thermal-plant.transport.tcp
  (:require
   [clojure.data.json :as json])
  (:import
   [java.io BufferedReader BufferedWriter
    InputStreamReader OutputStreamWriter]
   [java.net Socket]
   [java.nio.charset StandardCharsets]))

;;----------------------------------------------------------------------
;; CONNECTION
;; ---------------------------------------------------------------------

(defn open!
  "Open usual TCP-connection."
  [host port]
  (let [socket (Socket. host port)
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

;;----------------------------------------------------------------------
;; SEND / RECEIVE
;; ---------------------------------------------------------------------

(defn send!
  "Send one Clojure map as a JSON-string."
  [{:keys [writer]} message]
  (.write writer (json/write-str message))
  (.newLine writer)
  (.flush writer))

(defn receive!
  "Receive one JSON-string and covert it to Clojure map."
  [{:keys [reader]}]
  (let [line (.readLine reader)]
    (json/read-str line :key-fn keyword)))

;;----------------------------------------------------------------------
;; CLOSE
;; ---------------------------------------------------------------------

(defn close!
  "Close TCP-connection"
  [{:keys [socket]}]
  (.close socket))
