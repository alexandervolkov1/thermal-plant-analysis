(ns thermal-plant.transport.websocket
  (:require
   [clojure.data.json :as json])
  (:import
   [java.net URI]
   [java.net.http HttpClient WebSocket WebSocket$Listener]
   [java.util.concurrent CompletableFuture LinkedBlockingQueue]))

;;----------------------------------------------------------------------
;; LISTENER HELPERS
;; ---------------------------------------------------------------------

(defn- completed []
  (CompletableFuture/completedFuture nil))

;;----------------------------------------------------------------------
;; CONNECTION
;; ---------------------------------------------------------------------

(defn open!
  "Open WebSocket-connection"
  [url origin]
  (let [messages (LinkedBlockingQueue.)
        buffer (StringBuilder.)

        listener
        (reify WebSocket$Listener
          (onOpen [_ ws]
            (.request ws 1))

          (onText [_ ws data last]
            (.append buffer data)

            (when last
              (.put messages (str buffer))
              (.setLength buffer 0))

            (.request ws 1)
            (completed))

          (onClose [_ _ status reason]
            (.put messages
                  (ex-info "WebSocket closed"
                           {:status status
                            :reason reason}))
            (completed))

          (onError [_ _ error]
            (.put messages error)))

        websocket
        (-> (HttpClient/newHttpClient)
            (.newWebSocketBuilder)
            (.header "Origin" origin)
            (.subprotocols
             "lab-runtime.application.v1"
             (make-array String 0))
            (.buildAsync (URI/create url) listener)
            (.join))]
    {:websocket websocket
     :messages messages}))

;;----------------------------------------------------------------------
;; SEND / RECEIVE
;; ---------------------------------------------------------------------

(defn send!
  "Send one Clojure map as a WebSocket text message."
  [{:keys [websocket]} message]
  (-> ^WebSocket websocket
      (.sendText (json/write-str message) true)
      (.join))
  nil)

(defn receive!
  "Wait one WebSocket-message and convert it to Clojure map"
  [{:keys [messages]}]
  (let [message
        (.take ^LinkedBlockingQueue messages)]
    (if (instance? Throwable message)
      (throw message)
      (json/read-str message :key-fn keyword))))

;;----------------------------------------------------------------------
;; CLOSE
;;----------------------------------------------------------------------

(defn close!
  "Close WebSocket"
  [{:keys [websocket]}]
  (-> ^WebSocket websocket
      (.sendClose WebSocket/NORMAL_CLOSURE "bye")
      (.join))
  nil)
