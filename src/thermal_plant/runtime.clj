(ns thermal-plant.runtime
  (:require
   [thermal-plant.transport.tcp :as tcp]
   [thermal-plant.transport.websocket :as websocket]))

;;---------------------------------------------------------------------
;; TRANSPORT DISPATCH
;; --------------------------------------------------------------------

(defn- open-transport!
  "Open selected transport"
  [{:keys [transport host port url origin]}]
  (case transport
    :tcp
    (tcp/open! host port)

    :websocket
    (websocket/open! url origin)

    (throw
     (ex-info "Unknown transport"
              {:transport transport}))))

(defn- send!
  "Send one message through client's transport."
  [{:keys [transport connection]} message]
  (case transport
    :tcp
    (tcp/send! connection message)

    :websocket
    (websocket/send! connection message)))

(defn- receive!
  "Receive one message through client's transport."
  [{:keys [transport connection]}]
  (case transport
    :tcp
    (tcp/receive! connection)

    :websocket
    (websocket/receive! connection)))

(defn close!
  "Close client connection."
  [{:keys [transport connection]}]
  (case transport
    :tcp
    (tcp/close! connection)

    :websocket
    (websocket/close! connection)))

;;---------------------------------------------------------------------
;; MESSAGE EXCHANGE
;; --------------------------------------------------------------------

(defn- next-message-id!
  "Create next connection-local message id."
  [{:keys [message-counter]}]
  (str "m-" (swap! message-counter inc)))

(defn- send-request!
  "Send one Runtime request and return its msg-id."
  [client op args request-id]
  (let [msg-id (next-message-id! client)
        request (cond-> {:v 1
                         :msg_id msg-id
                         :op op
                         :args args}
                  request-id
                  (assoc :request_id request-id))]

    (send! client request)

    msg-id))

(defn- receive-response!
  "Receive one response for expected msg-id."
  [client msg-id]
  (let [response (receive! client)]

    (when-not (= msg-id (:msg_id response))
      (throw
       (ex-info "Unexpected response"
                {:expected msg-id
                 :response response})))
    response))

(defn- exchange!
  "Send one query and receive one response."
  [client op args]
  (let [msg-id (send-request! client op args nil)]
    (receive-response! client msg-id)))

;;---------------------------------------------------------------------
;; CONNECTION
;;---------------------------------------------------------------------

(defn- hello!
  "Perform required first Runtime request."
  [client scope]
  (let [response (exchange!
                  client
                  "hello"
                  {:scope scope})]
    (when-not (= "result" (:type response))
      (throw
       (ex-info "Runtime hello failed"
                {:response response})))
    (:result response)))

(defn connect!
  "Open transport, perform hello and return ready Runtime client."
  [{:keys [transport scope] :as config}]
  (let [connection (open-transport! config)

        client {:transport transport
                :connection connection
                :message-counter (atom 0)}]

    (try
      (let [hello
            (hello! client scope)]

        (assoc client
               :hello hello
               :scope (:scope hello)
               :boot-id (:boot_id hello)
               :next-seq
               (atom
                (Long/parseLong
                 (:next_seq hello)))))

      (catch Exception error
        (close! client)
        (throw error)))))

;;---------------------------------------------------------------------------------
;;  QUERIES
;; --------------------------------------------------------------------------------

(defn query!
  "Perform one read-only Runtime operation."
  [client op args]
  (let [response (exchange! client op args)]
    (if (= "result" (:type response))
      (:result response)
      (throw
       (ex-info "Runtime query failed"
                {:response response})))))

(defn discover!
  "Return Runtime discovery projection."
  [client]
  (query! client "discover" {}))

(defn latest!
  "Return latest value for one signal."
  [client signal]
  (query! client
          "latest"
          {:signal signal}))

(defn reference!
  "Return current state of one Reference."
  [client reference-id]
  (query! client
          "reference"
          {:reference reference-id}))

(defn controller!
  "Return current state of one controller."
  [client controller-id]
  (query! client
          "controller"
          {:controller controller-id}))

(defn output!
  "Return current state of one actuator output."
  [client actuator]
  (query! client
          "output"
          {:actuator actuator}))

(defn operation-status!
  "Return retained state of one mutation."
  [client request-id]
  (query! client
          "operation_status"
          {:request_id request-id}))

;;---------------------------------------------------------------------
;;  COMMANDS
;; --------------------------------------------------------------------

(defn- current-request-id
  "Build id for the next Runtime mutation."
  [{:keys [scope next-seq]}]
  {:scope scope
   :seq (str @next-seq)})

(defn command!
  "Perform one Runtime mutation."
  [client op args]
  (let [request-id (current-request-id client)
        msg-id (send-request! client op args request-id)
        accepted (receive-response! client msg-id)]

    (when (= "error" (:type accepted))
      (throw
       (ex-info "Runtime command rejected"
                {:response accepted})))

    (when-not (= "accepted" (:state accepted))
      (throw
       (ex-info "Expected accepted operation"
                {:response accepted})))

    (swap! (:next-seq client) inc)

    (let [terminal (receive-response! client msg-id)]
      (case (:state terminal)
        "completed"
        (:result terminal)

        "failed"
        (throw
         (ex-info "Runtime command failed"
                  {:response terminal}))

        (throw
         (ex-info "Unexpected command result"
                  {:response terminal}))))))

;; Reference commands

(defn retune-reference!
  "Change target and rate of one Reference."
  [client reference-id expected-revision target rate]
  (command! client
            "reference_retune"
            {:reference reference-id
             :expected_revision expected-revision
             :target target
             :rate rate}))

;; Controller commands

(defn start-controller!
  "Start one Runtime controller."
  [client controller-id]
  (command! client
            "controller_start"
            {:controller controller-id}))

(defn pause-controller!
  "Pause one Runtime controller."
  [client controller-id]
  (command! client
            "controller_pause"
            {:controller controller-id}))

(defn resume-controller!
  "Resume one Runtime controller."
  [client controller-id]
  (command! client
            "controller_resume"
            {:controller controller-id}))
