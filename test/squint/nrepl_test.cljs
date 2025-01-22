(ns squint.nrepl-test
  (:require [clojure.test :refer [deftest testing is async]]
            [squint.repl.nrepl-server :as nrepl]
            [squint.repl.nrepl.bencode :as bencode]
            ["net" :as net]
            ["fs" :as fs]))

(defn wait-for-port [interval-ms timeout-ms callback]
  (let [start-time (js/Date.now)]
    (letfn [(check []
              (if (> (- (js/Date.now) start-time) timeout-ms)
                (callback (js/Error. "Timeout waiting for port"))
                (let [port-file (try
                                  (str (fs/readFileSync ".nrepl-port"))
                                  (catch :default _e nil))]
                  (if port-file
                    (callback nil (js/parseInt port-file))
                    (js/setTimeout check interval-ms)))))]
      (check))))

(defn start-server []
  (js/Promise.
   (fn [resolve reject]
     ;; Use port 0 to let OS assign a port
     (-> (nrepl/start-server {:port 0})
         (.then (fn [_server]
                  (wait-for-port 100 5000
                                 (fn [err port]
                                   (if err
                                     (reject err)
                                     (resolve port))))))))))

(defn connect-client [port]
  (js/Promise.
   (fn [resolve reject]
     (let [socket (new net/Socket)]
       (.on socket "error" reject)
       (.on socket "connect" #(resolve socket))
       (.connect socket #js {:port port
                             :host "127.0.0.1"
                             :timeout 1000})))))

(defn send-message [^js socket msg]
  (.write socket (bencode/encode msg)))

(defn read-response [^js socket]
  (js/Promise.
   (fn [resolve reject]
     (let [responses (atom [])]
       (.on socket "error" reject)
       (.on socket "data"
            (fn [data]
              (let [[decoded _rest] (bencode/decode-all data :keywordize-keys true)]
                (swap! responses concat decoded)
                (when (some #(contains? (set (:status %)) "done") @responses)
                  (resolve @responses)))))))))

(deftest test-multiple-forms
  (testing "evaluating multiple forms in one message"
    (async done
           (let [session-id (str (random-uuid))
                 socket-ref (atom nil)]
             (-> (start-server)
                 (.then (fn [port]
                          (connect-client port)))
                 (.then (fn [socket]
                          (reset! socket-ref socket)
                          ;; Test case 1: Multiple def forms
                          (send-message socket
                                        {:op "eval"
                                         :code "(def x 1)\n(def y 2)\n(+ x y)"
                                         :session session-id
                                         :id "1"})
                          (read-response socket)))
                 (.then (fn [responses]
                          (try
                            (is (= 3 (count (filter :value responses)))
                                "Should receive three values")
                            (is (= ["nil" "nil" "3"]
                                   (->> responses
                                        (filter :value)
                                        (mapv :value)))
                                "Values should match expected sequence")
                            (catch :default e
                              (js/console.error "Assertion error:" e)
                              (throw e)))

                          ;; Test case 2: Using previous state
                          (send-message @socket-ref
                                        {:op "eval"
                                         :code "(+ x y 10)"
                                         :session session-id
                                         :id "2"})
                          (read-response @socket-ref)))
                 (.then (fn [responses]
                          (try
                            (is (= "13" (:value (last (butlast responses))))
                                "Second evaluation should use previous state")
                            (catch :default e
                              (js/console.error "Assertion error:" e)
                              (throw e)))))
                 (.catch (fn [err]
                           (js/console.error "Test error:" (.-message err))
                           (is false (str "Test failed with error: " (.-message err)))))
                 (.finally (fn []
                             (try
                               (when @socket-ref
                                 (.end @socket-ref))
                               (catch :default e
                                 (js/console.error "Cleanup error:" e)))
                             (done))))))))

(deftest test-error-handling
  (testing "handling errors in multiple forms"
    (async done
           (let [session-id (str (random-uuid))
                 socket-ref (atom nil)]
             (-> (start-server)
                 (.then (fn [port]
                          (connect-client port)))
                 (.then (fn [socket]
                          (reset! socket-ref socket)
                          (send-message socket
                                        {:op "eval"
                                         :code "(def a 1)\n(+ b 2)\n(def c 3)"
                                         :session session-id
                                         :id "1"})
                          (read-response socket)))
                 (.then (fn [responses]
                          (try
                            (is (some :ex responses) "Should contain error")
                            (is (= 2 (count (filter :value responses)))
                                "Should only evaluate first form")
                            (catch :default e
                              (js/console.error "Assertion error:" e)
                              (throw e)))))
                 (.catch (fn [err]
                           (js/console.error "Test error:" (.-message err))
                           (is false (str "Test failed with error: " (.-message err)))))
                 (.finally (fn []
                             (try
                               (when @socket-ref
                                 (.end @socket-ref))
                               (catch :default e
                                 (js/console.error "Cleanup error:" e)))
                             (done))))))))

(deftest test-state-preservation
  (testing "preserving state between multiple form evaluations"
    (async done
           (let [session-id (str (random-uuid))
                 socket-ref (atom nil)]
             (-> (start-server)
                 (.then (fn [port]
                          (connect-client port)))
                 (.then (fn [socket]
                          (reset! socket-ref socket)
                          ;; Define state and modify it
                          (send-message socket
                                        {:op "eval"
                                         :code "(def state (atom {}))\n(swap! state assoc :a 1)\n(swap! state assoc :b 2)\n@state"
                                         :session session-id
                                         :id "1"})
                          (read-response socket)))
                 (.then (fn [responses]
                          (try
                            (is (= "#js {:a 1, :b 2}" (:value (last (butlast responses))))
                                "State should be properly maintained")
                            (catch :default e
                              (js/console.error "Assertion error:" e)
                              (throw e)))

                    ;; Verify state in new evaluation
                          (send-message @socket-ref
                                        {:op "eval"
                                         :code "(get @state :a)"
                                         :session session-id
                                         :id "2"})
                          (read-response @socket-ref)))
                 (.then (fn [responses]
                          (try
                            (is (= "1" (:value (last (butlast responses))))
                                "Should be able to access previous state")
                            (catch :default e
                              (js/console.error "Assertion error:" e)
                              (throw e)))))
                 (.catch (fn [err]
                           (js/console.error "Test error:" (.-message err))
                           (is false (str "Test failed with error: " (.-message err)))))
                 (.finally (fn []
                             (try
                               (when @socket-ref
                                 (.end @socket-ref))
                               (catch :default e
                                 (js/console.error "Cleanup error:" e)))
                             (done))))))))
