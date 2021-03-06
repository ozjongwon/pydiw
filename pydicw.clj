;;;;   -*- Mode: clojure; encoding: utf-8; -*-
;;
;; Copyright (C) 2014 Jong-won Choi
;; All rights reserved.
;;
;;;; Commentary:
;;
;;      Programming, You're Doing It Completely Wrong!
;;
;;              Collection of wrong codes; Can you think of the reason?
;;
;;
;; This is just a joke, do not take it seriously :) Just for showing:
;;      - Even respected programmers make mistakes and sometimes those are serious mistakes
;;      - Premature optimisation can make things slow
;;
;;
;; I read other people's code often and I'll keep adding suspicious code whenever I find :)
;;
;;;; Code:



;;;
;;; From code examples used in the 2013 Clojure Conj talk by Timothy Baldridge
;;;
;;; https://github.com/halgari/clojure-conj-2013-core.async-examples/blob/master/src/clojure_conj_talk/core.clj
;;;
;;; I learnt interesting things from this code and could write my own for publishing/subscribing service.
;;; Thanks Timothy!
;;;

(defn thread-pool-service [ch f max-threads timeout-ms]
  (let [thread-count (atom 0)
        buffer-status (atom 0)
        buffer-chan (chan)
        thread-fn (fn []
                    (swap! thread-count inc)
                    (loop []
                      (when-let [v (first (alts!! [buffer-chan (timeout timeout-ms)]))]
                        (f v)
                        (recur)))
                    (swap! thread-count dec)
                    (println "Exiting..."))]
    (go (loop []
          (when-let [v (<! ch)]
            (if-not (alt! [[buffer-chan v]] true
                          :default false)
              (loop []
                (if (< @thread-count max-threads)
                  (do (put! buffer-chan v)
                      (thread (thread-fn)))
                  (when-not (alt! [[buffer-chan v]] true
                                  [(timeout 1000)] ([_] false))
                    (recur)))))
            (recur)))
        (close! buffer-chan))))


;;;
;;; From https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.cljx
;;;     This is one of functions in the file having fundamental problems.
;;;
(defn map-vals
  "Build map k -> (f v) for [k v] in map, preserving the initial type"
  [f m]
  (cond
   (sorted? m)
   (reduce-kv (fn [out-m k v] (assoc out-m k (f v))) (sorted-map) m)
   (map? m)
   (persistent! (reduce-kv (fn [out-m k v] (assoc! out-m k (f v))) (transient {}) m))
   :else
   (for-map [[k v] m] k (f v))))


;;;
;;; From https://github.com/ptaoussanis/carmine/blob/master/src/taoensso/carmine.clj
;;;
;;; Some designs are fragile, hard to understand and scary, most of people do not
;;; like to read or change such code (even the author!). It can cause some problems later.
;;; (I found an issue after finding that a timeout option in conn-spec can raise an exception)
;;;

(defmacro with-new-listener
  [conn-spec handler initial-state & body]
  `(let [handler-atom# (atom ~handler)
         state-atom#   (atom ~initial-state)
         {:as conn# in# :in} (conns/make-new-connection
                              (assoc (conns/conn-spec ~conn-spec)
                                :listener? true))]

     (future-call ; Thread to long-poll for messages
      (bound-fn []
        (while true ; Closes when conn closes
          (let [reply# (protocol/get-unparsed-reply in# {})]
            (try
              (@handler-atom# reply# @state-atom#)
              (catch Throwable t#
                (timbre/error t# "Listener handler exception")))))))

     (protocol/with-context conn# ~@body
       (protocol/execute-requests (not :get-replies) nil))
     (Listener. conn# handler-atom# state-atom#)))

;;; PYDICW.CLJ ends here
