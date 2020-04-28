# based on bits from boot.janet
(defn handler
  "REPL handler for connections."
  [stream]
  (defn net-repl
    "Run a repl. The first parameter is an optional function to call to
    get a chunk of source code that should return nil for end of file.
    The second parameter is a function that is called when a signal is
    caught. One can provide an optional environment table to run
    the repl in."
    [&opt chunks onsignal env]
    (default env (make-env))
    (default chunks
      (fn [buf p]
        # XXX: may be not desirable all of the time?
        (:write stream
                (string
                 "repl:"
                 ((parser/where p) 0)
                 ":"
                 (parser/state p :delimiters) "> "))
        (:read stream 1024 buf)))
    #
    (defn bad-parse
      "Default handler for a parse error."
      [p where]
      (def [line col] (parser/where p))
      (def err-b @"")
      (with-dyns [:err err-b]
        (eprint
         "parse error in " where
         " around line " (string line)
         ", column " (string col)
         ": " (parser/error p))
        (eflush)
        (print "bad-parse:" err-b)
        (:write stream err-b)))
    #
    (defn bad-compile
      "Default handler for a compile error."
      [msg macrof where]
      (def err-b @"")
      (with-dyns [:err err-b]
        (if macrof
          (debug/stacktrace macrof
                            (string msg " while compiling " where))
          (eprint
           "compile error: " msg
           " while compiling " where))
        (eflush)
        (print "bad-compile:" err-b)
        (:write stream err-b)))
    #
    (defn evaluate
      [to-eval source env where]
      (def out-b @"")
      (def err-b @"")
      (with-dyns [:err err-b :out out-b]
        (def res (to-eval))
        (def res-b @"")
        (with-dyns [:out res-b]
          (pp res))
        (:write stream out-b)
        (:write stream err-b)
        (:write stream res-b)))
    #
    (defn make-onsignal
      [e level]
      (fn [f x]
        (if (= :dead (fiber/status f))
          (do
            (print "onsignal: succeeded")
            (put e '_ @{:value x}))
          (do
            (print "onsignal: failure")
            (def err-b @"")
            (with-dyns [:err err-b]
              (debug/stacktrace f x)
              (eflush)
              (print err-b)
              (:write stream err-b))))))
    #
    (run-context {:env env
                  :chunks chunks
                  :evaluator evaluate
                  :on-compile-error bad-compile
                  :on-parse-error bad-parse
                  :on-status (or onsignal (make-onsignal env 1))
                  :source "net-repl"}))
  #
  (defer (:close stream)
    (def id (gensym))
    (print "Connection " id "!")
    (net-repl nil nil root-env)
    (printf "Done %v!" id)))

(net/server "127.0.0.1" "7650" handler)
