;;; compose.scm — the XBS services, for eddy's *Processes* pane
;;; cf. schemer/code/schemes/compose/README.md for the clauses
;;;
;;; duckdb is the store the viewer will read structures from; xbs-live
;;; serves the rendered SVG over SSE on :9090 and takes its rotate /
;;; perspective keys from the terminal (RET on its row attaches that
;;; terminal). Next beside them: the lmto calculator the structures get
;;; forwarded to.

(app xbs
  (duckdb   (duckdb xbs.duckdb))
  (xbs-live (cabal run xbs-live -- ../examples/ring.bs)
            (dir xbs-hs)
            (after duckdb)
            (ready (curl -sf http://localhost:9090/))))
