;;; cfpsgram.lsp [Chapter  4] example context free grammar in list format

(defvar rules)

(setq rules
 '(((S) (NP) (VP))
   ((VP) (V))
   ((VP) (V) (NP))
   ((V) died)
   ((V) employed)
   ((NP) nurses)
   ((NP) patients)
   ((NP) Medicenter)
   ((NP) Dr Chan)))
