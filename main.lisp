(uiop:define-package #:rakugokyokai-parser
  (:nicknames #:rakugokyokai-parser/main)
  (:use #:cl)
  (:use-reexport #:rakugokyokai-parser/variety-entertainer/index
                 #:rakugokyokai-parser/variety-entertainer/detail
                 #:rakugokyokai-parser/jyoseki
                 #:rakugokyokai-parser/rakugokai))
(in-package #:rakugokyokai-parser)
