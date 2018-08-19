(defpackage #:rakugokyokai-parser/variety-entertainer/detail
  (:use #:cl
        #:rakugokyokai-parser/utils
        #:lquery)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:rakugokyokai-parser/variety-entertainer/detail)

(defvar *base-uri*
  "http://rakugo-kyokai.jp/variety-entertainer/member_detail.php")

(defun parse-entertainer-detail (body)
  (let ((detail ($1 (initialize body) ".main .contents .member-detail")))
    (assert detail)
    (append
     ($1 detail "h2" (combine (render-text) ".kana")
       (map-apply (lambda (h2 kana)
                    `(("芸名" . ,(ppcre:regex-replace "\\s*（.+）$" h2 ""))
                      ("ふりがな" .
                       ,(ppcre:regex-replace
                         "（(.+)）"
                         ($1 kana (render-text))
                         "\\1"))))))
     (coerce
      ($1 detail ".inner .profile:nth-child(1) dl"
        (map (lambda (dl)
               (let ((data ($ dl (children))))
                 (loop for i from 0 below (length data) by 2
                       for dt = (aref data i)
                       for dd = (aref data (1+ i))
                       collect (cons ($1 dt "img" (attr "alt"))
                                     ($1 dd (render-text))))))))
      'list)
     `(("schedules"
        .
        ,(coerce
          ($ detail "table tr td a"
            (combine (render-text) (attr "href"))
            (map-apply (lambda (title uri)
                         `(("title" . ,title)
                           ("uri" . ,(merge-uris uri *base-uri*))))))
          'list))))))
