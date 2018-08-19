(defpackage #:rakugokyokai-parser/variety-entertainer/index
  (:use #:cl
        #:rakugokyokai-parser/utils
        #:lquery)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-entertainer-index))
(in-package #:rakugokyokai-parser/variety-entertainer/index)

(defvar *base-uri*
  "http://rakugo-kyokai.jp/variety-entertainer/index.php")

(defvar *id-to-category*
  '(("m01" . "真打")
    ("m02" . "講談")
    ("m03" . "二ツ目")
    ("m04" . "前座")
    ("m05" . "色物")
    ("m06" . "物故者")
    ("m07" . "お囃子")))

(defun parse-entertainer-index (body)
  (let* ((member-lists ($ (initialize body) ".main .contents .member-list")))
    (assert (< 0 (length member-lists)))

    (loop for member-list across member-lists
          append
             `((,(aget *id-to-category* (plump:get-attribute member-list "id"))
                .
                ,(coerce
                  ($ member-list ".inner ul li"
                    (combine "a" ".sns")
                    (map-apply (lambda (a sns)
                                 (append
                                  ($1 a (combine (render-text) (attr "href"))
                                    (map-apply (lambda (name uri)
                                                 `(("name" . ,name)
                                                   ("uri" . ,(merge-uris uri *base-uri*))))))
                                  (let ((sns-links (coerce
                                                    ($ sns "span a" (combine (attr "href") "img")
                                                      (map-apply (lambda (uri img)
                                                                   (cons ($1 img (attr "alt")) uri))))
                                                    'list)))
                                    (and sns-links
                                         `(("sns" . ,sns-links))))))))
                  'list))))))
