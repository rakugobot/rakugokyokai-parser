(defpackage #:rakugokyokai-parser/rakugokai
  (:use #:cl
        #:rakugokyokai-parser/utils
        #:lquery)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-rakugokai))
(in-package #:rakugokyokai-parser/rakugokai)

(defvar *base-uri*
  "http://rakugo-kyokai.jp/rakugokai/detail.php")

(defun parse-table (table)
  (when (plump:node-p table)
    (loop for tr across (plump:child-elements table)
          append
             (coerce
              (let ((class (plump:get-attribute tr "class")))
                (cond
                  ((equal class "Performers")
                   ($ tr (combine "th" "td")
                     (map-apply (lambda (th td)
                                  (cons ($1 th (render-text))
                                        (coerce ($ td "a"
                                                  (combine (attr "href") (render-text))
                                                  (map-apply (lambda (href text)
                                                               `(("name" . ,text)
                                                                 ("uri" . ,(merge-uris href *base-uri*))))))
                                                'list))))))
                  ((equal class "OtherPerformers")
                   ($ tr (combine "th" "td")
                     (map-apply (lambda (th td)
                                  (cons ($1 th (render-text))
                                        (ppcre:split "[\\s,、　]+" ($1 td (render-text))))))))
                  ((< 0 (length ($ tr "td table")))
                   ($ tr (combine "th" "td table")
                     (map-apply (lambda (th table)
                                  (cons ($1 th (render-text))
                                        (parse-table (aref table 0)))))))
                  (t
                   ($ tr (combine ".Caption, .caption" ".confirm-text")
                     (map-apply (lambda (th td)
                                  (cons ($1 th (render-text))
                                        ($1 td (render-text)))))))))
              'list))))

(defun %parse-rakugokai-html (body)
  (let* ((main ($1 (initialize body) ".main .contents"))
         (title ($1 main "h2" (render-text)))
         (body ($1 main ".rakugokai table")))
    `(("title" . ,title)
      ,@(parse-table body))))

(defun parse-rakugokai (body)
  (let ((res (%parse-rakugokai-html body)))
    `(("title" . ,(aget res "title"))
      ("start-date" . ,(aget res "開催日"))
      ("start-time" . ,(aget res "開演"))
      ,@(let ((place (aget res "会場")))
          `(("place" . ,(aget place "名称"))
            ("address" . ,(aget place "住所"))))
      ("performers" . ,(append
                        (aget res "出演者(協会員)")
                        (mapcar (lambda (name)
                                  `(("name" . ,name)))
                                (aget res "出演者(その他)")))))))
