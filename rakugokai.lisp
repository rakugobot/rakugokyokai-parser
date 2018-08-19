(defpackage #:rakugokyokai-parser/rakugokai
  (:use #:cl
        #:rakugokyokai-parser/utils
        #:lquery)
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
                                        (coerce ($ td "a" (render-text)) 'list))))))
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

(defun parse-rakugokai (body)
  (let* ((main ($1 (initialize body) ".main .contents"))
         (title ($1 main "h2" (render-text)))
         (body ($1 main ".rakugokai table")))
    `(("title" . ,title)
      ,@(parse-table body))))
