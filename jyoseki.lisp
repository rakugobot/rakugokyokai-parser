(defpackage #:rakugokyokai-parser/jyoseki
  (:use #:cl
        #:rakugokyokai-parser/utils
        #:lquery)
  (:import-from #:local-time)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-jyoseki))
(in-package #:rakugokyokai-parser/jyoseki)

(defparameter *base-uri*
  "http://rakugo-kyokai.jp/program/suzumoto/index.php")

(defun normalize-month (month)
  (cond
    ((ignore-errors (parse-integer month)))
    ((aget '(("１" . 1)
             ("２" . 2)
             ("３" . 3)
             ("４" . 4)
             ("５" . 5)
             ("６" . 6)
             ("７" . 7)
             ("８" . 8)
             ("９" . 9))
           month))
    ((equal month "正") 1)
    (t (error "Invalid month: ~S" month))))

(defun get-year-of-month (month)
  (let ((today (local-time:today)))
    (if (< month (local-time:timestamp-month today))
        (1+ (local-time:timestamp-year today))
        (local-time:timestamp-year today))))

(defun parse-jyoseki-period (month period part)
  (let ((year (get-year-of-month month)))
    (cond
      ((or (string= period "上")
           (string= period "初"))
       (values (if (equal part "後半")
                   6
                   1)
               (if (equal part "前半")
                   5
                   10)))
      ((or (string= period "中")
           (string= period "二之"))
       (values (if (equal part "後半")
                   16
                   11)
               (if (equal part "前半")
                   15
                   20)))
      ((string= period "下")
       (values (if (equal part "後半")
                   26
                   21)
               (if (equal part "前半")
                   25
                   (if (= month 2)
                       (local-time:days-in-month month year)
                       30))))
      (t (error "Unexpected period format: ~A" period)))))

(defun parse-title (title)
  (let ((year (nth-value 5 (decode-universal-time (get-universal-time)))))
    (cond
      ;; 本日の寄席
      ((ppcre:register-groups-bind (hall (#'parse-integer mon day))
           ("^(.+)：(\\d{1,2})月(\\d{1,2})日\\(.\\)$" title)
         `(("title" . ,(format nil "~A ~2,'0D月~2,'0D日"
                               hall mon day))
           ("date" . ,(format nil "~D-~2,'0D-~2,'0D"
                              year mon day)))))
      ;; 黒門亭
      ((ppcre:register-groups-bind (hall (#'parse-integer mon day))
           ("^(.+)(?:　|\\s+?)(\\d{1,2})月(\\d{1,2})日" title)
         `(("title" . ,(format nil "~A ~2,'0D月~2,'0D日" hall mon day))
           ("date" . ,(format nil "~D-~2,'0D-~2,'0D"
                              year mon day)))))
      ;; 定席番組
      ((ppcre:register-groups-bind (hall (#'normalize-month mon) period part)
           ("^(.+)(?:　|\\s+?)(\\d{1,2}|[１２３４５６７８９]|正)月(上|中|下|初|二之)席(?:（(前半|後半)）)?$" title)
         (let ((year (get-year-of-month mon)))
           (multiple-value-bind (start-day end-day) (parse-jyoseki-period mon period part)
             `(("title" . ,(format nil "~A ~A月~A席~:[~;~:*(~A)~]" hall mon period part))
               ("date-from" . ,(format nil "~D-~2,'0D-~2,'0D" year mon start-day))
               ("date-to" . ,(format nil "~D-~2,'0D-~2,'0D" year mon end-day)))))))
      (t
       `(("title" . ,title))))))

(defun parse-jyoseki (body)
  (let ((boxes ($ (initialize body) ".main .contents .member-detail")))
    (loop for box across boxes
          append
             (coerce
              ($ box (combine "h2" ".inner")
                (map-apply (lambda (h2 inner)
                             `(,@(parse-title ($1 h2 (render-text)))
                               ("tables"
                                . ,(coerce
                                    ($ inner ".time-table table"
                                      (map (lambda (table)
                                             `(("subtitle" . ,(remove #\Space ($1 table "caption" (render-text))))
                                               ("entertainers"
                                                . ,(coerce
                                                    ($ table ".name a"
                                                      (map
                                                       (lambda (name)
                                                         ($1 name (combine (render-text) (attr "href"))
                                                           (map-apply (lambda (name uri)
                                                                        `(("name" . ,name)
                                                                          ("uri" . ,(merge-uris uri *base-uri*)))))))))
                                                    'list))))))
                                    'list))))))
              'list))))
