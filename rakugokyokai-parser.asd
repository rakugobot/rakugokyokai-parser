(defsystem "rakugokyokai-parser"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Parser for an HTML of rakugo-kyokai.jp"
  :depends-on ("rakugokyokai-parser/main")
  :in-order-to ((test-op (test-op "rakugokyokai-parser/tests"))))
