(defsystem cl-sf2
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "SoundFont 2 (SF2) reader/writer for Common Lisp."
  :depends-on (#:binstruct #:cliff #:fast-io)
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op #:cl-sf2/test))))

(defsystem cl-sf2/test
  :depends-on (#:cl-sf2 #:parachute #:flexi-streams)
  :pathname "test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:sf2.test))))
