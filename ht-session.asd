(asdf:defsystem ht-session
  :version "1.0"
  :description "Signed client-side sessions for Hunchentoot"
  :author "Orivej Desh <orivej@gmx.fr>"
  :licence "Unlicense" ; http://unlicense.org/UNLICENSE
  :depends-on (alexandria hunchentoot sha3 babel binascii)
  :serial t
  :components ((:file "package")
               (:file "z85") ; license: BSD
               (:file "signed-cookie")
               (:file "signed-session")))
