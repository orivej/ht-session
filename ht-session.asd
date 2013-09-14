(asdf:defsystem ht-session
  :depends-on (alexandria hunchentoot ironclad babel binascii)
  :serial t
  :components ((:file "hunchentoot-patch-session")
               (:file "package")
               (:file "z85")
               (:file "signed-cookie")
               (:file "signed-session")))
