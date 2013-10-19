## Purpose

This package implements signed cookies and provides hash table backed
client side stored sessions for Hunchentoot.

## Rationale

The session mechanism built into Hunchentoot saves a signed client ID
in a cookie and maintains in-memory mapping between IDs and session
data.  This mapping is gone whenever Lisp process is restarted.  The
ht-session mechanism of this package saves signed session data in a
cookie on each response and decodes it into a hash table on each
request.

In short, the benefit over Hunchentoot sessions is persistence, the
benefit over unsigned cookies is that signed cookies can not contain
something you did not put there first.

## Caveat

The nature of signed cookies is such that the server can verify that
the cookie was indeed placed by the server, but can not ascertain from
the cookie itself whether it was the latest placed one or not.

There is a built-in mechanism of expiration (with expiration date
encoded in the cookie), but until the cookie is expired, it is valid.

This limits the scope of data which is safe to store in a signed cookie to:

- data which affects no one but the user with the cookie
- data which can forever be associated with the client

The latter includes a client ID in your database, unless you need to
revoke cookies without revoking client ID; or some intermediary
database backed session ID which links to the client ID until the
cookie is revoked, in case you do need revocation.

## Sample Usage

```lisp
(defpackage hts-user
  (:use cl)
  (:local-nicknames (ht hunchentoot)
                    (hts ht-session)))

(in-package hts-user)
```

1. Specify `signed-session-request` as the request class

```lisp
(defclass my-acceptor (ht:easy-acceptor)
  ()
  (:default-initargs
   :request-class 'hts:signed-session-request))

or inherit your request from `signed-session-request-mixin` (e.g. with
[restas](https://github.com/archimag/restas) framework)

```lisp
(defclass heroku-request (hts:signed-session-request-mixin
                          restas::restas-request)
  ())

(defclass heroku-acceptor (restas-acceptor)
  ()
  (:default-initargs
   :request-class 'heroku-request))
```

2. Initialize session secret from a persistent location so that it remains the same across Lisp process restarts.  (Ignore this step to let `hts:randomize-signing-key` initialize it for you.)  Initialize session time out.

```lisp
(setf hts:*signing-key* (binascii:decode-z85 *persisted-z85-encoded-session-secret*))
(setf ht:*session-max-time* 432000) ; 5 days
```

3. Use signed sessions in request handlers.

```lisp
(defun sess (key)
  (hts:signed-session-value key))
(defun (setf sess) (val key)
  (setf (hts:signed-session-value key) val))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (when name
    (setf (sess :name) name))
  (format nil "Hey~@[ ~A~]!" (sess :name)))

(hunchentoot:define-easy-handler (logout :uri "/bye") ()
  (hts:delete-signed-session-value :name)
  "Bye")
```

## Implementation details

To encode a session in a cookie, the session hash table is converted
to a plist, prepended with get-universal-time + session-max-time,
serialized with prin1-to-string, encoded into a cookie-safe sequence
of characters with [Z85](http://rfc.zeromq.org/spec:32); then a SHA-3 256
sum of that sequence and the session secret is taken, encoded with Z85
and prepended.

To decode a session from a cookie, the initial part of the cookie is
compared with the SHA-3 sum calculated from the remainder of the
cookie and the session secret as described above.  When they match,
the remainder is decoded from Z85 (where up to three zero bytes may
appear at the end of the string), read-from-string into the list
(where trailing zero bytes are ignored), and its first element is
compared with get-universal-time.  Unless the cookie is expired, the
rest of the list is converted from plist to a hash table.

I chose a base-85 encoding because its 5/4 encoded/decoded ratio is
perfectly suited for a 32 byte hash sum.  I chose Z85 because all of
its encoded code points are allowed in a cookie, unlike
binascii:base85 which has a semicolon.

## Some credits

For the dependencies:

- Binascii (BSD) by Nathan Froyd
- SHA-3 (MIT) by Pierre R. Mai
- Babel (MIT) by Luis Oliveira
- Hunchentoot (BSD-2) by Dr. Edmund Weitz
- Alexandria (public-domain / MIT)

For the specifications:

- Z85 by Pieter Hintjens
