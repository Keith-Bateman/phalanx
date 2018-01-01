(use-package :asdf)

(defsystem phalanx
	:name "phalanx"
	:description ""
	:author ""
	:components
	((:file "curses")
	 (:file "phalanx" :depends-on ("curses")))
	:depends-on ("asdf" "rt" "cffi" "trivial-gray-streams" "cl-store" "md5"))
			
