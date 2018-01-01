;;;; Phalanx
;;; A tactical 7DRL about a Roman legionary who has his soul split in two by a witch

;;; Copyright (C) 2018 Keith Bateman

;;; Email: kbateman@hawk.iit.edu

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see http://www.gnu.org/licenses/.

(in-package :cl-user)
(defpackage :phalanx
  (:use :cl :curses :md5))

(in-package :phalanx)

;;; Control Flow

(defun main ()
  (setf *random-state* (make-random-state t))
  (connect-console)
  (noecho)
  (getch)
  (close-console)
  #+clisp (ext:quit))

#+clisp
(defun make-exec ()
  (ext:saveinitmem "phalanx" :quiet t :init-function #'main :executable t :norc t))
