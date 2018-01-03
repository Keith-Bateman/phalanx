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

;;; Macros and Helper Functions

(defmacro do-while (test &rest body)
  `(progn ,@body
		  (do ()
			  ((not ,test))
			,@body)))
  
(defmacro while (test &rest body)
  `(do ()
	   ((not ,test))
	 ,@body))

;; var starts at start and is incremented by step every time until it equals end. Note that if dostep is given an incorrect step it can easily form an infinite loop, as there is no check for this.
(defmacro dostep ((var start end &optional (step 1)) &rest body)
  (let ((v (gensym)))
	(setq v var)
	`(do ((,v ,start (+ ,v ,step)))
		 ((= ,v ,end) (progn ,@body nil))
	   ,@body)))
  
(defmacro instance-of-p (instance class)
  `(eq (class-name (class-of ,instance)) ,class))

(defmacro format-message ((str &rest format-args) &key (color :cwhite))
  `(message (format nil ,str ,@format-args) :color ,color))

;; Helper macro for init-print. Returns the slot-value if the slot is bound or the symbol unbound if the slot is not bound
(defmacro slot-value-or-unbound (instance slot)
  `(if (slot-boundp ,instance ,slot)
	   (slot-value ,instance ,slot)
	   'unbound))

;; Create a printer for the specified class (unquoted) with the specified slots (quoted)
(defmacro init-print (class slots)
  `(defmethod print-object ((instance ,class) stream)
	 (let ((desc (cons ',class (mapcar (lambda (slot) (cons slot (slot-value-or-unbound instance slot))) ,slots))))
	   (write desc :stream stream))))

(defun random-list (lst)
  (nth (random (length lst)) lst))

;;; Name generator

(defun gen-roman-name ()
  (let ((praenomen (list "Gaius" "Lucius" "Marcus" "Publius" "Quintus" "Titus" "Tiberius" "Sextus" "Aulus" "Decimus" "Gnaeus" "Spurius" "Manius" "Servius" "Appius" "Numerius"))
		(nomen (list "Annius" "Antonius" "Arrius" "Artorius" "Asinius" "Atilius" "Atius" "Aurelius" "Autronius" "Caecilius" "Caedicius" "Caelius" "Calidius" "Calpurnius" "Cassius" "Claudius" "Cloelius" "Cocceius" "Cominius" "Cornelius" "Coruncanius" "Curiatius" "Curius" "Curtius" "Decius" "Didius" "Domitius" "Duilius" "Durmius" "Equitius" "Fabius" "Fabricius" "Fannius" "Flavius" "Fulvius" "Furius" "Gabinius" "Galerius" "Geganius" "Gellius" "Geminius" "Genucius" "Gratius" "Herennius" "Hirtius" "Horatius" "Hortensius" "Hostilius" "Iulius" "Iunius" "Iuventius" "Laelius" "Lartius" "Licinius" "Livius" "Lucilius" "Lucretius" "Manlius" "Marcius" "Marius" "Memmius" "Menenius" "Minicius" "Minius" "Minucius" "Modius" "Mucius" "Naevius" "Nautius" "Numerius" "Numicius" "Octavius" "Ovidius" "Papirius" "Petronius" "Pinarius" "Pompeius" "Pompilius" "Pontius" "Popillius" "Porcius" "Postumius" "Quinctilius" "Quinctius" "Rubellius" "Rufius" "Rutilius" "Sallustius" "Salonius" "Salvius" "Scribonius" "Seius" "Sempronius" "Sentius" "Sergius" "Sertorius" "Servilius" "Sextius" "Sicinius" "Suetonius" "Sulpicius" "Tarpeius" "Tarquitius" "Terentius" "Titinius" "Titurius" "Tuccius" "Tullius" "Ulpius" "Valerius" "Vedius" "Velleius" "Vergilius" "Verginius" "Vibius" "Villius" "Vipsanius" "Vitellius" "Vitruvius" "Volumnius"))
		(cognomen (list "Aculeo" "Agricola" "Agrippa" "Ahala" "Ahenobarbus" "Albinus" "Albus" "Ambustus" "Annalis" "Aquila" "Aquilinus" "Arvina" "Asellio" "Asina" "Atellus" "Avitus" "Balbus" "Barba" "Barbatus" "Bassus" "Bestia" "Bibaculus" "Bibulus" "Blaesus" "Brocchus" "Brutus" "Bubulcus" "Bucco" "Bulbus" "Buteo" "Caecus" "Caepio" "Caesar" "Calidus" "Calvinus" "Calvus" "Camillus" "Caninus" "Canus" "Capito" "Carbo" "Catilina" "Cato" "Catulus" "Celer" "Celsus" "Cethegus" "Cicero" "Cicurinus" "Cilo" "Cincinnatus" "Cinna" "Cordus" "Cornicen" "Cornutus" "Corvinus" "Corvus" "Cossus" "Costa" "Cotta" "Crassipes" "Crassus" "Crispinus" "Crispus" "Culleo" "Curio" "Cursor" "Curvus" "Denter" "Dento" "Dives" "Dolabella" "Dorsuo" "Drusus" "Figulus" "Fimbria" "Flaccus" "Flavus" "Florus" "Fronto" "Fullo" "Fusus" "Galeo" "Gemellus" "Glabrio" "Gracchus" "Gurges" "Habitus" "Helva" "Imperiosus" "Iullus" "Labeo" "Lactuca" "Laenas" "Lanatus" "Laevinus" "Laterensis" "Lentulus" "Lepidus" "Libo" "Licinus" "Longus" "Lucullus" "Lupus" "Lurco" "Macer" "Macula" "Malleolus" "Mamercus" "Marcellus" "Maro" "Merenda" "Mergus" "Merula" "Messalla" "Metellus" "Murena" "Mus" "Musca" "Nasica" "Naso" "Natta" "Nepos" "Nero" "Nerva" "Niger" "Novellus" "Ocella" "Pacilus" "Paetus" "Pansa" "Papus" "Paterculus" "Paullus" "Pavo" "Pera" "Pictor" "Piso" "Plancus" "Plautus" "Poplicola" "Postumus" "Potitus" "Praeconinus" "Praetextatus" "Priscus" "Proculus" "Publicola" "Pulcher" "Pullus" "Pulvillus" "Purpureo" "Quadratus" "Ralla" "Regillus" "Regulus" "Rufus" "Ruga" "Rullus" "Rutilus" "Salinator" "Saturninus" "Scaeva" "Scaevola" "Scapula" "Scaurus" "Scipio" "Scrofa" "Seneca" "Severus" "Silanus" "Silo" "Silus" "Stolo" "Strabo" "Structus" "Sulla" "Sura" "Taurus" "Triarius" "Trigeminus" "Trio" "Tubero" "Tubertus" "Tubulus" "Tuditanus" "Tullus" "Turdus" "Varro" "Varus" "Vatia" "Verres" "Vespillo" "Vetus" "Vitulus" "Volusus")))
	(list (random-list praenomen) (random-list nomen) (random-list cognomen))))

;;; Classes, Methods, and Relevant Functions

(defclass tile ()
  ((blocks :initarg :blocks :accessor blocks-p)
   (blocks-sight :initform t :initarg :blocks-sight :accessor blocks-sight-p)
   (lit :initform nil :accessor lit)
   (explored :initform nil :accessor explored-p)))

(init-print tile '(blocks blocks-sight lit explored))

(defclass obj ()
  ((x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)
   (char :initarg :char :accessor get-char)
   (name :initarg :name :accessor get-name)
   (color :initform :cwhite :initarg :color)
   (blocks :initform nil :initarg :blocks :accessor blocks-p)
   (blocks-sight :initform nil :initarg :blocks-sight :accessor blocks-sight-p)
   (pickup :initform nil :initarg :pickup :accessor pickup-p)))

(init-print obj '(x y char name color blocks blocks-sight pickup))

;; list of roman enemies '(gladiator slave hun visigoth persian goth vandal carthaginian greek)
;; list of fantastical enemies '()
(defclass monster (obj)
  ((hp :initarg :hp :accessor get-hp)
   (max-hp :accessor get-max-hp)
   (attack :initarg :atk :accessor get-atk)
   (defense :initarg :def :accessor get-def)
   (ai :initform 'basic-ai :initarg :ai :accessor get-ai)
   (death :initform 'basic-death :initarg :death :accessor get-death)))

(defmethod initialize-instance :after ((mon monster) &key)
  (when (slot-boundp mon 'hp)
	(setf (get-max-hp mon) (get-hp mon))))

(defmethod take-turn ((mon monster))
  (funcall (coerce (get-ai mon) 'function) mon))

(defmethod kill-monster ((mon monster))
  (funcall (coerce (get-death mon) 'function) mon))

(defmethod basic-ai ((mon monster))
  (cond ((not (or (can-see-p mon (get-p1 *player*))
				  (can-see-p mon (get-p2 *player*))))
		 ;; Wander
		 (move-mon mon (- (random 3) 1) (- (random 3) 1)) ; Something like this, but really we'd prefer to have the monster explore the dungeon and remember where things are (like the player)
		 )
		((<= (get-hp mon) (/ (max-hp mon) 5))
		 (let ((map (dijkstra-map (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
								  (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))))))
		   (dotimes (x (- *screen-width* 1))
			 (dotimes (y (- *screen-height* 1))
			   (when (aref map x y)
				 (setf (aref map x y) (* (aref map x y) -1.2)))))
		   (let ((path (dijkstra-path map (cons (get-x mon) (get-y mon)))))
			 (move-mon mon (- (car path) (get-x mon)) (- (cdr path) (get-y mon)))))
		 ;; PANIC. Run away
		 t)
		((and (> (length (points (make-instance 'line :x1 (get-x mon) :y1 (get-y mon)
												:x2 (get-x (get-p1 *player*)) :y2 (get-y (get-p1 *player*))))) 2)
			  (> (length (points (make-instance 'line :x1 (get-x mon) :y1 (get-y mon)
												:x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*))))) 2)
			  (not (null (dijkstra-path (dijkstra-map (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
													  (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))))
										(cons (get-x mon) (get-y mon))))))
		 (let ((path (dijkstra-path (dijkstra-map (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
												  (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))))
									(cons (get-x mon) (get-y mon)))))
		   (move-mon mon (- (car path) (get-x mon)) (- (cdr path) (get-y mon)))
		 ;; Chase
		   t))
		(t
		 (attack mon *player*) ; Attack
		 )))

(defmethod basic-death ((mon monster))
  (setf (get-char mon) #\%)
  (setf (get-ai mon) '(lambda (mon) t))
  (setf (blocks-p mon) nil)
  (format-message ("~A dies a horrible and painful death" (get-name mon)) :color :cred))

(defmethod player-death ((mon monster))
  (setf (get-char mon) #\%)
  (setf *game-state* nil)
  (message "You die a lonely and depressing death" :color :cred))

(defmethod can-see-p ((mon-looking monster) (mon-looked-at monster))
  (member (cons (get-x mon-looked-at) (get-y mon-looked-at))
		  (get-sight-line (make-instance 'line :x1 (get-x mon-looking) :y1 (get-y mon-looking)
										 :x2 (get-x mon-looked-at) :y2 (get-y mon-looked-at)))
		  :test #'equal))

(defmethod attack ((attacker monster) (defender monster))
  "Have the attacker monster perform an attack on the defender monster"
  ;; FIXME: overly simplistic, although it works for a test
  (let ((hit-p (< (random 1.0) (/ (atk attacker) (+ (atk attacker) (def defender)))))
		(damage (+ (random 3) (round (/ (atk attacker) (def defender))))))
	(cond (hit-p (format-message ("~A attacks ~A for ~A damage!" (get-name attacker) (get-name defender) damage) :color :cred)
				 (setf (get-hp defender) (- (get-hp defender) damage))
				 (if (<= (get-hp defender) 0)
					 (kill-monster defender)))
		  (t (format-message ("~A lunges at ~A and misses!" (get-name attacker) (get-name defender)) :color :cred)))))

(defmethod move-mon ((mon monster) dx dy)
  (setf (get-x mon) (+ (get-x mon) dx))
  (setf (get-y mon) (+ (get-y mon) dy)))

(init-print monster '(x y char name color blocks blocks-sight pickup hp max-hp attack defense ai death))

(defclass half-player (monster)
  ((inventory :initform nil :initarg :inv :accessor get-inv)))

(init-print half-player '(x y color blocks blocks-sight pickup inventory))

;; NOTE: This gets quite complicated because of the restrictions of an object system
(defclass player (monster)
  ((p1 :initform (make-instance 'half-player) :initarg :p1 :accessor get-p1)
   (p2 :initform (make-instance 'half-player) :initarg :p2 :accessor get-p2)
   (name :initform (gen-roman-name) :accessor get-name)))

(init-print player '(p1 p2 name x y hp max-hp attack defense ai death))

(defun move-player (dx dy)
  "Moves the player if possible"
  (when (and (not (blocked-p (+ (get-x (get-p1 *player*)) dx) (+ (get-y (get-p1 *player*)) dy)))
			 (not (blocked-p (+ (get-x (get-p2 *player*)) dx) (+ (get-y (get-p2 *player*)) dy))))
	(move-mon (get-p1 *player*) dx dy)
	(move-mon (get-p2 *player*) dx dy)
	(fov-calculate)))

(defun separate-player ()
  "Moves the player units farther apart if possible"
  (defun mon-away-direction (mon pt)
	(let* ((x (car pt))
		   (y (cdr pt))
		   (dist (sqrt (+ (expt (- x (get-x mon)) 2) (expt (- y (get-y mon)) 2))))
		   (dx (round (/ (- x (get-x mon))
						 dist)))
		   (dy (round (/ (- y (get-y mon))
						 dist))))
	  (cons (- dx) (- dy))))
  
  (let* ((ln (make-instance 'line
						   :x1 (get-x (get-p1 *player*)) :y1 (get-y (get-p1 *player*))
						   :x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*))))
		 (p1-dir (mon-away-direction (get-p1 *player*) (center ln)))
		 (p2-dir (mon-away-direction (get-p2 *player*) (center ln))))
	(when (and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
			 (not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
	  (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
	  (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
	  (fov-calculate))))

(defun gather-player ()
  "Moves the player units closer together if possible"
  (defun mon-towards-direction (mon pt)
	(let* ((x (car pt))
		   (y (cdr pt))
		   (dist (sqrt (+ (expt (- x (get-x mon)) 2) (expt (- y (get-y mon)) 2))))
		   (dx (round (/ (- x (get-x mon))
						 dist)))
		   (dy (round (/ (- y (get-y mon))
						 dist))))
	  (cons dx dy)))
  
  (let* ((ln (make-instance 'line
						   :x1 (get-x (get-p1 *player*)) :y1 (get-y (get-p1 *player*))
						   :x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*))))
		 (p1-dir (mon-towards-direction (get-p1 *player*) (center ln)))
		 (p2-dir (mon-towards-direction (get-p2 *player*) (center ln))))
	(when (and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
			   (not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir))))
			   (or (not (= (+ (get-x (get-p1 *player*)) (car p1-dir)) ; For now I just check to avoid moving to the center, but eventually I would like to treat it as two attacks on the center point
						   (+ (get-x (get-p2 *player*)) (car p2-dir))))
				   (not (= (+ (get-y (get-p1 *player*)) (cdr p1-dir))
						   (+ (get-y (get-p2 *player*)) (cdr p2-dir))))))
	  (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
	  (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
	  (fov-calculate))))

(defun rotate-player (direction)
  "Rotate player in a clockwise or counter direction"
  (defun find-rect (center pt size)
	"Brute force iterative method for finding a rectangle with specified center containing edge point pt"
	(if (not (member pt
					 (wall-points (make-instance 'rect
												 :x (- (car center) size) :y (- (cdr center) size)
												 :width (+ 1 (* 2 size)) :height (+ 1 (* 2 size))))
					 :test #'equal))
		(find-rect center pt (+ size 1))
		(make-instance 'rect
					   :x (- (car center) size) :y (- (cdr center) size)
					   :width (+ 1 (* 2 size)) :height (+ 1 (* 2 size)))))
  
  (let ((ln (make-instance 'line
						   :x1 (get-x (get-p1 *player*)) :y1 (get-y (get-p1 *player*))
						   :x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*)))))

	;; FIXME: This is a mess because I got rid of the abstraction in order to check for blockage "in parallel"
	(cond ((eq direction 'clockwise)
		   (let ((p1-dir (let* ((pts (reverse (wall-points (find-rect (center ln) (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*))) 1))))
								(subsequent (member (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
													pts :test #'equal)))
						   (if (> (length subsequent) 1)
							   (let ((goto (cadr subsequent)))
								 (cons (- (car goto) (get-x (get-p1 *player*)))
									   (- (cdr goto) (get-y (get-p1 *player*)))))
							   (let ((goto (car pts)))
								 (cons (- (car goto) (get-x (get-p1 *player*)))
									   (- (cdr goto) (get-y (get-p1 *player*))))))))
				 (p2-dir (let* ((pts (reverse (wall-points (find-rect (center ln) (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))) 1))))
								(subsequent (member (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*)))
													pts :test #'equal)))
						   (if (> (length subsequent) 1)
							   (let ((goto (cadr subsequent)))
								 (cons (- (car goto) (get-x (get-p2 *player*)))
									   (- (cdr goto) (get-y (get-p2 *player*)))))
							   (let ((goto (car pts)))
								 (cons (- (car goto) (get-x (get-p2 *player*)))
									   (- (cdr goto) (get-y (get-p2 *player*)))))))))
			 (when (and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
						(not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
			   (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
			   (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
			   (fov-calculate))))
		  ((eq direction 'counter)
(let ((p1-dir (let* ((pts (wall-points (find-rect (center ln) (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*))) 1)))
								(subsequent (member (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
													pts :test #'equal)))
						   (if (> (length subsequent) 1)
							   (let ((goto (cadr subsequent)))
								 (cons (- (car goto) (get-x (get-p1 *player*)))
									   (- (cdr goto) (get-y (get-p1 *player*)))))
							   (let ((goto (car pts)))
								 (cons (- (car goto) (get-x (get-p1 *player*)))
									   (- (cdr goto) (get-y (get-p1 *player*))))))))
				 (p2-dir (let* ((pts (wall-points (find-rect (center ln) (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))) 1)))
								(subsequent (member (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*)))
													pts :test #'equal)))
						   (if (> (length subsequent) 1)
							   (let ((goto (cadr subsequent)))
								 (cons (- (car goto) (get-x (get-p2 *player*)))
									   (- (cdr goto) (get-y (get-p2 *player*)))))
							   (let ((goto (car pts)))
								 (cons (- (car goto) (get-x (get-p2 *player*)))
									   (- (cdr goto) (get-y (get-p2 *player*)))))))))
			 (when (and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
						(not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
			   (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
			   (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
			   (fov-calculate))))
		  (t
		   (error "Improper direction ~A" direction)))))

(defclass shape ()
  ((pts :initform nil :accessor points)))

(init-print shape '(pts))

(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen"))

(defgeneric center (shape)
  (:documentation "Returns the center of the given shape as a cons pair"))

(defgeneric random-point-in (shape)
  (:documentation "Returns a random point inside of the given shape"))

(defmethod random-point-in ((sh shape))
  (let ((pts (points sh)))
	(nth (random (length pts)) pts)))

(defmethod draw ((sh shape))
  (dolist (pt (points sh) sh)
	(setf (slot-value (aref *map* (car pt) (cdr pt)) 'blocks) nil)
	(setf (slot-value (aref *map* (car pt) (cdr pt)) 'blocks-sight) nil)))

(defclass rect (shape)
  ((x1 :initarg :x :accessor get-x1)
   (y1 :initarg :y :accessor get-y1)
   (w :initarg :width)
   (h :initarg :height)
   (x2 :accessor get-x2)
   (y2 :accessor get-y2)))

(defmethod initialize-instance :after ((rm rect) &key)
  (let ((x1 (get-x1 rm)) (y1 (get-y1 rm))
		(h (slot-value rm 'h)) (w (slot-value rm 'w)))
	(setf (get-x2 rm) (+ x1 w))
	(setf (get-y2 rm) (+ y1 h)))
  (let ((pts nil) (x1 (get-x1 rm)) (y1 (get-y1 rm))
		(x2 (get-x2 rm)) (y2 (get-y2 rm)))
	(dostep (x (+ x1 1) (- x2 2))
			(dostep (y (+ y1 1) (- y2 2))
					(setf pts (cons (cons x y) pts))))
	(setf (points rm) pts)))

(init-print rect '(x1 y1 w h x2 y2 pts))

(defgeneric wall-points (rect)
	(:documentation "Returns the points of the walls of the given rectangle in clockwise order starting from the top left corner"))

(defmethod wall-points ((rm rect))
  (let ((pts nil) (x1 (get-x1 rm)) (y1 (get-y1 rm))
		(x2 (get-x2 rm)) (y2 (get-y2 rm)))
	(dostep (x x1 (- x2 1))
			(setf pts (cons (cons x y1) pts)))
	(dostep (y (+ y1 1) (- y2 2))
			(setf pts (cons (cons (- x2 1) y) pts)))
	(dostep (x (- x2 1) x1 -1)
			(setf pts (cons (cons x (- y2 1)) pts)))
	(dostep (y (- y2 2) (+ y1 1) -1)
			(setf pts (cons (cons x1 y) pts)))
	pts))			
		
(defclass line (shape)
  ((x1 :initarg :x1 :accessor get-x1)
   (y1 :initarg :y1 :accessor get-y1)
   (x2 :initarg :x2 :accessor get-x2)
   (y2 :initarg :y2 :accessor get-y2)))

(defmethod initialize-instance :after ((ln line) &key)
  ;; Bresenham line drawing to get all points along the line
  ;; Unfortunately, this doesn't make code reuse very easy, but I tried to comment the tricky parts
  (let ((x (get-x1 ln)) (y (get-y1 ln))
		(x-end (get-x2 ln)) (y-end (get-y2 ln))
		(dy (- (get-y2 ln) (get-y1 ln))) (dx (- (get-x2 ln) (get-x1 ln)))
		x-inc y-inc to-iter (e 0))

	;; Test to find which point to plot next
	(defun bham-test (dx dy e)
	  (< (* 2 (+ e dy)) dx)) ; This uses a modification so the algorithm is only handling integer values

	;; Initialization, set x-inc, y-inc, and to-iter (and maybe dx and dy for convenience) so that we can use the same code for all 8 Bresenham cases. The variable to-iter is either 'x or 'y depending on which one we're iterating over (it's the value we add to every time)
	(cond ((and (<= dy dx) (>= dy 0)) ; +x +y 0 <= dy/dx <= 1
		   (setf x-inc 1)
		   (setf y-inc 1)
		   (setf to-iter 'x))
		  ((and (> dy dx) (>= dx 0)) ; +x +y dy/dx > 1
		   (setf dy (prog2 0 dx (setf dx dy))) ; I knew HAKMEM 163 would come in handy some day. This is because we are incrementing along y, so we pretend that dx <-> dy to maximize code reuse.
		   (setf to-iter 'y)
		   (setf x-inc 1)
		   (setf y-inc 1))
		  ((and (<= dy 0) (< dx 0) (<= (abs dy) (abs dx))) ; -x -y 0 <= dy/dx <= 1
		   (setf dx (- dx))
		   (setf dy (- dy))
		   (setf x-inc -1)
		   (setf y-inc -1)
		   (setf to-iter 'x))
		  ((and (< dy 0) (<= dx 0) (> (abs dy) (abs dx))) ; -x -y dy/dx > 1
		   (setf dx (- dx))
		   (setf dy (- dy))
		   (setf x-inc -1)
		   (setf y-inc -1)
		   (setf to-iter 'y)
		   (setf dy (prog2 0 dx (setf dx dy))))
		  ((and (<= (abs dy) (abs dx)) (> dx 0) (<= dy 0)) ; +x -y 0 <= abs(dy/dx) <= 1
		   (setf x-inc 1)
		   (setf y-inc -1)
		   (setf to-iter 'x)
		   (setf dy (- dy))) ; We pretend here that dy is positive to keep error calculations simple
		  ((and (> (abs dy) (abs dx)) (>= dx 0) (< dy 0)) ; +x -y abs(dy/dx) > 1
		   (setf x-inc 1)
		   (setf y-inc -1)
		   (setf to-iter 'y)
		   (setf dy (- dy))
		   (setf dy (prog2 0 dx (setf dx dy))))
		  ((and (<= (abs dy) (abs dx)) (< dx 0) (>= dy 0)) ; -x +y 0 <= abs(dy/dx) <= 1
		   (setf x-inc -1)
		   (setf y-inc 1)
		   (setf to-iter 'x)
		   (setf dx (- dx)))
		  ((and (> (abs dy) (abs dx)) (<= dx 0) (> dy 0)) ; -x +y abs(dy/dx) > 1
		   (setf x-inc -1)
		   (setf y-inc 1)
		   (setf to-iter 'y)
		   (setf dx (- dx))
		   (setf dy (prog2 0 dx (setf dx dy))))
		  (t (error "Line does not fulfill Bresenham requirements")))

	;; Perform the actual algorithm
	(cond ((eq to-iter 'x)
		   (setf (points ln) (cons (cons x y) (points ln)))
		   (while (or (not (= x x-end)) (not (= y y-end)))
			 (bounds-check x y)
			 (if (bham-test dx dy e)
				 (progn (setf e (+ e dy))
						(setf x (+ x x-inc))
						(setf (points ln) (cons (cons x y) (points ln))))
				 (progn (setf e (- (+ e dy) dx))
						(setf x (+ x x-inc))
						(setf y (+ y y-inc))
						(setf (points ln) (cons (cons x y) (points ln)))))))
		  ((eq to-iter 'y)
		   (setf (points ln) (cons (cons x y) (points ln)))
		   (while (or (not (= x x-end)) (not (= y y-end)))
			 (bounds-check x y)
			 (if (bham-test dx dy e)
				 (progn (setf e (+ e dy))
						(setf y (+ y y-inc))
						(setf (points ln) (cons (cons x y) (points ln))))
				 (progn (setf e (- (+ e dy) dx))
						(setf x (+ x x-inc))
						(setf y (+ y y-inc))
						(setf (points ln) (cons (cons x y) (points ln)))))))
		  (t (error "improper value of to-iter ~A" to-iter))))

  ;; Reverse the list, just so we have it in order for LoS check
  (setf (points ln) (reverse (points ln))))

(init-print line '(x1 y1 x2 y2 pts))

(defmethod center ((ln line))
  (cons (round (/ (+ (get-x1 ln) (get-x2 ln)) 2)) (round (/ (+ (get-y1 ln) (get-y2 ln)) 2))))

(defmethod get-sight-line ((ln line))
  (let ((sight-points nil))
	(dolist (pt (points ln) sight-points)
	  (setf sight-points (cons pt sight-points))
	  (when (sight-blocked-p (car pt) (cdr pt))
		(return sight-points)))))

(defmethod see-along ((ln line))
  (dolist (pt (get-sight-line ln))
	(when (not (null pt))
	  (setf (lit (aref *map* (car pt) (cdr pt))) t)
	  (when (not (explored-p (aref *map* (car pt) (cdr pt))))
		(setf (explored-p (aref *map* (car pt) (cdr pt))) t))))
  ;; (dolist (pt (points ln))
  ;; 	(setf (lit (aref *map* (car pt) (cdr pt))) t)
  ;; 	(when (not (explored-p (aref *map* (car pt) (cdr pt))))
  ;; 	  (setf (explored-p (aref *map* (car pt) (cdr pt))) t))
  ;; 	(when (member t (cons (blocks-sight-p (aref *map* (car pt) (cdr pt)))
  ;; 						  (mapcar #'blocks-sight-p
  ;; 								  (remove-if-not (lambda (obj) (and (= (get-x obj) (car pt))
  ;; 																	(= (get-y obj) (cdr pt))))
  ;; 												 *objects*))))
  ;; 	  (return t)))
  )


(defclass message ()
  ((color :initarg :color :accessor get-color)
   (str :initarg :str :accessor get-string)))

(init-print message '(color str))

;;; Globals

(defconstant *screen-width* 50)
(defconstant *screen-height* 20)

(defconstant *max-room-size* 20)
(defconstant *min-room-size* 10)

(defconstant *max-iterations* 3)

(defconstant *fov-radius* 5)

(defparameter *map* (make-array (list *screen-width* *screen-height*)))

(defparameter *objects* nil)

(defparameter *game-state* nil)

(defparameter *messages* nil)

(defparameter *player* (make-instance 'player
									  :p1 (make-instance 'half-player :x 10 :y 10 :blocks-sight nil)
									  :p2 (make-instance 'half-player :x 12 :y 10 :blocks-sight nil)
									  :hp 10
									  :atk 5
									  :def 5))

;;; Map Functions

(defun bounds-check (x y)
  (when (or (>= x *screen-width*) (< x 0)
			(>= y *screen-height*) (< y 0))
	(error "Tried to access point (~A . ~A) outside of the bounds of the map" x y)))

(defun points-within (distance point)
  (let (pts)
	(dostep (x (if (< (- (car point) distance) 0)
				   0
				   (- (car point) distance))
			   (if (> (+ (car point) distance) (- *screen-width* 1))
				   (- *screen-width* 1)
				   (+ (car point) distance)))
			(dostep (y (if (< (- (cdr point) distance) 0)
						   0
						   (- (cdr point) distance))
					   (if (> (+ (cdr point) distance) (- *screen-height* 1))
						   (- *screen-height* 1)
						   (+ (cdr point) distance)))
					(setf pts (cons (cons x y) pts))))
	pts))

(defun init-map ()
  "Initializes the *map* global variable"
  (dostep (x 0 (- *screen-width* 1))
	   (dostep (y 0 (- *screen-height* 1))
			   (setf (aref *map* x y) (make-instance 'tile :blocks t))))

  (defun big-open-level ()
	"Uses 3 different dungeon generation methods in unison in an attempt to create a big open level with roughly rectangular rooms and a directional component"
	(let* ((shapes (split-dungeon 0 1 1 (- *screen-width* 2) (- *screen-height* 2)))
		   (rects (remove-if-not (lambda (x) (instance-of-p x 'rect)) shapes))
		   (lines (remove-if-not (lambda (x) (instance-of-p x 'line)) shapes)))

	  (dolist (r rects)
		(draw r))

	  (smooth-dungeon 1)

	  (dolist (l lines)
		(draw l)))
	
	(direct-dungeon :windyness (random 100) :roughness (random 50) :complexity 1)

	;;Generate pillars
	(dotimes (i (+ 30 (random 40)))
	  (setf (aref *map*
				  (+ 2 (random (- *screen-width* 5)))
				  (+ 2 (random (- *screen-height* 5))))
			(make-instance 'tile :blocks t))))

  (defun two-corridor-level ()
	;; TODO: This will be a level with two separate corridors, forcing the player to split his force
	(direct-dungeon :windyness (random 20) :roughness (random 20) :complexity 2))

  (big-open-level))

(defun get-random-rect (x y width height)
  "Get a random rectangle within the \"dungeon\" rectangle defined by x y width height"
  (when (or (< width *min-room-size*)
			(< height *min-room-size*))
	(error "room too small ~A ~A ~A ~A" x y width height))
  (let* ((new-width (cond ((= width *min-room-size*)
						   *min-room-size*)
						  ((< width *max-room-size*)
						   (+ (random (- width *min-room-size*)) *min-room-size*))
						  (t (+ (random (- *max-room-size* *min-room-size*)) *min-room-size*))))
		 (new-height (cond ((= height *min-room-size*)
							*min-room-size*)
						   ((< height *max-room-size*)
							(+ (random (- height *min-room-size*)) *min-room-size*))
						   (t (+ (random (- *max-room-size* *min-room-size*)) *min-room-size*))))
		 (new-x (if (= width new-width)
					x
					(+ x (random (- width new-width)))))
		 (new-y (if (= height new-height)
					y
					(+ y (random (- height new-height))))))
	(make-instance 'rect :x new-x :y new-y :width new-width :height new-height)))

(defun get-random-split (a b)
  "Get a random split between a and b, where a is less than b"
  (if (> (- (+ b 4) (* *min-room-size* 2) a) 0) ; Split keeping *min-room-size* on either end of the split
	  (+ a (- *min-room-size* 2) (random (- (+ b 4) (* *min-room-size* 2) a)))
	  (error "too small to split")))

;; BSP Dungeon Generator
(defun split-dungeon (iter x y width height)
  "Generates a dungeon using a recursive BSP process and returns a list of shapes which compose that dungeon"
  
  (defun horizontal-split ()
	(let ((split (get-random-split y (+ y height))))
	  ;;(color-line (verify-line (make-instance 'line :x1 x :y1 split :x2 (- (+ x width) 1) :y2 split)) :cpurple)
	  (let* ((r1 (split-dungeon (+ iter 1) x y width (- split y)))
			 (r2 (split-dungeon (+ iter 1) x (+ split 1) width (- (+ y height) split 1)))
			 (pt1 (random-point-in (car r1)))
			 (pt2 (random-point-in (car r2)))
			 (ln (make-instance 'line :x1 (car pt1) :y1 (cdr pt1)
								:x2 (car pt2) :y2 (cdr pt2))))
		(cons ln (append r1 r2)))))
  
  (defun vertical-split ()
	(let ((split (get-random-split x (+ x width))))
	  ;;(color-line (verify-line (make-instance 'line :x1 split :y1 y :x2 split :y2 (- (+ y height) 1))) :cpurple)
	  (let* ((r1 (split-dungeon (+ iter 1) x y (- split x) height))
			 (r2 (split-dungeon (+ iter 1) (+ split 1) y (- (+ x width) split 1) height))
			 (pt1 (random-point-in (car r1)))
			 (pt2 (random-point-in (car r2)))
			 (ln (make-instance 'line :x1 (car pt1) :y1 (cdr pt1)
									   :x2 (car pt2) :y2 (cdr pt2))))
		(cons ln (append r1 r2)))))

  (cond ((and (> (+ width 4) (* *min-room-size* 2))
			  (> (+ height 4) (* *min-room-size* 2))
			  (< iter *max-iterations*))
		 (if (= (random 2) 1)
			 (vertical-split)
			 (horizontal-split)))
		((and (< iter *max-iterations*)
			  (> (+ width 4) (* *min-room-size* 2)))
		 (vertical-split))
		((and (< iter *max-iterations*)
			  (> (+ height 4) (* *min-room-size* 2)))
		 (horizontal-split))
		(t
		 (let ((r (get-random-rect (- x 1) (- y 1) (+ width 2) (+ height 2))))
		   ;; (setf (get-x *player*) (car (random-point-in r)))
		   ;; (setf (get-y *player*) (cdr (random-point-in r)))
		   (list r)))))

;; Directional Dungeon Generator
(defun direct-dungeon (&key (length (- *screen-width* 4)) (roughness 50) (windyness 50) (complexity 1))
  "Generates a cavelike dungeon left to right"
  (let ((x 1)
		(start 1)
		(y (+ 1 (random (- *screen-height* 10))))
		(height (+ 3 (random 3))))
	(draw (make-instance 'line :x1 x :y1 y :x2 x :y2 (+ y height)))
	(when (= complexity 1)
	  (let ((stair-loc (random (- height 2))))
		(setf (get-x (get-p1 *player*)) x)
		(setf (get-y (get-p1 *player*)) (+ y stair-loc))
		(setf (get-x (get-p2 *player*)) x)
		(setf (get-y (get-p2 *player*)) (+ y stair-loc 2))
		(setf *objects* (cons (make-instance 'obj :name "upstair" :x x :y (+ y stair-loc 2) :char #\<)
							  (cons (make-instance 'obj :name "upstair" :x x :y (+ y stair-loc) :char #\<) *objects*)))))
	(while (<= (- x start) length)
	  (setf x (+ x 1))
	  (when (<= (random 100) roughness)
		(setf height (+ height (random-list '(-2 -1 1 2))))
		(when (< height 3)
		  (setf height 3))
		(when (> height 6)
		  (setf height 6)))
	  (when (<= (random 100) windyness)
		(setf y (+ y (random-list (if (= height 3)
									  '(-1 1) ;Prevent a situation where the player can't move
									  '(-2 -1 1 2)))))
			
		(when (< y 1)
		  (setf y 1)))
	  (when (> (+ y height) (- *screen-height* 2))
		(setf y (- *screen-height* 2 height)))
	  (draw (make-instance 'line :x1 x :y1 y :x2 x :y2 (+ y height)))
	  )
	(if (> complexity 1)
		(direct-dungeon :length length :windyness windyness :roughness roughness :complexity (- complexity 1))
		(let ((stair-loc (random (- height 2))))
		  (setf *objects* (cons (make-instance 'obj :name "downstair" :x x :y (+ y stair-loc 2) :char #\>)
								(cons (make-instance 'obj :name "downstair" :x x :y (+ y stair-loc) :char #\>) *objects*)))))))

;; Cellular automata dungeon generator
(defun smooth-dungeon (iterations)
  "Smooths out a cave using cellular automata with a 4-5 rule"
  (let ((new-map *map*))
	(dostep (x 1 (- *screen-width* 2))
			(dostep (y 1 (- *screen-height* 2))
					(let ((adjacent-walls (count t (mapcar (lambda (pt)
															 (blocks-p (aref *map* (car pt) (cdr pt))))
														   (points-within 1 (cons x y))))))
					  (setf (aref new-map x y) (make-instance 'tile
															  :blocks (>= adjacent-walls 5)
															  :blocks-sight (>= adjacent-walls 5))))))
	(setf *map* new-map))
  (when (> iterations 1)
	(smooth-dungeon (- iterations 1))))

(defun sight-blocked-p (x y)
  "Checks *map* and *objects* to see if the point x y is blocked from sight"
  (or (blocks-sight-p (aref *map* x y))
	  (member t (mapcar #'blocks-sight-p
						(remove-if-not (lambda (obj) (and (= (get-x obj) x)
														  (= (get-y obj) y)))
									   *objects*)))))

(defun blocked-p (x y)
  "Checks *map* and *objects* to see if the point x y is blocked"
  (or (slot-value (aref *map* x y) 'blocks)
				 (member t (mapcar #'blocks-p (remove-if-not (lambda (obj) (and (= (get-x obj) x) (= (get-y obj) y))) *objects*)))))

;;; Rendering

(defun render-map ()
  "Renders the *map* global variable, which consists of the background tiles"
  (dostep (x 0 (- *screen-width* 1))
	   (dostep (y 0 (- *screen-height* 1))
			(when (explored-p (aref *map* x y))
			  (let ((col (if (lit (aref *map* x y)) :cwhite :cgray)))
				(if (slot-value (aref *map* x y) 'blocks)
					(progn (attron col)
						   (mvaddch y x #\#)
						 (Attroff col))
					(progn (attron col)
						   (mvaddch y x #\.)
						   (Attroff col))))))))

(defun render-objects ()
  "Renders every object in the *objects* list; usually called after the function render-map"
  (dolist (obj *objects*)
	(when (or (lit (aref *map* (get-x obj) (get-y obj)))
			  (and (or (string= (get-name obj) "downstair") (string= (get-name obj) "upstair"))
				   (explored-p (aref *map* (get-x obj) (get-y obj)))))
	  (attron (slot-value obj 'color))
	  (mvaddch (get-y obj) (get-x obj) (slot-value obj 'char))
	  (Attroff (slot-value obj 'color))))
  (dolist (mon (remove-if-not (lambda (obj) (instance-of-p obj 'monster)) *objects*))
	(when (lit (aref *map* (get-x mon) (get-y mon)))
		 (progn (attron (slot-value mon 'color))
				(mvaddch (get-y mon) (get-x mon) (slot-value mon 'char))
				(Attroff (slot-value mon 'color))))))

(defun render-player ()
  (let* ((x1 (get-x (get-p1 *player*))) (y1 (get-y (get-p1 *player*)))
		 (x2 (get-x (get-p2 *player*))) (y2 (get-y (get-p2 *player*)))
		 (cen (center (make-instance 'line :x1 x1 :y1 y1 :x2 x2 :y2 y2))))
	(mvaddch y1 x1 #\@)
	(mvaddch y2 x2 #\@)
	(move (cdr cen) (car cen))))

(defun render-messages ()
  (when (not (null (car *messages*)))
	(let* ((mess (car *messages*))
		   (color (get-color mess))
		   (str (get-string mess)))
	  (attron color)
	  (mvprintw (+ *screen-height* 3) 0 (make-string *screen-width* :initial-element #\Space)) ; Clear message line 3
	  (mvprintw (+ *screen-height* 3) 0 str)
	  (Attroff color))
	(when (not (null (cadr *messages*)))
	  (let* ((mess (cadr *messages*))
			 (color (get-color mess))
			 (str (get-string mess)))
		(attron color)
		(mvprintw (+ *screen-height* 2) 0 (make-string *screen-width* :initial-element #\Space)) ; Clear message line 2
		(mvprintw (+ *screen-height* 2) 0 str)
		(Attroff color))
	  (when (not (null (caddr *messages*)))
		(let* ((mess (caddr *messages*))
			   (color (get-color mess))
			   (str (get-string mess)))
		  (attron color)
		  (mvprintw (+ *screen-height* 1) 0 (make-string *screen-width* :initial-element #\Space)) ; Clear message line 1
		  (mvprintw (+ *screen-height* 1) 0 str)
		  (Attroff color))))))

(defun render-all ()
  (fov-calculate)
  (render-map)
  (render-objects)
  (render-messages)
  (stats)
  (render-player)
  (refresh))

;;; Pathfinding

(defun dijkstra-map (&rest sources)
  (defun dmap (source &optional (default 0))
	(let ((path-lengths (make-array (list *screen-width* *screen-height*))) (last source) (known (list (cons (car source) (cdr source)))) selectable)
	  (setf (aref path-lengths (car source) (cdr source)) default)
	  (dostep (x (- (car source) 1) (+ (car source) 1))
			  (dostep (y (- (cdr source) 1) (+ (cdr source) 1))
					  (when (and (not (member (cons x y) known :test #'equal))
								 (not (blocked-p x y)))
						(setf (aref path-lengths x y) 1)
						(setf selectable (cons (cons x y) selectable)))))
	  (while selectable
		(let* ((min-len (reduce #'min
								(mapcar
								 (lambda (x) (aref path-lengths (car x) (cdr x)))
								 (remove-if-not #'identity selectable))))
			   (selected (random-list (remove-if-not
									   (lambda (x) (= (aref path-lengths (car x) (cdr x)) min-len))
									   selectable))))
		  (setf selectable (delete selected selectable :test #'equal))
		  (dostep (x (- (car selected) 1) (+ (car selected) 1))
			(dostep (y (- (cdr selected) 1) (+ (cdr selected) 1))
					(when (and (not (member (cons x y) known :test #'equal))
							   (not (blocked-p x y))
							   (not (and (= x (car selected)) (= y (cdr selected)))))
					  (if (not (aref path-lengths x y))
						  (setf (aref path-lengths x y) (+ (aref path-lengths (car selected) (cdr selected)) 1))
						  (setf (aref path-lengths x y) (min (+ (aref path-lengths (car selected) (cdr selected)) 1) (aref path-lengths x y))))
					  (setf selectable (cons (cons x y) selectable)))))
		  (setf last selected)
		  (setf known (cons selected known))))
	  path-lengths))
  
  (let ((map (make-array (list *screen-width* *screen-height*))))
  	(dolist (src sources)
  	  (let ((temp (dmap src)))
  		(dotimes (x (- *screen-width* 1))
  		  (dotimes (y (- *screen-height* 1))
  			(cond ((null (aref map x y))
  				   (setf (aref map x y) (aref temp x y)))
  				  ((null (aref temp x y))
  				   nil)
  				  ((< (aref temp x y) (aref map x y))
  				   (setf (aref map x y) (aref temp x y))))))))
  	map))

(defun dijkstra-path (dmap start)
  (let* ((allowed-point-vals (remove-if-not #'identity
											(mapcar (lambda (x) (aref dmap (car x) (cdr x)))
													(points-around start))))
		 (min-points 
		  (if (null allowed-point-vals)
			  nil
			  (remove-if-not (lambda (x) (eq (aref dmap (car x) (cdr x))
											 (reduce #'min allowed-point-vals)))
					 (points-around start)))))
	(if (null min-points)
		nil
		(random-list min-points))))

;;; FOV

(defun fov-calculate ()
  (defun fov-reset ()
	(dostep (x 0 (- *screen-width* 1))
			(dostep (y 0 (- *screen-height* 1))
					(setf (lit (aref *map* x y)) nil))))
  (fov-reset)
  
  ;; HAKMEM 149 circle drawing for choice of rays
  (let ((x *fov-radius*) (y *fov-radius*) (e 0.1))
  	(dotimes (i 100)
  		(see-along (make-instance 'line :x1 (get-x (get-p1 *player*)) :y1 (get-y (get-p1 *player*))
  								  :x2 (cond ((>= (+ (get-x (get-p1 *player*)) (round x)) *screen-width*)
  											 (- *screen-width* 1))
  											((< (+ (get-x (get-p1 *player*)) (round x)) 0)
  											 0)
  											(t
  											 (+ (get-x (get-p1 *player*)) (round x))))
  								  :y2 (cond ((>= (+ (get-y (get-p1 *player*)) (round y)) *screen-height*)
  											 (- *screen-height* 1))
  											((< (+ (get-y (get-p1 *player*)) (round y)) 0)
  											 0)
  											(t
  											 (+ (get-y (get-p1 *player*)) (round y))))))
  		(see-along (make-instance 'line :x1 (get-x (get-p2 *player*)) :y1 (get-y (get-p2 *player*))
  								  :x2 (cond ((>= (+ (get-x (get-p2 *player*)) (round x)) *screen-width*)
  											 (- *screen-width* 1))
  											((< (+ (get-x (get-p2 *player*)) (round x)) 0)
  											 0)
  											(t
  											 (+ (get-x (get-p2 *player*)) (round x))))
  								  :y2 (cond ((>= (+ (get-y (get-p2 *player*)) (round y)) *screen-height*)
  											 (- *screen-height* 1))
  											((< (+ (get-y (get-p2 *player*)) (round y)) 0)
  											 0)
  											(t
  											 (+ (get-y (get-p2 *player*)) (round y))))))
  		(setf x (- x (* e y)))
  		(setf y (+ y (* e x)))))
  )

;;; HUD

(defun message (str &key (color :cwhite))
  (push (make-instance 'message :color color :str str) *messages*))

(defun stats ()
  (bar 0 *screen-height* "HP" (get-hp *player*) (get-max-hp *player*) :color :cgreen)
  (mvprintw *screen-height* 25 (format nil "ATK: ~A: DEF: ~A" (get-atk *player*) (get-def *player*))))

(defun bar (x y label val max &key (color :cwhite))
  (mvprintw y x (concatenate 'string
							 label
							 ": [     ] " (format nil "(~A / ~A)" val max)))
  (attron color)
  (mvprintw y (+ x (length label) 3) (concatenate 'string
												  (make-string (floor (* 5 (/ val max))) :initial-element #\=)
												  (if (= (floor (* 5 (/ val max))) (ceiling (* 5 (/ val max))))
													  ""
													  ":")))
  (attroff color))

;;; Input

(defun take-in-char ()
  (curses-code-char (wgetch *stdscr*)))

(defun input ()
  (setf in (take-in-char))
  (cond ((eq in #\j)
  		 (move-player 0 1))
  		((eq in #\k)
  		 (move-player 0 -1))
  		((eq in #\h)
  		 (move-player -1 0))
  		((eq in #\l)
  		 (move-player 1 0))
		((eq in #\y)
		 (move-player -1 -1))
		((eq in #\u)
		 (move-player 1 -1))
		((eq in #\b)
		 (move-player -1 1))
		((eq in #\n)
		 (move-player 1 1))
		((eq in #\s)
		 (separate-player))
		((eq in #\d)
		 (rotate-player 'counter))
		((eq in #\f)
		 (rotate-player 'clockwise))
		((eq in #\g)
		 (gather-player))
		((or (eq in #\Escape) (eq in #\m))
		 (main-menu))))

;;; Main Menu

(defun main-menu ()
  (erase)
  (let ((cursor-pos 0) (max-cursor 6) (options nil))
	(attron :cred)
	(mvprintw 4 5 "PHALANX")
	(attroff :cred)

	(attron :cyellow)
	(mvprintw 7 5 "By Keith Bateman")
	(attroff :cyellow)
	(mvprintw 10 5 "n) New Game")
	(mvprintw 11 5 "s) Save Game")
	(mvprintw 12 5 "c) Continue Saved Game")
	(mvprintw 13 5 "r) Resume Game")
	(mvprintw 14 5 "q) Quit")
	(mvprintw 15 5 "?) Help")
	  
	(setf options (acons 0 #\n options))
	(setf options (acons 1 #\s options))
	(setf options (acons 2 #\c options))
	(setf options (acons 3 #\r options))
	(setf options (acons 4 #\q options))
	(setf options (acons 5 #\? options))

	(defun resume-game ()
	  (when *game-state*
		(erase)
		(render-all)
		t))
	
	(defun new-game ()
	  (setf *objects* nil)
	  (setf *player* (make-instance 'player
									:p1 (make-instance 'half-player :x 10 :y 10 :blocks-sight nil)
									:p2 (make-instance 'half-player :x 12 :y 10 :blocks-sight nil)
									:hp 10
									:atk 5
									:def 5
									:ai '(lambda (mon) (input))
									:death 'player-death))
	  (setf *messages* nil)
	  (setf *map* (make-array (list *screen-width* *screen-height*)))
	  (setf *game-state* t)
	  (init-map)
	  (fov-calculate)
	  (resume-game))
	
	(defun save-game ()
	  "Saves the game in a text file as lisp data"
	  (with-open-file (save-file "phalanx-save.sav"
								 :direction :output
								 :if-exists :supersede
								 :if-does-not-exist :create)
		(write (cons '*player* *player*) :stream save-file)
		(write (cons '*map* *map*) :stream save-file)
		(write (cons '*objects* *objects*) :stream save-file)
		(write (cons '*game-state* *game-state*) :stream save-file)
		(write (cons '*messages* *messages*) :stream save-file))
	  'quit)
	
	(defun continue-game ()
	  "A hairy hack for loading a save game. It's a hairy hack because of CLOS, and I put up with it because of CLOS"
	  (defun load-object (spec)
		"Create a new instance of an object with the given print representation"
		(let ((new-obj (make-instance (car spec))))
		  (dolist (slot-pair (cdr spec))
			(cond ((eq (cdr slot-pair) 'unbound)
				   (slot-makunbound new-obj (car slot-pair)))
				  ((and (listp (cdr slot-pair)) (not (null (cdr slot-pair))) (not (eq (cadr slot-pair)
																					  'lambda)))
				   (setf (slot-value new-obj (car slot-pair)) nil)
				   (dolist (new-spec (cdr slot-pair))
					 (setf (slot-value new-obj (car slot-pair))
						   (cons (load-object new-spec) (slot-value new-obj (car slot-pair))))))
				  ((or (atom (cdr slot-pair)) (and (listp (cdr slot-pair)) (eq (cadr slot-pair) 'lambda)))
				   (setf (slot-value new-obj (car slot-pair))
						 (cdr slot-pair)))))
		  new-obj))					
	  
	  (with-open-file (load-file "phalanx-save.sav"
								 :direction :input
								 ;; :if-does-not-exist nil
								 )
		(let ((parse-file (read load-file nil 'done)) (temp-obj nil))
		  (while (not (eq parse-file 'done))
			(when (find-symbol (symbol-name (car parse-file)))
			  (setf temp-obj (cdr parse-file))
			  (cond ((listp temp-obj)
					 (if (not (listp (car temp-obj)))
						 (set (car parse-file) (load-object temp-obj))
						 (progn
						   (set (car parse-file) nil)
						   (dolist (obj-datum temp-obj)
							 (let ((new-obj (load-object obj-datum)))
							   (set (car parse-file) (cons new-obj (eval (car parse-file)))))))))
					((and (arrayp temp-obj)
						  (= (array-rank temp-obj) 2))
					 (set (car parse-file) (make-array (list (array-dimension temp-obj 0)
															 (array-dimension temp-obj 1))))
					 (dotimes (x (array-dimension temp-obj 0))
					   (dotimes (y (array-dimension temp-obj 1))
						 (let* ((obj-spec (aref temp-obj x y))
								(new-obj (load-object obj-spec)))
						   (setf (aref (eval (car parse-file)) x y)
								 new-obj)))))
					((atom temp-obj)
					 (set (car parse-file) temp-obj))
					(t
					 (error "In file load, could not parse object ~A" temp-obj))))
			(setf parse-file (read load-file nil 'done))
			(setf temp-obj nil)))
		(resume-game)))
	
	(defun process-input (in)
	  (cond ((eq in #\j)
			 (setf cursor-pos (mod (+ 1 cursor-pos) max-cursor))
			 t)
			((eq in #\k)
			 (setf cursor-pos (mod (- cursor-pos 1) max-cursor))
			 t)
			((eq in #\h)
			 (setf cursor-pos (mod (- cursor-pos 1) max-cursor))
			 t)
			((eq in #\l)
			 (setf cursor-pos (mod (+ 1 cursor-pos) max-cursor))
			 t)
			((or (eq in #\Space) (eq in #\Return) (eq in #\Newline))
			 (process-input (cdr (assoc cursor-pos options))))
			((eq in #\?)
			 ;; FIXME: display help
			 t)
			((or (eq in #\n) (eq in #\N))
			 (new-game)
			 nil)
			((or (eq in #\s) (eq in #\S))
			 (save-game))
			((or (eq in #\r) (eq in #\R))
			 (not (resume-game)))
			((or (eq in #\c) (eq in #\C))
			 (continue-game)
			 nil)
			((or (eq in #\q) (eq in #\Q))
			 (setf *game-state* nil) ; FIXME: This should probably also delete save files and display a warning prompt
			 'quit)
			(t t)))
	
	(while t
	  (move (+ 10 cursor-pos) 5)
	  (refresh)
	  (let ((in (process-input (curses-code-char (wgetch *stdscr*)))))
		(when (not in)
		  (return t))
		(when (eq in 'quit)
		  (return 'quit))))))

;;; Control Flow

(defun main ()
  (setf *random-state* (make-random-state t))
  (connect-console)
  (noecho)
  (main-menu)
  (while (and *game-state*
			  (prog1 (not (eq (take-turn *player*) 'quit))
				(dolist (mon (remove-if-not (lambda (obj) (instance-of-p obj 'monster)) *objects*))
				  (take-turn mon))))
	(render-all))
  (close-console)
  #+clisp (ext:quit))

#+clisp
(defun make-exec ()
  (ext:saveinitmem "phalanx" :quiet t :init-function #'main :executable t :norc t))
