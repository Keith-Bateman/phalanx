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
  "Takes a random element from a list using equal weight"
  (nth (random (length lst)) lst))

(defun random-list-weighted (lst)
  "Takes a list where each element is a cons of a probability (out of 100) and an element and takes a random element from that list"
  (let ((num (random 100))
		(weights 0)
		(retval nil))
	(mapcar (lambda (elem) (if (< num (+ weights (car elem)))
							   (progn (setf retval (cdr elem))
									  (setf num 101))
							   (setf weights (+ weights (car elem)))))
			lst)
	retval))

(defun random-probability (&optional (percentage 50))
  "Takes a percentage out of 100 and returns t or nil randomly"
  (< (random 100) percentage))

(defun random-deviation (num &key (zero-p nil))
  "Gives a random number from -num to +num inclusive, possibly including zero"
  (let ((chosen (- (random (+ 1 (* 2 num))) num)))
	(if (and (not zero-p)
			 (= chosen 0))
		(random-deviation num)
		chosen)))

(defun bound (num boundary &optional (lower-limit nil))
  "Gives num if num is less than boundary, otherwise gives boundary"
  (if (> num boundary)
	  boundary
	  (if (and lower-limit (< num lower-limit))
		  lower-limit
		  num)))

;;; Name generator

(defun gen-roman-name ()
  (let ((praenomen (list "Gaius" "Lucius" "Marcus" "Publius" "Quintus" "Titus" "Tiberius" "Sextus" "Aulus" "Decimus" "Gnaeus" "Spurius" "Manius" "Servius" "Appius" "Numerius"))
		(nomen (list "Annius" "Antonius" "Arrius" "Artorius" "Asinius" "Atilius" "Atius" "Aurelius" "Autronius" "Caecilius" "Caedicius" "Caelius" "Calidius" "Calpurnius" "Cassius" "Claudius" "Cloelius" "Cocceius" "Cominius" "Cornelius" "Coruncanius" "Curiatius" "Curius" "Curtius" "Decius" "Didius" "Domitius" "Duilius" "Durmius" "Equitius" "Fabius" "Fabricius" "Fannius" "Flavius" "Fulvius" "Furius" "Gabinius" "Galerius" "Geganius" "Gellius" "Geminius" "Genucius" "Gratius" "Herennius" "Hirtius" "Horatius" "Hortensius" "Hostilius" "Iulius" "Iunius" "Iuventius" "Laelius" "Lartius" "Licinius" "Livius" "Lucilius" "Lucretius" "Manlius" "Marcius" "Marius" "Memmius" "Menenius" "Minicius" "Minius" "Minucius" "Modius" "Mucius" "Naevius" "Nautius" "Numerius" "Numicius" "Octavius" "Ovidius" "Papirius" "Petronius" "Pinarius" "Pompeius" "Pompilius" "Pontius" "Popillius" "Porcius" "Postumius" "Quinctilius" "Quinctius" "Rubellius" "Rufius" "Rutilius" "Sallustius" "Salonius" "Salvius" "Scribonius" "Seius" "Sempronius" "Sentius" "Sergius" "Sertorius" "Servilius" "Sextius" "Sicinius" "Suetonius" "Sulpicius" "Tarpeius" "Tarquitius" "Terentius" "Titinius" "Titurius" "Tuccius" "Tullius" "Ulpius" "Valerius" "Vedius" "Velleius" "Vergilius" "Verginius" "Vibius" "Villius" "Vipsanius" "Vitellius" "Vitruvius" "Volumnius"))
		(cognomen (list "Aculeo" "Agricola" "Agrippa" "Ahala" "Ahenobarbus" "Albinus" "Albus" "Ambustus" "Annalis" "Aquila" "Aquilinus" "Arvina" "Asellio" "Asina" "Atellus" "Avitus" "Balbus" "Barba" "Barbatus" "Bassus" "Bestia" "Bibaculus" "Bibulus" "Blaesus" "Brocchus" "Brutus" "Bubulcus" "Bucco" "Bulbus" "Buteo" "Caecus" "Caepio" "Caesar" "Calidus" "Calvinus" "Calvus" "Camillus" "Caninus" "Canus" "Capito" "Carbo" "Catilina" "Cato" "Catulus" "Celer" "Celsus" "Cethegus" "Cicero" "Cicurinus" "Cilo" "Cincinnatus" "Cinna" "Cordus" "Cornicen" "Cornutus" "Corvinus" "Corvus" "Cossus" "Costa" "Cotta" "Crassipes" "Crassus" "Crispinus" "Crispus" "Culleo" "Curio" "Cursor" "Curvus" "Denter" "Dento" "Dives" "Dolabella" "Dorsuo" "Drusus" "Figulus" "Fimbria" "Flaccus" "Flavus" "Florus" "Fronto" "Fullo" "Fusus" "Galeo" "Gemellus" "Glabrio" "Gracchus" "Gurges" "Habitus" "Helva" "Imperiosus" "Iullus" "Labeo" "Lactuca" "Laenas" "Lanatus" "Laevinus" "Laterensis" "Lentulus" "Lepidus" "Libo" "Licinus" "Longus" "Lucullus" "Lupus" "Lurco" "Macer" "Macula" "Malleolus" "Mamercus" "Marcellus" "Maro" "Merenda" "Mergus" "Merula" "Messalla" "Metellus" "Murena" "Mus" "Musca" "Nasica" "Naso" "Natta" "Nepos" "Nero" "Nerva" "Niger" "Novellus" "Ocella" "Pacilus" "Paetus" "Pansa" "Papus" "Paterculus" "Paullus" "Pavo" "Pera" "Pictor" "Piso" "Plancus" "Plautus" "Poplicola" "Postumus" "Potitus" "Praeconinus" "Praetextatus" "Priscus" "Proculus" "Publicola" "Pulcher" "Pullus" "Pulvillus" "Purpureo" "Quadratus" "Ralla" "Regillus" "Regulus" "Rufus" "Ruga" "Rullus" "Rutilus" "Salinator" "Saturninus" "Scaeva" "Scaevola" "Scapula" "Scaurus" "Scipio" "Scrofa" "Seneca" "Severus" "Silanus" "Silo" "Silus" "Stolo" "Strabo" "Structus" "Sulla" "Sura" "Taurus" "Triarius" "Trigeminus" "Trio" "Tubero" "Tubertus" "Tubulus" "Tuditanus" "Tullus" "Turdus" "Varro" "Varus" "Vatia" "Verres" "Vespillo" "Vetus" "Vitulus" "Volusus")))
	(concatenate 'string (random-list praenomen) " " (random-list nomen) " " (random-list cognomen))))

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

(defclass item (obj)
  ((pickup :initform t)
   (use :initform '(lambda (i p hp) t) :initarg :use :accessor get-use)
   (one-use :initform nil :initarg :one-use :accessor one-use-p)
   (takes-turn :initform nil :initarg :takes-turn :accessor takes-turn-p)
   (atk-bonus :initform 0 :initarg :atk-bonus :accessor get-atk-bonus)
   (def-bonus :initform 0 :initarg :def-bonus :accessor get-def-bonus)
   (max-hp-bonus :initform 0 :initarg :max-hp-bonus :accessor get-max-hp-bonus)))

(init-print item '(x y char name color blocks blocks-sight pickup use one-use takes-turn atk-bonus def-bonus max-hp-bonus))

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
			  (not (null (dijkstra-path (apply #'dijkstra-map (append (list (cons (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
																				  (cons (get-x mon) (get-y mon)))
																			(cons (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*)))
																				  (cons (get-x mon) (get-y mon))))
																	  (mapcar (lambda (g) (cons g (cons (get-x mon) (get-y mon)))) *goals*)))
										(cons (get-x mon) (get-y mon))))))
		 (let ((path (dijkstra-path (apply #'dijkstra-map (append (list (cons (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
																			  (cons (get-x mon) (get-y mon)))
																		(cons (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*)))
																			  (cons (get-x mon) (get-y mon))))
																  (mapcar (lambda (g) (cons g (cons (get-x mon) (get-y mon)))) *goals*)))
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
  (format-message ("~A dies a horrible and painful death" (get-name mon)) :color :cred)
  (setf (get-exp *player*) (+ (get-exp *player*) (cond ((> (get-dlvl *player*) 6)
														50)
													   ((> (get-dlvl *player*) 3)
														30)
													   (t
														10))))
  (when (> (get-exp *player*) (threshold *player*))
	(level-up *player*)))

(defmethod player-death ((mon monster))
  (setf (get-char mon) #\%)
  (setf *game-state* nil)
  (message "You die a lonely and depressing death" :color :cred)
  (render-messages)
  (delete-file "phalanx-save.sav")
  (getch))

(defmethod leader-death ((mon monster))
  ;; NOTE: this removes all goals associated with the leader, even if they're associated with another leader
  (setf *goals* (remove-if (lambda (x) (member x (get-goals mon)))
						   *goals*))
  (setf (get-char mon) #\%)
  (setf (get-ai mon) '(lambda (mon) t))
  (setf (blocks-p mon) nil)
  (format-message ("~A dies a noble death" (get-name mon)) :color :cred)
  (setf (get-exp *player*) (+ (get-exp *player*) 100))
  (when (> (get-exp *player*) (threshold *player*))
	(level-up *player*)))

(defmethod witch-death ((mon monster))
  (setf *goals* (remove-if (lambda (x) (member x (get-goals mon)))
						   *goals*))
  (setf (get-char mon) #\%)
  (setf (get-ai mon) '(lambda (mon) t))
  (setf (blocks-p mon) nil)
  (story 'win)
  (setf *game-state* nil))

(defmethod can-see-p ((mon-looking monster) (mon-looked-at monster))
  (member (cons (get-x mon-looked-at) (get-y mon-looked-at))
		  (get-sight-line (make-instance 'line :x1 (get-x mon-looking) :y1 (get-y mon-looking)
										 :x2 (get-x mon-looked-at) :y2 (get-y mon-looked-at)))
		  :test #'equal))

(defmethod atk ((mon monster))
  (defun bonus-atk (mon)
	(if (typep mon 'player)
		(+ (if (get-inv (get-p1 mon))
			   (reduce #'max (mapcar #'get-atk-bonus (get-inv (get-p1 mon))))
			   0)
		   (if (get-inv (get-p2 mon))
			   (reduce #'max (mapcar #'get-atk-bonus (get-inv (get-p2 mon))))
			   0))
		0))
  (+ (get-atk mon) (bonus-atk mon)))

(defmethod def ((mon monster))
  (defun bonus-def (mon)
	(if (typep mon 'player)
		(+ (if (get-inv (get-p1 mon))
			   (reduce #'max (mapcar #'get-def-bonus (get-inv (get-p1 mon))))
			   0)
		   (if (get-inv (get-p2 mon))
			   (reduce #'max (mapcar #'get-def-bonus (get-inv (get-p2 mon))))
			   0))
		0))
  (+ (get-def mon) (bonus-def mon)))

(defmethod max-hp ((mon monster))
  (defun bonus-max-hp (mon)
	(if (typep mon 'player)
		(+ (if (get-inv (get-p1 mon))
			   (reduce #'max (mapcar #'get-max-hp-bonus (get-inv (get-p1 mon))))
			   0)
		   (if (get-inv (get-p2 mon))
			   (reduce #'max (mapcar #'get-max-hp-bonus (get-inv (get-p2 mon))))
			   0))
		0))
  (+ (get-max-hp mon) (bonus-max-hp mon)))

(defmethod deal-damage ((mon monster) dam)
  (setf (get-hp mon) (- (get-hp mon) dam))
  (if (<= (get-hp mon) 0)
	  (kill-monster mon)))

(defmethod attack ((attacker monster) (defender monster) &optional (hit-message "~A attacks ~A for ~A damage!") (miss-message "~A lunges at ~A and misses!"))
  "Have the attacker monster perform an attack on the defender monster"
  (let ((hit-p (< (random 1.0) (/ (atk attacker) (+ (atk attacker) (def defender)))))
		(damage (+ (random 3) (round (/ (atk attacker) (def defender))))))
	(cond (hit-p (format-message (hit-message (get-name attacker) (get-name defender) damage) :color :cred)
				 (deal-damage defender damage))
		  (t (format-message (miss-message (get-name attacker) (get-name defender)) :color :cred)))))

(defmethod move-mon ((mon monster) dx dy)
  (let ((new-x (+ (get-x mon) dx))
		(new-y (+ (get-y mon) dy)))
	(when (not (blocked-p new-x new-y))
	  (setf (get-x mon) new-x)
	  (setf (get-y mon) new-y))))

(init-print monster '(x y char name color blocks blocks-sight pickup hp max-hp attack defense ai death))

(defclass leader (monster)
  ((goals :initform nil :initarg :goals :accessor get-goals)
   (ai :initform 'self-leader-ai :initarg :ai :accessor get-ai)
   (death :initform 'leader-death :initarg :death :accessor get-death)))

(init-print leader '(x y char name color blocks blocks-sight pickup hp max-hp attack defense ai death goals))

(defmethod add-goal ((l leader) goal)
  (push goal (get-goals l))
  (if (member goal *goals* :test #'equal)
	  goals
	  (push goal *goals*)))

(defmethod self-leader-ai ((l leader))
  (setf *goals* (remove-if (lambda (x) (member x (get-goals l)))
						   *goals*))
  (basic-ai l) ; Basic AI is used to give easy run-away functionality
  (setf (get-goals l) nil) ; Reset goals for better or worse
  (add-goal l (cons (get-x l) (get-y l))))

(defmethod witch-ai ((l leader))
  (setf *goals* (remove-if (lambda (x) (member x (get-goals l)))
						   *goals*))

  (cond ((or (random-probability 20)
			 (= (length (points (make-instance 'line :x1 (get-x l) :y1 (get-y l)
											   :x2 (get-x (get-p1 *player*)) :y2 (get-y (get-p1 *player*))))) 2)
			 (= (length (points (make-instance 'line :x1 (get-x l) :y1 (get-y l)
											   :x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*))))) 2))
		 ;; Run away
		 (let ((map (dijkstra-map (cons (get-x (get-p1 *player*)) (get-y (get-p1 *player*)))
								  (cons (get-x (get-p2 *player*)) (get-y (get-p2 *player*))))))
		   (dotimes (x (- *screen-width* 1))
			 (dotimes (y (- *screen-height* 1))
			   (when (aref map x y)
				 (setf (aref map x y) (* (aref map x y) -1.2)))))
		   (let ((path (dijkstra-path map (cons (get-x l) (get-y l)))))
			 (move-mon l (- (car path) (get-x l)) (- (cdr path) (get-y l))))))
		((or (and (can-see-p l (get-p1 *player*))
				  (< (length (points (make-instance 'line :x1 (get-x l) :y1 (get-y l)
													:x2 (get-x (get-p1 *player*)) :y2 (get-y (get-p1 *player*)))))
					 6))
			 (and (can-see-p l (get-p2 *player*))
				  (< (length (points (make-instance 'line :x1 (get-x l) :y1 (get-y l)
													:x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*)))))
					 6)))
		 ;; Attack
		 (defun spawn-specter ()
		   (let ((spawn-points (remove-if-not (lambda (pt) (not (blocked-p (car pt) (cdr pt))))											  
											  (cdr (butlast (points (make-instance 'line :x1 (get-x l) :y1 (get-y l)
																				   :x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*))))
															1)))))
			 (if (not (null spawn-points))
				 (push (make-instance 'monster :x (caar spawn-points) :y (cdar spawn-points)
									  :hp (+ 100 (random-deviation 5 :zero-p t))
									  :atk (+ 30 (random-deviation 2 :zero-p t))
									  :def (+ 30 (random-deviation 2 :zero-p t))
									  :name "Specter" :char #\s :color :cyellow :blocks t)
					   *objects*)
				 (message "The Witch gestures wildly"))))
		 
		 (eval (random-list-weighted (list (cons 40 `(message "The Witch gestures wildly"))
										   (cons 30 `(spawn-specter))
										   (cons 30 `(progn (attack ,l ,*player* "~A zaps ~A with lightning for ~A damage" "~A zaps lightning which shoots past ~A"))))))))
  
  (setf (get-goals l) nil) ; Reset goals for better or worse
  (add-goal l (cons (get-x l) (get-y l))))

(defclass half-player (monster)
  ((inventory :initform nil :initarg :inv :accessor get-inv)))

(init-print half-player '(x y color blocks blocks-sight pickup inventory))

(defgeneric pickup (monster)
  (:documentation "Pick up the item where the monster is standing"))

(defmethod pickup ((p half-player))
  (if (< (length (get-inv p)) *inventory-size*)
	  (let ((objs (remove-if-not (lambda (obj) (and (= (get-x obj) (get-x p))
													(= (get-y obj) (get-y p))
													(pickup-p obj)))
								 *objects*)))
		(when (not (null (car objs)))
		  (push (car objs) (get-inv p))
		  (setf *objects* (delete (car objs) *objects* :count 1)))
		(car objs))
	  (progn (message "Inventory full")
			 nil)))

(defmethod drop-item ((i item) (p half-player))
  (setf (get-x i) (get-x p))
  (setf (get-y i) (get-y p))
  (push i *objects*)
  (setf (get-inv p) (delete i (get-inv p) :count 1)))

(defmethod power-strike ((p half-player) direction damage &key (hit-message "~A gets hit") (hit-message-color :cwhite))
  "A super-powered attack"
  (mapcar (lambda (mon)
			(deal-damage mon damage)
			(format-message (hit-message (get-name mon)) :color hit-message-color))
		  (remove-if-not (lambda (obj) (and (typep obj 'monster)
											(not (char= (get-char obj) #\%))
											(= (get-x obj) (+ (get-x p) (car direction)))
											(= (get-y obj) (+ (get-y p) (cdr direction)))))
						 *objects*)))

(defmethod throw-item ((p half-player) direction distance damage &key (hit-message "~A gets hit") (hit-message-color :cwhite) to-drop)
  (let ((path (points (make-instance 'line
									 :x1 (get-x p) :y1 (get-y p)
									 :x2 (bound (+ (get-x p) (* (car direction) distance)) (- *screen-width* 1) 1) :y2 (bound (+ (get-y p) (* (cdr direction) distance)) (- *screen-height* 1) 1))))
		hit)
	(dolist (pt path)
	  (move (cdr pt) (car pt))
	  (when (blocks-p (aref *map* (car pt) (cdr pt)))
		(message "Hit a wall" :color hit-message-color)
		(return))
	  (dolist (mon (remove-if-not (lambda (obj) (and (typep obj 'monster)
													 (not (char= (get-char obj) #\%)))) *objects*))
		(when (and (= (get-x mon) (car pt))
				   (= (get-y mon) (cdr pt)))
		  (deal-damage mon damage)
		  (format-message (hit-message (get-name mon)) :color hit-message-color)
		  (setf hit t)))
	  (when hit
		(when to-drop
		  (setf (get-x to-drop) (car pt))
		  (setf (get-y to-drop) (cdr pt))
		  (push to-drop *objects*))
		(return)))
	(when (and (not hit) to-drop)
	  (setf (get-x to-drop) (bound (+ (get-x p) (* (car direction) distance)) (- *screen-width* 1) 1))
	  (setf (get-y to-drop) (bound (+ (get-y p) (* (cdr direction) distance)) (- *screen-height* 1) 1))
	  (push to-drop *objects*))))

(defmethod check-for-mon-attack ((p half-player) dx dy)
  "Attacks space where player would have moved to if possible"
  (let ((new-x (+ (get-x p) dx))
		(new-y (+ (get-y p) dy)))
	(dolist (defender (remove-if-not (lambda (obj) (typep obj 'monster)) *objects*))
	  (when (and (= (get-x defender) new-x)
				 (= (get-y defender) new-y))
		(attack *player* defender))))
  t)

(defclass player (monster)
  ((p1 :initform (make-instance 'half-player) :initarg :p1 :accessor get-p1)
   (p2 :initform (make-instance 'half-player) :initarg :p2 :accessor get-p2)
   (full-name :initform (gen-roman-name) :accessor get-full-name)
   (dlvl :initform 1 :accessor get-dlvl)
   (lvl :initform 1 :accessor get-lvl)
   (exp :initform 0 :accessor get-exp)))

(defmethod initialize-instance :after ((p player) &key)
  (setf (get-name p) (subseq (get-full-name p) 0 (position #\Space (get-full-name p)))))

(init-print player '(p1 p2 name lvl dlvl exp x y hp max-hp attack defense ai death))

(defmethod use-item ((i item) (p player) (hp half-player))
  (funcall (coerce (get-use i) 'function) i p hp))

(defmethod threshold ((p player))
  (defun to-next-level (l)
	(if (<= l 1)
		50
		(* (+ 1 (expt 0.85 l)) (to-next-level (- l 1)))))
  (defun total-exp (l)
	(if (<= l 1)
		(to-next-level l)
		(+ (to-next-level l) (total-exp (- l 1)))))
  (total-exp (get-lvl p)))

(defmethod level-up ((p player))
  (setf (get-lvl p) (+ (get-lvl p) 1))
  (setf (get-atk p) (+ (get-atk p) (* (get-lvl p) (+ 1 (random 3)))))
  (setf (get-def p) (+ (get-def p) (* (get-lvl p) (+ 1 (random 3)))))
  (setf (get-max-hp p) (+ (get-max-hp p) (* (get-lvl p) (+ 1 (random 3)))))
  (setf (get-hp p) (max-hp p))
  (message "Level up!" :color :cblue)
  (stats)
  (refresh))

(defmethod down ((p player))
  ;; Currently dungeon generation is Angband-style, mostly because of space considerations, but also due to laziness
  (dolist (first-stair *objects*) 
	(when (and (string= (get-name first-stair) "downstair")
			   (= (get-x first-stair) (get-x (get-p1 *player*)))
			   (= (get-y first-stair) (get-y (get-p1 *player*))))
	  (dolist (second-stair (delete first-stair *objects*))
		(when (and (string= (get-name second-stair) "downstair")
				   (= (get-x second-stair) (get-x (get-p2 *player*)))
				   (= (get-y second-stair) (get-y (get-p2 *player*))))
		  (setf *objects* nil)
		  (setf (get-dlvl p) (+ (get-dlvl p) 1))
		  (erase)
		  (init-map)
		  (render-all))))))

(defmethod up ((p player))
  (dolist (first-stair *objects*) 
	(when (and (string= (get-name first-stair) "upstair")
			   (= (get-x first-stair) (get-x (get-p1 *player*)))
			   (= (get-y first-stair) (get-y (get-p1 *player*))))
	  (dolist (second-stair (remove first-stair *objects*))
		(when (and (string= (get-name second-stair) "upstair")
				   (= (get-x second-stair) (get-x (get-p2 *player*)))
				   (= (get-y second-stair) (get-y (get-p2 *player*))))
		  (when (or (not (= (get-dlvl p) 1))
					(warn-of-impending-doom))
			(setf *objects* nil)
			(setf (get-dlvl p) (- (get-dlvl p) 1))
			(init-map :stair 'down))
		  (erase)
		  (render-all))))))

(defmethod pickup ((p player))
  (let ((pickup-1 (pickup (get-p1 p)))
		(pickup-2 (pickup (get-p2 p))))
	(when pickup-1
	  (format-message ("~A picked up ~A" (get-name p) (get-name pickup-1)) :color :cgreen))
	(when pickup-2
	  (format-message ("~A picked up ~A" (get-name p) (get-name pickup-2)) :color :cgreen))))

(defun move-player (dx dy)
  "Moves the player if possible"
  (cond ((and (not (blocked-p (+ (get-x (get-p1 *player*)) dx) (+ (get-y (get-p1 *player*)) dy)))
			  (not (blocked-p (+ (get-x (get-p2 *player*)) dx) (+ (get-y (get-p2 *player*)) dy))))
		 (move-mon (get-p1 *player*) dx dy)
		 (move-mon (get-p2 *player*) dx dy)
		 (fov-calculate))
		(t
		 (check-for-mon-attack (get-p1 *player*) dx dy)
		 (check-for-mon-attack (get-p2 *player*) dx dy))))

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
	(cond ((and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
				(not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
		   (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
		   (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
		   (fov-calculate))
		  (t
		   (check-for-mon-attack (get-p1 *player*) (car p1-dir) (cdr p1-dir))
		   (check-for-mon-attack (get-p2 *player*) (car p2-dir) (cdr p2-dir))))))

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
	(cond ((and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
				(not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir))))
				(or (not (= (+ (get-x (get-p1 *player*)) (car p1-dir)) ; For now I just check to avoid moving to the center, but eventually I would like to treat it as two attacks on the center point
							(+ (get-x (get-p2 *player*)) (car p2-dir))))
					(not (= (+ (get-y (get-p1 *player*)) (cdr p1-dir))
							(+ (get-y (get-p2 *player*)) (cdr p2-dir))))))
		   (move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
		   (move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
		   (fov-calculate))
		  (t
		   (check-for-mon-attack (get-p1 *player*) (car p1-dir) (cdr p1-dir))
		   (check-for-mon-attack (get-p2 *player*) (car p2-dir) (cdr p2-dir))))))

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
			 (cond ((and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
						 (not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
					(move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
					(move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
					(fov-calculate))
				   (t
					(check-for-mon-attack (get-p1 *player*) (car p1-dir) (cdr p1-dir))
					(check-for-mon-attack (get-p2 *player*) (car p2-dir) (cdr p2-dir))))))
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
			 (cond ((and (not (blocked-p (+ (get-x (get-p1 *player*)) (car p1-dir)) (+ (get-y (get-p1 *player*)) (cdr p1-dir))))
						 (not (blocked-p (+ (get-x (get-p2 *player*)) (car p2-dir)) (+ (get-y (get-p2 *player*)) (cdr p2-dir)))))
					(move-mon (get-p1 *player*) (car p1-dir) (cdr p1-dir))
					(move-mon (get-p2 *player*) (car p2-dir) (cdr p2-dir))
					(fov-calculate))
				   (t
					(check-for-mon-attack (get-p1 *player*) (car p1-dir) (cdr p1-dir))
					(check-for-mon-attack (get-p2 *player*) (car p2-dir) (cdr p2-dir))))))
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
		(setf (explored-p (aref *map* (car pt) (cdr pt))) t)))))


(defclass message ()
  ((color :initarg :color :accessor get-color)
   (str :initarg :str :accessor get-string)))

(init-print message '(color str))

;;; Globals

(defconstant *screen-width* 50)
(defconstant *screen-height* 20)

(defconstant *class-list* '(tile obj monster player half-player leader item shape rect line message))

(defconstant *max-room-size* 20)
(defconstant *min-room-size* 10)

(defconstant *max-iterations* 3)

(defconstant *inventory-size* 5)

(defconstant *fov-radius* 5)

(defparameter *map* (make-array (list *screen-width* *screen-height*)))

(defparameter *objects* nil)

(defparameter *game-state* nil)

(defparameter *messages* nil)

(defparameter *goals* nil)

(defparameter *player* (make-instance 'player
									  :p1 (make-instance 'half-player :x 10 :y 10 :blocks-sight nil)
									  :p2 (make-instance 'half-player :x 12 :y 10 :blocks-sight nil)
									  :hp (+ 30 (random 21))
									  :atk (+ 5 (random 4))
									  :def (+ 5 (random 4))
									  :ai '(lambda (mon) (input))
									  :death 'player-death))

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

(defun init-map (&key (stair 'up))
  "Initializes the *map* global variable"

  (when (= (get-dlvl *player*) 0)
	(when (not (null *game-state*))
	  (delete-file "phalanx-save.sav"))
	(setf *game-state* nil))
  
  (dostep (x 0 (- *screen-width* 1))
	   (dostep (y 0 (- *screen-height* 1))
			   (setf (aref *map* x y) (make-instance 'tile :blocks t))))

  (defun gen-pillars (&optional (num-pillars (+ 30 (random 40))))
	"Adds a bunch of random pillars for variety"
	(dotimes (i (+ 30 (random 40)))
	  (setf (aref *map*
				  (+ 2 (random (- *screen-width* 5)))
				  (+ 2 (random (- *screen-height* 5))))
			(make-instance 'tile :blocks t))))
  
  (defun big-open-level ()
	"Uses 3 different dungeon generation methods in unison in an attempt to create a big open level with roughly rectangular rooms and a directional component"
	(let* ((shapes (split-dungeon 0 1 1 (- *screen-width* 2) (- *screen-height* 2)))
		   (rects (remove-if-not (lambda (x) (typep x 'rect)) shapes))
		   (lines (remove-if-not (lambda (x) (typep x 'line)) shapes)))

	  (dolist (r rects)
		(draw r))

	  (smooth-dungeon 1)

	  (dolist (l lines)
		(draw l)))
	
	(direct-dungeon :windyness (random 100) :roughness (random 50) :complexity 1 :stair stair)

	;;Generate pillars
	(gen-pillars))

  (defun two-corridor-level ()
	"Generate a level with two side-by-side corridors"
	(let ((displacement (random 4)))
	  (direct-dungeon :windyness (random 10) :roughness (random 10) :stair nil :ystart (+ 1 displacement) :hstart 3)
	  (direct-dungeon :windyness (random 10) :roughness (random 10) :stair nil :ystart (- *screen-height* 6 displacement) :hstart 3)
	  (push (make-instance 'obj :x 1 :y (+ 2 displacement) :name "upstair" :char #\<) *objects*)
	  (push (make-instance 'obj :x 1 :y (- *screen-height* 6 displacement) :name "upstair" :char #\<) *objects*)
	  (when (eq stair 'up)
		(setf (get-x (get-p1 *player*)) 1)
		(setf (get-y (get-p1 *player*)) (+ 2 displacement))
		(setf (get-x (get-p2 *player*)) 1)
		(setf (get-y (get-p2 *player*)) (- *screen-height* 6 displacement)))
	  (let* ((y1 (dotimes (y *screen-height*)
	  			   (when (not (blocks-p (aref *map* (- *screen-width* 2) y)))
	  				 (return y))))
	  		 (y2 (dostep (y (- *screen-height* 2) 1 -1)
	  		 			 (when (and (not (blocks-p (aref *map* (- *screen-width* 2) y)))
	  		 						(> (- y y1) 1)
	  		 						(evenp (- y y1)))
	  		 			   (return y)))))
	  	(when (eq stair 'down)
	  	  (setf (get-x (get-p1 *player*)) (- *screen-width* 2))
	  	  (setf (get-y (get-p1 *player*)) y1)
	  	  (setf (get-x (get-p2 *player*)) (- *screen-width* 2))
	  	  (setf (get-y (get-p2 *player*)) y2))
	  	(push (make-instance 'obj
	  						 :x (- *screen-width* 2) :y y1
	  						 :name "downstair" :char #\>)
	  		  *objects*)
	  	(push (make-instance 'obj :x (- *screen-width* 2) :y y2
	  						 :name "downstair" :char #\>)
	  		  *objects*))))

  (defun one-cavern-level ()
	"Generate a level with one big cavern and no downstairs"
	(dostep (x 1 (- *screen-width* 2))
				 (dostep (y 1 (- *screen-height* 2))
						 (let ((type (random-probability 35)))
						   (setf (aref *map* x y) (make-instance 'tile :blocks type :blocks-sight type)))))
	(smooth-dungeon 7)

	(gen-pillars)

	(let ((x (random (- *screen-width* 2)))
		  (y (random *screen-height*)))
	  (while (or (blocked-p x y)
				 (blocked-p (+ x 2) y))
			 (setf x (random *screen-width*))
			 (setf y (random *screen-height*)))
	  (push (make-instance 'obj :x x :y y :name "upstair" :char #\<) *objects*)
	  (push (make-instance 'obj :x (+ x 2) :y y :name "upstair" :char #\<) *objects*)
	  (setf (get-x (get-p1 *player*)) x)
	  (setf (get-y (get-p1 *player*)) y)
	  (setf (get-x (get-p2 *player*)) (+ x 2))
	  (setf (get-y (get-p2 *player*)) y)))

  (cond ((= (get-dlvl *player*) 10)

		 ;; Init map
		 (one-cavern-level)
		 
		 ;; Place Witch
		 (let ((x (random *screen-width*))
			   (y (random *screen-height*)))
		   (while (or (blocked-p x y)
					  (and (= x (get-x (get-p1 *player*)))
						   (= y (get-y (get-p1 *player*))))
					  (and (= x (get-x (get-p2 *player*)))
						   (= y (get-y (get-p2 *player*)))))
			 (setf x (random *screen-width*))
			 (setf y (random *screen-height*)))
		   (push (make-instance 'leader :x x :y y
								:hp (+ 200 (random 10))
								:atk (+ 30 (random 5))
								:def (+ 100 (random 10))
								:ai 'witch-ai
								:death 'witch-death
								:name "Witch" :char #\W :color :cpurple :blocks t)
				 *objects*)

		   ;; Place Witch vanguard
		   (dolist (mon-pt (butlast (remove-if-not (lambda (pt) (not (blocked-p (car pt) (cdr pt))))
												   (points-within 1 (cons x y)))
									5))
			 (push (make-instance 'monster :x (car mon-pt) :y (cdr mon-pt)
								  :hp (+ 100 (random-deviation 5 :zero-p t))
								  :atk (+ 30 (random-deviation 2 :zero-p t))
								  :def (+ 30 (random-deviation 2 :zero-p t))
								  :name "Specter" :char #\s :color :cyellow :blocks t)
				   *objects*)))

		 ;; Place monsters
		 (dotimes (n (+ 4 (random 4)))
		   (let ((x (random *screen-width*))
				 (y (random *screen-height*)))
			 (while (or (blocked-p x y)
						(and (= x (get-x (get-p1 *player*)))
							 (= y (get-y (get-p1 *player*))))
						(and (= x (get-x (get-p2 *player*)))
							 (= y (get-y (get-p2 *player*)))))
			   (setf x (random *screen-width*))
			   (setf y (random *screen-height*)))
			 (push (make-instance 'monster :x x :y y
								  :hp (+ 100 (random-deviation 5 :zero-p t))
								  :atk (+ 30 (random-deviation 2 :zero-p t))
								  :def (+ 30 (random-deviation 2 :zero-p t))
								  :name "Specter" :char #\s :color :cyellow :blocks t)
				   *objects*))))
		(t
		 ;; Generate map
		 (eval (random-list-weighted (list (cons 80 '(big-open-level))
										   (cons 20 '(two-corridor-level)))))

		 ;; WARNING: Numbers not playtested enough, balance unlikely
  
		 ;; Generate monsters
		 (dotimes (n (+ (bound (get-dlvl *player*) 4) 2 (random 3)))
		   (let ((x (random *screen-width*))
				 (y (random *screen-height*)))
			 (while (or (blocked-p x y)
						(and (= x (get-x (get-p1 *player*)))
							 (= y (get-y (get-p1 *player*))))
						(and (= x (get-x (get-p2 *player*)))
							 (= y (get-y (get-p2 *player*)))))
			   (setf x (random *screen-width*))
			   (setf y (random *screen-height*)))
			 (let ((monster-list (cond ((< (get-dlvl *player*) 4) ; Monster set dlvl 1+
										(list (cons 20 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 20 (random-deviation 3 :zero-p t))
																	   :atk (+ 6 (random-deviation 1 :zero-p t))
																	   :def (+ 3 (random-deviation 1 :zero-p t))
																	   :name "Goth" :char #\g :color :cpurple :blocks t))
											  (cons 50 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 10 (random-deviation 1 :zero-p t))
																	   :atk 2
																	   :def 2
																	   :name "Slave" :char #\s :blocks t))
											  (cons 30 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 15 (random-deviation 4 :zero-p t))
																	   :atk (+ 3 (random-deviation 2 :zero-p t))
																	   :def (+ 6 (random-deviation 2 :zero-p t))
																	   :name "Greek" :char #\g :color :cgreen :blocks t))))
									   ((< (get-dlvl *player*) 7) ; Monster set dlvl 4+
										(list (cons 10 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 50 (random-deviation 10 :zero-p t))
																	   :atk (+ 16 (random-deviation 1))
																	   :def (+ 16 (random-deviation 1))
																	   :name "Gladiator" :char #\g :blocks t)) ; Slave+
											  (cons 20 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 40 (random-deviation 5 :zero-p t))
																	   :atk (+ 14 (random-deviation 3))
																	   :def (+ 10 (random-deviation 3))
																	   :name "Visigoth" :char #\v :color :cpurple :blocks t)) ; Goth+
											  (cons 30 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 35 (random-deviation 7 :zero-p t))
																	   :atk (+ 10 (random-deviation 3))
																	   :def (+ 14 (random-deviation 3))
																	   :name "Persian" :char #\p :color :cgreen :blocks t)) ; Greek+
											  (cons 40 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 30 (random-deviation 3 :zero-p t))
																	   :atk (+ 18 (random-deviation 3))
																	   :def (+ 10 (random-deviation 2))
																	   :name "Vandal" :char #\v :color :cblue :blocks t))))
									   (t ; Monster set dlvl 7+
										(list (cons 15 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 50 (random-deviation 10 :zero-p t))
																	   :atk (+ 16 (random-deviation 1))
																	   :def (+ 16 (random-deviation 1))																
																	   :name "Gladiator" :char #\g :blocks t))
											  (cons 30 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 75 (random-deviation 10 :zero-p t))
																	   :atk (+ 20 (random-deviation 7 :zero-p t))
																	   :def (+ 40 (random-deviation 7 :zero-p t))
																	   :name "Carthaginian" :char #\c :color :cgreen :blocks t)) ; Greek++
											  (cons 30 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 80 (random-deviation 6 :zero-p t))
																	   :atk (+ 40 (random-deviation 4 :zero-p t))
																	   :def (+ 20 (random-deviation 4 :zero-p t))
																	   :name "Hun" :char #\h :color :cred :blocks t))
											  (cons 25 `(make-instance 'monster :x ,x :y ,y
																	   :hp (+ 100 (random-deviation 5 :zero-p t))
																	   :atk (+ 30 (random-deviation 2 :zero-p t))
																	   :def (+ 30 (random-deviation 2 :zero-p t))
																	   :name "Specter" :char #\s :color :cyellow :blocks t)))))))
			   (push (eval (random-list-weighted monster-list))
					 *objects*))))
  
		 ;; Generate items
		 (dotimes (n (+ 3 (get-dlvl *player*) (random 5)))
		   (let ((x (random *screen-width*))
				 (y (random *screen-height*)))
			 (while (blocks-p (aref *map* x y))
			   (setf x (random *screen-width*))
			   (setf y (random *screen-height*)))
			 (let ((item-list (cond ((< (get-dlvl *player*) 4) ; Item set dlvl 1+
									 (list (cons 30 `(make-instance 'item :x ,x :y ,y
																	:name "Healing Draught"
																	:char #\!
																	:color :cpurple
																	:use '(lambda (i p hp)
																		   (let ((healed (- (bound (+ (get-hp p) 5 (random 6)) (max-hp p)) (get-hp p))))
																			 (setf (get-hp p) (+ (get-hp p) healed))
																			 (format-message ("You get healed ~A points" healed) :color :cpurple)))
																	:one-use t))
										   (cons 15 `(make-instance 'item :x ,x :y ,y
																	:name "Caligae (boots)"
																	:char #\[
																	:color :cbrown
																	:def-bonus 3))
										   (cons 25 `(make-instance 'item :x ,x :y ,y
																	:name "Pugio (dagger)"
																	:char #\)
																	:atk-bonus 1
																	:one-use (random-probability 70)
																	:use '(lambda (i p hp)
																		   (power-strike hp (take-in-direction) (+ 5 (random (bound (atk p) 16))) :hit-message "The pugio stabs ~A viciously")
																		   (when (random-probability)
																			 (setf (one-use-p i) t)))
																	:takes-turn t
																	:color :cgray))
										   (cons 30 `(make-instance 'item :x ,x :y ,y
																	:name "Verutum (javelin)"
																	:atk-bonus 1
																	:char #\)
																	:color :cyellow
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 3 (+ 1 (random (bound (atk p) 10))) :hit-message "The verutum pokes ~A" :to-drop i))
																	:one-use t
																	:takes-turn t))))
									((< (get-dlvl *player*) 7) ; Item set dlvl 4+
									 (list (cons 25 `(make-instance 'item :x ,x :y ,y
																	:name "Healing Draught"
																	:char #\!
																	:color :cpurple
																	:use '(lambda (i p hp)
																		   (let ((healed (- (bound (+ (get-hp p) 5 (random 6)) (max-hp p)) (get-hp p))))
																			 (setf (get-hp p) (+ (get-hp p) healed))
																			 (format-message ("You get healed ~A points" healed) :color :cpurple)))
																	:one-use t))
										   (cons 15 `(make-instance 'item :x ,x :y ,y
																	:name "Clipeus (shield)"
																	:char #\[
																	:color :ccyan
																	:def-bonus 9))
										   (cons 10 `(make-instance 'item :x ,x :y ,y
																	:name "Hasta (spear)"
																	:char #\)
																	:color :cbrown
																	:takes-turn t
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 2 (+ 4 (random (bound (atk p) 15))) :hit-message "The hasta thoroughly jabs ~A"))
																	:atk-bonus 4))
										   (cons 25 `(make-instance 'item :x ,x :y ,y
																	:name "Pilum (javelin)"
																	:atk-bonus 5
																	:char #\)
																	:color :cbrown
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 5 (+ 5 (random (bound (atk p) 20))) :hit-message "The pilum severely pierces ~A"))
																	:one-use t
																	:takes-turn t))
										   (cons 15 `(make-instance 'item :x ,x :y ,y
																	:name "Verutum (javelin)"
																	:atk-bonus 1
																	:char #\)
																	:color :cyellow
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 3 (+ 1 (random (bound (atk p) 10))) :hit-message "The verutum pokes ~A" :to-drop i))
																	:one-use t
																	:takes-turn t))
										   (cons 10 `(make-instance 'item :x ,x :y ,y
																	:name "Gladius (sword)"
																	:char #\)
																	:atk-bonus 10
																	:use '(lambda (i p hp)
																		   (power-strike hp (take-in-direction) (+ 10 (random (bound (atk p) 30))) :hit-message "The gladius slashes and gashes ~A")
																		   (when (random-probability 30)
																			 (setf (one-use-p i) t)))
																	:one-use (random-probability 30)
																	:takes-turn t
																	:color :cwhite))))
									(t	; Item set dlvl 7+
									 (list (cons 35 `(make-instance 'item :x ,x :y ,y
																	:name "Arcus (bow)"
																	:char #\)
																	:color :cbrown
																	:takes-turn t
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 7 (+ 3 (random (bound (atk p) 30))) :hit-message "The arrow firmly strikes ~A"))))
										   (cons 25 `(make-instance 'item :x ,x :y ,y
																	:name "Vitality Pendant"
																	:char #\"
																	:color :cblue
																	:max-hp-bonus 10
																	:use '(lambda (i p hp)
																		   (let ((healed (- (bound (+ (get-hp p) 10 (random 11)) (max-hp p)) (get-hp p))))
																			 (setf (get-hp p) (+ (get-hp p) healed))
																			 (format-message ("You get healed ~A points" healed) :color :cpurple)))
																	:one-use t))
										   (cons 10 `(make-instance 'item :x ,x :y ,y
																	:name "Scythe"
																	:char #\)
																	:color :cpurple
																	:takes-turn t
																	:atk-bonus 15
																	:use '(lambda (i p hp)
																		   (throw-item hp (take-in-direction) 2 (+ 8 (random (bound (atk p) 40))) :hit-message "The scythe reaps ~A")
																		   (when (random-probability 20)
																			 (setf (one-use-p i) t)))
																	:one-use (random-probability 40)))
										   (cons 10 `(make-instance 'item :x ,x :y ,y
																	:name "Scutum (shield)"
																	:char #\[
																	:color :cred
																	:takes-turn t
																	:def-bonus 20
																	:use '(lambda (i p hp)
																		   (power-strike hp (take-in-direction) (+ 10 (random (bound (def p) 50))) :hit-message "The scutum bashes ~A")
																		   (when (random-probability 20)
																			 (setf (one-use-p i) t)))
																	:one-use (random-probability 20)))
										   (cons 20 `(make-instance 'item :x ,x :y ,y
																	:name "Gladius (sword)"
																	:char #\)
																	:atk-bonus 10
																	:use '(lambda (i p hp)
																		   (power-strike hp (take-in-direction) (+ 10 (random (bound (atk p) 30))) :hit-message "The gladius slashes and gashes ~A")
																		   (when (random-probability 30)
																			 (setf (one-use-p i) t)))
																	:one-use (random-probability 30)
																	:takes-turn t
																	:color :cwhite)))))))
			   (push (eval (random-list-weighted item-list)) *objects*)))) ; The reason for this trick is so that only the selection is evaluated
		 
		 ;; Generate leader
		 (when (random-probability 25) ; Each level has a 25% chance of containing a leader
		   (let ((x (random *screen-width*))
				 (y (random *screen-height*)))
			 (while (or (blocked-p x y)
						(and (= x (get-x (get-p1 *player*)))
							 (= y (get-y (get-p1 *player*))))
						(and (= x (get-x (get-p2 *player*)))
							 (= y (get-y (get-p2 *player*)))))
			   (setf x (random *screen-width*))
			   (setf y (random *screen-height*)))
			 (push (case (get-dlvl *player*)
					 (1 (make-instance 'leader :x x :y y
									   :hp (+ 15 (random 5))
									   :atk (+ 3 (random 3))
									   :def (+ 1 (random 3))
									   :name "Archimedes" :char #\G :color :cgreen :blocks t)) ; Greek inventor
					 (2 (make-instance 'leader :x x :y y
									   :hp (+ 25 (random 5))
									   :atk (+ 8 (random 3))
									   :def (+ 4 (random 3))
									   :name "Cniva" :char #\G :color :cpurple :blocks t)) ; King of the Goths
					 (3 (make-instance 'leader :x x :y y
									   :hp (+ 30 (random 5))
									   :atk (+ 30 (random 5))
									   :def (+ 1 (random 2)) ; Pyrrhus has insane attack, and insanely low defense, making a Pyrrhic victory all too likely
									   :name "Pyrrhus" :char #\P :color :cred :blocks t)) ; of Epirus; "Pyrrhic victory"
					 (4 (make-instance 'leader :x x :y y
									   :hp (+ 45 (random 5))
									   :atk (+ 18 (random 3))
									   :def (+ 14 (random 3))
									   :name "Alaric" :char #\V :color :cpurple :blocks t)) ; King of the Visigoths
					 (5 (make-instance 'leader :x x :y y
									   :hp (+ 35 (random 3))
									   :atk (+ 26 (random 3))
									   :def (+ 12 (random 2))
									   :name "Genseric" :char #\V :color :cblue :blocks t)) ; King of the Vandals
					 (6 (make-instance 'leader :x x :y y
									   :hp (+ 40 (random 7))
									   :atk (+ 18 (random 5))
									   :def (+ 22 (random 5))
									   :name "Shapur" :char #\P :color :cgreen :blocks t)) ; King of the Sassanid Persians
					 (7 (make-instance 'leader :x x :y y
									   :hp (+ 60 (random 10))
									   :atk (+ 36 (random 5))
									   :def (+ 24 (random 5))								
									   :name "Spartacus" :char #\G :blocks t)) ; Gladiator
					 (8 (make-instance 'leader :x x :y y
									   :hp (+ 100 (random 6))
									   :atk (+ 50 (random 4))
									   :def (+ 30 (random 4))								
									   :name "Attila" :char #\H :color :cred :blocks t)) ; the Hun
					 (9 (make-instance 'leader :x x :y y
									   :hp (+ 110 (random 11))
									   :atk (+ 40 (random 8))
									   :def (+ 60 (random 8))
									   :name "Hannibal" :char #\C :color :cgreen :blocks t))) ; of Carthage
				   *objects*))))))

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
(defun direct-dungeon (&key (length (- *screen-width* 4)) (roughness 50) (windyness 50) (complexity 1) (stair 'up) (ystart (+ 1 (random (- *screen-height* 10)))) (hstart (+ 3 (random 3))))
  "Generates a cavelike dungeon left to right"
  (let ((x 1)
		(start 1)
		(y ystart)
		(height hstart))
	(draw (make-instance 'line :x1 x :y1 y :x2 x :y2 (+ y height)))
	(when (= complexity 1)
	  (let ((stair-loc (random (- height 2))))
		(when (eq stair 'up)
		  (setf (get-x (get-p1 *player*)) x)
		  (setf (get-y (get-p1 *player*)) (+ y stair-loc))
		  (setf (get-x (get-p2 *player*)) x)
		  (setf (get-y (get-p2 *player*)) (+ y stair-loc 2)))
		(when (or (eq stair 'up)
				  (eq stair 'down))
		  (setf *objects* (cons (make-instance 'obj :name "upstair" :x x :y (+ y stair-loc 2) :char #\<)
								(cons (make-instance 'obj :name "upstair" :x x :y (+ y stair-loc) :char #\<) *objects*))))))
	(while (<= (- x start) length)
	  (setf x (+ x 1))
	  (when (<= (random 100) roughness)
		(setf height (+ height (random-deviation 2)))
		(when (< height 3)
		  (setf height 3))
		(when (> height 6)
		  (setf height 6)))
	  (when (<= (random 100) windyness)
		(setf y (+ y  (if (= height 3)
						  (random-deviation 1) ;Prevent a situation where the player can't move
						  (random-deviation 2))))
			
		(when (< y 1)
		  (setf y 1)))
	  (when (> (+ y height) (- *screen-height* 2))
		(setf y (- *screen-height* 2 height)))
	  (draw (make-instance 'line :x1 x :y1 y :x2 x :y2 (+ y height)))
	  )
	(if (> complexity 1)
		(direct-dungeon :length length :windyness windyness :roughness roughness :complexity (- complexity 1))
		(let ((stair-loc (random (- height 2))))
		  (when (eq stair 'down)
			(setf (get-x (get-p1 *player*)) x)
			(setf (get-y (get-p1 *player*)) (+ y stair-loc))
			(setf (get-x (get-p2 *player*)) x)
			(setf (get-y (get-p2 *player*)) (+ y stair-loc 2)))
		  (when (or (eq stair 'up)
					(eq stair 'down))
			(setf *objects* (cons (make-instance 'obj :name "downstair" :x x :y (+ y stair-loc 2) :char #\>)
								  (cons (make-instance 'obj :name "downstair" :x x :y (+ y stair-loc) :char #\>) *objects*))))))))

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
  (dolist (corpse (remove-if-not (lambda (obj) (char= (get-char obj) #\%))
								 *objects*))
	(attron (slot-value corpse 'color))
	(mvaddch (get-y corpse) (get-x corpse) (slot-value corpse 'char))
	(Attroff (slot-value corpse 'color)))
  (dolist (obj (remove-if-not (lambda (potential-corpse) (not (char= (get-char potential-corpse) #\%)))
							  *objects*))
	(when (or (lit (aref *map* (get-x obj) (get-y obj)))
			  (and (or (string= (get-name obj) "downstair") (string= (get-name obj) "upstair"))
				   (explored-p (aref *map* (get-x obj) (get-y obj)))))
	  (attron (slot-value obj 'color))
	  (mvaddch (get-y obj) (get-x obj) (slot-value obj 'char))
	  (Attroff (slot-value obj 'color))))
  (dolist (mon (remove-if-not (lambda (obj) (and (typep obj 'monster) (not (eq (get-char obj) #\%)))) *objects*))
	(when (lit (aref *map* (get-x mon) (get-y mon)))
		 (progn (attron (slot-value mon 'color))
				(mvaddch (get-y mon) (get-x mon) (slot-value mon 'char))
				(Attroff (slot-value mon 'color))))))

(defun render-player ()
  (let* ((x1 (get-x (get-p1 *player*))) (y1 (get-y (get-p1 *player*)))
		 (x2 (get-x (get-p2 *player*))) (y2 (get-y (get-p2 *player*)))
		 (cen (center (make-instance 'line :x1 x1 :y1 y1 :x2 x2 :y2 y2))))
	(attron :cred)
	(mvaddch y1 x1 #\@)
	(attroff :cred)
	(attron :cblue)
	(mvaddch y2 x2 #\@)
	(attroff :cblue)
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

  ;; NOTE: The inefficiency of this method caused me significant consternation when working with leaders
  (defun dmap (source &optional (default 0) destination)
  	(let ((path-lengths (make-array (list *screen-width* *screen-height*)))
		  (known (list source))
		  selectable)
  	  (setf (aref path-lengths (car source) (cdr source)) default)
  	  (dostep (x (- (car source) 1) (+ (car source) 1))
  			  (dostep (y (- (cdr source) 1) (+ (cdr source) 1))
  					  (when (and (not (member (cons x y) known :test #'equal))
  								 (not (blocked-p x y)))
  						(setf (aref path-lengths x y) (+ default 1))
  						(push (cons x y) selectable))))
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
  					  (push (cons x y) selectable))))
  		  (push selected known)
		  (when (equal selected destination)
			(return))))
  	  path-lengths))

  (let ((map (make-array (list *screen-width* *screen-height*))))
  	(dolist (src sources)
  	  (let ((temp (cond ((and (consp (car src)) (consp (cdr src)))
						 (dmap (car src) 0 (cdr src)))
						((consp (cdr src))
						 (dmap (cdr src) (car src))) ;Each source is either a cons pair consisting of the default value for that source followed by a point or just a point or two points, a source and destination
						(t
						 (dmap src)))))
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
													(points-within 1 start))))
		 (min-points 
		  (if (null allowed-point-vals)
			  nil
			  (remove-if-not (lambda (x) (eq (aref dmap (car x) (cdr x))
											 (reduce #'min allowed-point-vals)))
					 (points-within 1 start)))))
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
  (mvprintw *screen-height* 0 (format nil "DL: ~A CL: ~A" (get-dlvl *player*) (get-lvl *player*)))
  (bar 14 *screen-height* "H" (get-hp *player*) (max-hp *player*) :color :cgreen)
  (mvprintw *screen-height* 37 (format nil "A: ~A D: ~A" (atk *player*) (def *player*))))

(defun bar (x y label val max &key (color :cwhite))
  (mvprintw y x (concatenate 'string
							 label
							 ": [     ] " (format nil "(~A / ~A)" val max)))
  (attron color)
  (mvprintw y (+ x (length label) 3) (if (> val 0)
										 (concatenate 'string
													  (make-string (floor (* 5 (/ val max))) :initial-element #\=)
													  (if (= (floor (* 5 (/ val max))) (ceiling (* 5 (/ val max))))
														  ""
														  ":"))
										 ""))
  (attroff color))

;;; Menus and Input

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
		((eq in #\i)
		 (when (not (eq (inventory) 'end-turn))
		   (input)))
		((eq in #\,)
		 (pickup *player*))
		((eq in #\;)
		 (look)
		 (input))
		((eq in #\>)
		 (down *player*)
		 (input))
		((eq in #\<)
		 (up *player*)
		 (when *game-state*
		   (input)))
		((or (eq in #\Escape) (eq in #\m))
		 (when (not (eq (main-menu) 'quit))
		   (input)))))

(defun take-in-direction ()
  (mvprintw (+ 1 *screen-height*) 0 (make-string *screen-width* :initial-element #\Space))
  (mvprintw (+ 1 *screen-height*) 0 "What direction?")
  (let ((in (take-in-char)))
	(cond ((eq in #\j)
		   (cons 0 1))
		  ((eq in #\k)
		   (cons 0 -1))
		  ((eq in #\h)
		   (cons -1 0))
		  ((eq in #\l)
		   (cons 1 0))
		  ((eq in #\y)
		   (cons -1 -1))
		  ((eq in #\u)
		   (cons 1 -1))
		  ((eq in #\b)
		   (cons -1 1))
		  ((eq in #\n)
		   (cons 1 1))
		  ;; ((eq in #\s)
		  ;;  (separate-player))
		  ;; ((eq in #\d)
		  ;;  (rotate-player 'counter))
		  ;; ((eq in #\f)
		  ;;  (rotate-player 'clockwise))
		  ;; ((eq in #\g)
		  ;;  (gather-player))
		  )))

(defun look ()
  "Look functions similarly to a menu, in that it blocks input"
  (let* ((i 0)
		 (cursor (center (make-instance 'line
										:x1 (get-x (get-p1 *player*)) :y1 (get-y (get-p1 *player*))
										:x2 (get-x (get-p2 *player*)) :y2 (get-y (get-p2 *player*)))))
		 (in nil))

	(defun process-input (in)
	  (cond ((eq in #\j)
			 (setf (cdr cursor) (mod (+ (cdr cursor) 1) *screen-height*)))
			((eq in #\k)
			 (setf (cdr cursor) (mod (- (cdr cursor) 1) *screen-height*)))
			((eq in #\h)
			 (setf (car cursor) (mod (- (car cursor) 1) *screen-width*)))
			((eq in #\l)
			 (setf (car cursor) (mod (+ (car cursor) 1) *screen-width*)))
			((eq in #\y)
			 (process-input #\h)
			 (process-input #\k))
			((eq in #\u)
			 (process-input #\l)
			 (process-input #\k))
			((eq in #\b)
			 (process-input #\h)
			 (process-input #\j))
			((eq in #\n)
			 (process-input #\l)
			 (process-input #\j))))
	
	(while (not (eq in #\q))
	  (mvprintw (+ *screen-height* 1) 0 (make-string *screen-width* :initial-element #\Space)) ; Clear line
	  (mvprintw (+ *screen-height* 1) 0 (format nil "~A" (mapcar (lambda (obj) (concatenate 'string
																							"|"
																							(make-string 1
																										 :initial-element (get-char obj))
																							"|: "
																							(get-name obj)))
																 ;; TODO: include player and walls
																 (remove-if-not (lambda (obj) (and (= (get-x obj) (car cursor))
																								   (= (get-y obj) (cdr cursor))
																								   (or (lit (aref *map* (car cursor) (cdr cursor)))
																									   (and (or (string= (get-name obj) "downstair")
																												(string= (get-name obj) "upstair"))
																											(explored-p (aref *map* (car cursor) (cdr cursor)))))))
																				*objects*))))
	  (mvprintw (+ *screen-height* 2) 0 (make-string *screen-width* :initial-element #\Space))
	  (mvprintw (+ *screen-height* 2) 0 "Type \"q\" to exit look mode")
	  (move (cdr cursor) (car cursor))
	  (refresh)
	  (setf in (take-in-char))
	  (process-input in))
	(erase)
	(render-all)))

(defun inventory (&optional (filter #'identity))
  (erase)
  (let* ((end-turn nil)
		 (i 0)
		 (cursor (cons 0 0))
		 (in nil)
		 (selectable (cons (remove-if-not filter (get-inv (get-p1 *player*)))
						   (remove-if-not filter (get-inv (get-p2 *player*)))))
		 (num-selections (cons (if (= (length (car selectable)) 0)
								   1 ; Pretend that we have one selection if we have none
								   (length (car selectable)))
							   (if (= (length (cdr selectable)) 0)
								   1
								   (length (cdr selectable))))))
	(attron :cred)
	(mvprintw 0 0 "red")
	(attroff :cred)
	(attron :cblue)
	(mvprintw 0 20 "blue")
	(attroff :cblue)
	(mvprintw (+ 1 *screen-height*) 0 "t - transfer | d - drop | a - apply | q - quit")
	(dolist (obj (car selectable))
	  (mvprintw (+ 1 i) 0 (get-name obj))
	  (setf i (+ 1 i)))
	(setf i 0)
	(dolist (obj (cdr selectable))
	  (mvprintw (+ 1 i) 20 (get-name obj))
	  (setf i (+ 1 i)))
	(dotimes (i (+ 1 *inventory-size*))
	  (mvprintw i 19 "|"))
	(stats)

	(defun refresh-inventory ()
	  "Refresh the inventory screen"
	  (setf selectable (cons (remove-if-not filter (get-inv (get-p1 *player*)))
							 (remove-if-not filter (get-inv (get-p2 *player*)))))
	  (setf num-selections (cons (if (= (length (car selectable)) 0)
									 1 ; Pretend that we have one selection if we have none
									 (length (car selectable)))
								 (if (= (length (cdr selectable)) 0)
									 1
									 (length (cdr selectable)))))
	  (setf cursor (cons 0 0))
	  (setf i 0)
	  
	  (erase)
	  (attron :cred)
	  (mvprintw 0 0 "red")
	  (attroff :cred)
	  (attron :cblue)
	  (mvprintw 0 20 "blue")
	  (attroff :cblue)
	  (mvprintw (+ 1 *screen-height*) 0 "t - transfer | d - drop | a - apply | q - quit")
	  (dolist (obj (car selectable))
		(mvprintw (+ 1 i) 0 (get-name obj))
		(setf i (+ 1 i)))
	  (setf i 0)
	  (dolist (obj (cdr selectable))
		(mvprintw (+ 1 i) 20 (get-name obj))
		(setf i (+ 1 i)))
	  (dotimes (i (+ 1 *inventory-size*))
		(mvprintw i 19 "|"))
	  (stats))
	
	(while (not (eq in #\q))
	  (cond ((eq in #\j)
			 (setf (car cursor) (mod (+ (car cursor) 1) (if (= (cdr cursor) 0)
															(car num-selections)
															(cdr num-selections)))))
			((eq in #\k)
			 (setf (car cursor) (mod (- (car cursor) 1) (if (= (cdr cursor) 0)
															(car num-selections)
															(cdr num-selections)))))
			((eq in #\h)
			 (setf (cdr cursor) (mod (- (cdr cursor) 1) 2))
			 (setf (car cursor) (bound (car cursor) (if (= (cdr cursor) 0)
														(- (car num-selections) 1)
														(- (cdr num-selections) 1)))))
			((eq in #\l)
			 (setf (cdr cursor) (mod (+ (cdr cursor) 1) 2))
			 (setf (car cursor) (bound (car cursor) (if (= (cdr cursor) 0)
														(- (car num-selections) 1)
														(- (cdr num-selections) 1)))))
			((eq in #\a)
			 (let ((it (nth (car cursor) (get-inv (if (= (cdr cursor) 0)
													 (get-p1 *player*)
													 (get-p2 *player*))))))
			   (when (not (null it))
				   (erase)
				   (render-all)
				   (refresh)
				   (use-item it
							 *player*
							 (if (= (cdr cursor) 0)
								 (get-p1 *player*)
								 (get-p2 *player*)))
				   (if (one-use-p it)
					   (setf (get-inv (if (= (cdr cursor) 0)
										  (get-p1 *player*)
										  (get-p2 *player*)))
							 (delete it
									 (get-inv (if (= (cdr cursor) 0)
												  (get-p1 *player*)
												  (get-p2 *player*)))
									 :count 1)))
				   (refresh-inventory)
				   (when (takes-turn-p it)
					 (setf end-turn t)
					 (return 'end-turn)))))
			((eq in #\d)
			 (when (not (null (nth (car cursor) (get-inv (if (= (cdr cursor) 0)
															 (get-p1 *player*)
															 (get-p2 *player*))))))
			   (let ((half-player (if (= (cdr cursor) 0)
									  (get-p1 *player*)
									  (get-p2 *player*))))
				 (drop-item (nth (car cursor) (get-inv half-player)) half-player))
			   
			   (refresh-inventory)))
			((eq in #\t)
			 (when (and (not (null (nth (car cursor) (get-inv (if (= (cdr cursor) 0)
																  (get-p1 *player*)
																  (get-p2 *player*))))))
						(< (length (get-inv (if (= (cdr cursor) 0)
												(get-p2 *player*)
												(get-p1 *player*))))
						   *inventory-size*))
			   (let ((it (nth (car cursor)
							  (get-inv (if (= (cdr cursor) 0)
										   (get-p1 *player*)
										   (get-p2 *player*))))))

				 ;; Add to opposite inventory
				 (push it (get-inv (if (= (cdr cursor) 0)
									   (get-p2 *player*)
									   (get-p1 *player*))))

				 ;; Delete from corresponding inventory
				 (setf (get-inv (if (= (cdr cursor) 0)
									(get-p1 *player*)
									(get-p2 *player*)))
					    (delete it
								(get-inv (if (= (cdr cursor) 0)
											 (get-p1 *player*)
											 (get-p2 *player*)))
								:count 1)))
			   (refresh-inventory))))
	  (move (+ 1 (car cursor)) (* 20 (cdr cursor)))
	  (refresh)
	  (setf in (curses-code-char (wgetch *stdscr*))))
	(erase)
	(render-all)
	(when end-turn
	  'end-turn)))

(defun warn-of-impending-doom ()
  (erase)
  (mvprintw 0 0 "WARNING: This will delete your save. Continue (Y/N)?")
  (let ((in (take-in-char)))
	(while (not (or (eq in #\y) (eq in #\Y) (eq in #\n) (eq in #\N)))
	  (setf in (take-in-char)))
	(or (eq in #\y) (eq in #\Y))))

(defun help (&optional (type 'basic))
  (erase)
  (cond ((eq type 'basic)
		 (mvprintw 0 0 "Controls")
		 (mvprintw 1 0 (make-string *screen-width* :initial-element #\-))
		 (mvprintw 2 0 "h - left")
		 (mvprintw 3 0 "j - down")
		 (mvprintw 4 0 "k - up")
		 (mvprintw 5 0 "l - right")
		 (mvprintw 6 0 "y - diagonal up left")
		 (mvprintw 7 0 "u - diagonal up right")
		 (mvprintw 8 0 "b - diagonal down left")
		 (mvprintw 9 0 "n - diagonal down right")
		 (mvprintw 11 0 "s - separate")
		 (mvprintw 12 0 "d - rotate counterclockwise")
		 (mvprintw 13 0 "f - rotate clockwise")
		 (mvprintw 14 0 "g - gather")
		 (mvprintw 16 0 "i - inventory")
		 (mvprintw 17 0 ", - pickup")
		 (mvprintw 18 0 "; - look")
		 (mvprintw 19 0 "m - menu")))
  (mvprintw *screen-height* 0 "Type \"q\" to exit help")
  (while (not (eq (take-in-char) #\q))
	nil))

(defun story (&optional (type 'basic))
  (erase)
  (case type
	(basic (mvprintw 0 0 (format nil "You are ~A," (get-full-name *player*)))
		   (mvprintw 1 30 "a Roman legionary")
		   (mvprintw 3 0 "You were looking forward to some well-earned")
		   (mvprintw 4 0 "downtime during an absentminded raid on some")
		   (mvprintw 5 0 "noname barbarian village outside of Thracia when")
		   (mvprintw 6 0 "it happened. After you drove your pilum into this")
		   (mvprintw 7 0 "guy's skull some witch came out screaming curses")
		   (mvprintw 8 0 "at you. Next thing you know you're in two bodies.")
		   (mvprintw 9 0 "Naturally you were beside yourself, but you")
		   (mvprintw 10 0 "pulled yourself together long enough to chase the")
		   (mvprintw 11 0 "fleeing witch to a spectral palace. You suspect")
		   (mvprintw 12 0 "that killing her will lift the curse.")
		   (mvprintw 14 30 "Time to get to work."))
	(continue (mvprintw 0 0 (format nil "Welcome back ~A" (get-full-name *player*)))
			  (mvprintw 1 0 "Is she dead yet?")
			  (mvprintw 2 0 (if (or (< (get-dlvl *player*) 10)
									(< (length (remove-if-not (lambda (obj)
																(and (string= (get-name obj) "Witch")
																	 (char= (get-char obj) #\%)))
															  *objects*))
									   1))
								"No? Well get on with it then"
								"Yes? Then what are you still doing here?")))
	(win (mvprintw 0 0 "You have vanquished the mighty witch.")
		 (mvprintw 1 0 "The palace dissolves as if it had never existed,")
		 (mvprintw 2 0 "and you find yourself in a ghastly graveyard.")
		 (mvprintw 4 0 "Whole once more,")
		 (mvprintw 5 0 "you take your first steps...")
		 (mvprintw 6 25 "... toward vacation.")))
  (getch))

(defun main-menu ()
  (erase)
  (let ((cursor-pos 0) (max-cursor 6) (options nil))

	(defun refresh-menu ()
	  (erase)
	  (attron :cred)
	  (mvprintw 4 5 "PHALANX")
	  (attroff :cred)
				   
	  (attron :cyellow)
	  (mvprintw 7 5 "By Keith Bateman")
	  (mvprintw 8 5 "Copyright (C) 2018")
	  (attroff :cyellow)
	  (mvprintw 10 5 "n) New Game")
	  (mvprintw 11 5 "s) Save Game")
	  (mvprintw 12 5 "c) Continue Saved Game")
	  (mvprintw 13 5 "r) Resume Game")
	  (mvprintw 14 5 "q) Quit")
	  (mvprintw 15 5 "?) Help")
	  t)

	(refresh-menu)
	
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
									:hp (+ 30 (random 21))
									:atk (+ 5 (random 4))
									:def (+ 5 (random 4))
									:ai '(lambda (mon) (input))
									:death 'player-death))	  
	  (setf *messages* nil)
	  (setf *map* (make-array (list *screen-width* *screen-height*)))
	  (setf *game-state* t)
	  (story)
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
		(write (cons '*goals* *goals*) :stream save-file)
		(write (cons '*messages* *messages*) :stream save-file)))
	
	(defun continue-game ()
	  "A hairy hack for loading a save game. It's a hairy hack because of CLOS, and I put up with it because of CLOS"
	  (defun load-object (spec)
		"Create a new instance of an object with the given print representation"
		(if (not (member (car spec) *class-list*))
			(car spec)
			(let ((new-obj (make-instance (car spec))))
			  (dolist (slot-pair (cdr spec))
				(cond ((eq (cdr slot-pair) 'unbound) ; This is unbound
					   (slot-makunbound new-obj (car slot-pair)))
					  ((and (listp (cdr slot-pair)) (not (null (cdr slot-pair)))
							(member (cadr slot-pair) *class-list*)) ; This is a class we initialized a printer for
					   (setf (slot-value new-obj (car slot-pair)) (load-object (cdr slot-pair))))
					  ((and (listp (cdr slot-pair)) (not (null (cdr slot-pair)))
							(not (eq (cadr slot-pair)
									 'lambda))) ; This is a list of objects like the inventory
					   (setf (slot-value new-obj (car slot-pair)) nil)
					   (dolist (new-spec (cdr slot-pair))
						 (setf (slot-value new-obj (car slot-pair))
							   (cons (load-object new-spec) (slot-value new-obj (car slot-pair))))))
					  ((or (atom (cdr slot-pair)) (and (listp (cdr slot-pair)) (eq (cadr slot-pair) 'lambda))) ; This is an atom or a lambda function
					   (setf (slot-value new-obj (car slot-pair))
							 (cdr slot-pair)))))
			  new-obj)))
	  
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
		(story 'continue)
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
			 (help)
			 (refresh-menu)
			 t)
			((or (eq in #\n) (eq in #\N))
			 (new-game)
			 nil)
			((or (eq in #\s) (eq in #\S))
			 (when (not (null *game-state*))
			   (save-game)
			   (setf *game-state* nil)
			   'quit))
			((or (eq in #\r) (eq in #\R))
			 (not (resume-game)))
			((or (eq in #\c) (eq in #\C))
			 (continue-game)
			 nil)
			((or (eq in #\q) (eq in #\Q))
			 (if (warn-of-impending-doom)
				 (progn (when (not (null *game-state*))
						  (delete-file "phalanx-save.sav"))
						(setf *game-state* nil)
						'quit)
				 (refresh-menu)))
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
				(dolist (mon (remove-if-not (lambda (obj) (typep obj 'monster)) *objects*))
				  (take-turn mon))))
	(render-all))
  (close-console)
  #+clisp (ext:quit))

#+clisp
(defun make-exec ()
  (ext:saveinitmem "phalanx" :quiet t :init-function #'main :executable t :norc t))
