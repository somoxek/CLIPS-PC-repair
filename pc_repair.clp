
;;;======================================================
;;;   PC Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a PX.
;;;
;;;	Authors:
;;;	Hackl Dominik
;;;	Kavan Harrys
;;;	Scheibelhofer Thomas
;;;	Schmid-Kietreiber Matthias
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (print ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (print ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-pc-state ""
   (not (pc-starts ?))
   (not (repair ?))
   =>
   (assert pc-starts (yes-or-no-p "Does the PC start (yes/no)? "))))


(defrule determine-powerconnection-state ""
   (engine-starts no)
   (not (repair ?))   
   =>
   (assert (powerconnection (yes-or-no-p "Is the power connected (yes/no)? "))))



;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-pc-state-conclusions ""
   (runs-normally yes)
   (not (repair ?))
   =>
   (assert (repair "Repair done.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (println crlf "The PC Diagnosis Expert System" crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (println crlf "Suggested Repair:" crlf)
  (println " " ?item crlf))