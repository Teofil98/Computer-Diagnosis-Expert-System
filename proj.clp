;;;======================================================
;;;   Computer Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a computer.
;;;
;;;     Bodea Teofil - Project 
;;;	Group 30432
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

  
;;**************
;;* DEFGLOBALS *
;;**************

(defglobal 
    ?*scan-confidence* = 0.9
    ?*fragmentation-confidence* = 0.8
    ?*overheat-no-confidence* = 0.9
    ?*shuts-down-confidence* = 0.8
    ?*kbd-drivers-up-to-date* = 0.9)
  
;;****************
;;* DEFFUNCTIONS *
;;****************


(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))
       
(deffunction calculate-confidence-charged (?ans)
    (if (eq ?ans alongtimeago ) then 0.9
        else (if (eq ?ans quiterecently) then 0.65
        else (if (eq ?ans recently) then  0.8
        else 0.9))))

(deffunction calculate-confidence-bsod (?ans)
    (if (eq ?ans always) then 0.95 else 0.75))

(deffunction calculate-confidence-slow (?ans)
    (if (eq ?ans asusual) then 1
    else (if (eq ?ans veryslow) then 0.9
    else (if (eq ?ans slow) then 0.8
    else 0.7))))
  
  
;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-device ""
    (not (device ?))
    (not (repair ?))
    =>
    (assert (device (ask-question "What device are you using? (computer/laptop) " computer laptop))))

(defrule determine-computer-state ""
   (device computer|laptop)
   ;(not (computer-starts ?))
   ;(not (repair ?))
   =>
   (assert (computer-starts (yes-or-no "Does the computer start (yes/no)? "))))
   
(defrule determine-runs-normally ""
   (device computer|laptop)
   (computer-starts yes)
   ;(not (repair ?))
   =>
   (assert (runs-normally (yes-or-no "Does the computer run normally (yes/no)? "))))
   
(defrule determine-plugged-in ""
    (device computer)
   (computer-starts no)
   ;(not (computer-plugged-in ?))
   ;(not (repair ?))
   =>
   (assert (computer-plugged-in (yes-or-no "Is the computer plugged in (yes/no)? "))))
   
(defrule determine-laptop-charged
    (device laptop)
    (computer-starts no)
    ;(not (laptop-charged ?))
    ;(not (repair ?))
    =>
    (bind ?ans (ask-question "How recent has the laptop been charged (aLongTimeAgo/quiteRecently/Recently/JustNow) ?" alongtimeago quiterecently recently justnow))
    (assert (laptop-charged ?ans) CF (calculate-confidence-charged ?ans ))) 
    
    
(defrule get-bsod
    (runs-normally no)
    ;(not (bsod ?))
    ;(not (repair ?))
    =>
    (assert (bsod (yes-or-no "Do you get blue screen of death (yes/no)? "))))
    
    
(defrule determine-freq-bsod
    (bsod yes)
    ;(not (repair ?))
    =>
    (bind ?ans (ask-question "Do you get blue screen of death all the time right after starting the computer or intermitent after a while (always/intermitent)? " always intermitent))
    (assert (freq-bsod ?ans) CF (calculate-confidence-bsod ?ans)))
    
    
(defrule computer-slow
    (bsod no)
    ;(not (repair ?))
    =>
    (bind ?ans (ask-question "How fast is your computer running (verySlow/slow/aBitSlow/asUsual)? " veryslow slow abitslow asusual))
    (assert (slow ?ans) CF (calculate-confidence-slow ?ans)))
    
(defrule computer-virus
    (slow veryslow|slow|abitslow)
    ;(not (repair ?))
    =>
    (assert (virus-scan (yes-or-no "Did you run a virus scan (yes/no)? ")) CF ?*scan-confidence*))
   
(defrule check-fragmentation
    (virus-scan yes)
    ;(not (repair ?))
    =>
    (assert (fragmentation (yes-or-no "Is your hard disk fragmented (check by going to mycomputer -> right click on drive -> properties -> tools -> optimize) (yes/no)? ")) CF ?*fragmentation-confidence*)) 
    
(defrule is-low-res
    (slow asusual)
    ;(not (repair ?))
    =>
    (assert (low-res (yes-or-no "Does your desktop have a low resolution, without the possibility to change it (yes/no)? "))))
    
(defrule computer-freezes
    (low-res no)
    ;(not (repair ?))
    =>
    (assert (freezes (yes-or-no "Does your computer experience freezes (yes/no)? "))))
    
(defrule check-ram
    (freezes yes)
    ;(not (repair ?))
    =>
    (assert (ram-ok (yes-or-no "Is your RAM memory functioning without problems (to check this, run the Windows Memory Diagnostic Tool) (yes/no)? "))))
    

(defrule laptop-overheating
    (device laptop)
    (freezes no)
    ;(not (repair ?))
    =>
    (assert (overheating (yes-or-no "Does your laptop overheat and shut down (yes/no)? "))))

    
(defrule laptop-shuts-down
    (overheating no)
    ;(not (repair ?))
    =>
    (assert (shut-down-laptop (yes-or-no "Does your laptop shut down after a sort period of usage (yes/no)? "))))

(defrule recently-charged
    (declare (CF ?*shuts-down-confidence*))
    (shut-down-laptop yes)
    ;(not (repair ?))
    =>
    (assert (charged-recently (yes-or-no "Has your laptop been charged recently (yes/no)? "))))
    
(defrule keyboard-not-working
    (shut-down-laptop no)
    ;(not (repair ?))
    =>
    (assert (keyboard-problems (yes-or-no "Do you have problems with your keyboard (yes/no)? "))))
    
(defrule determine-key-problems
    (keyboard-problems yes)
    ;(not (repair ?))
    =>
    (assert (key-problem (ask-question "Is your whole keyboard not working or just some keys (all/some)? " all some))))
    
(defrule keyboard-drivers
    (declare (CF ?*kbd-drivers-up-to-date*))
    (key-problem all)
    ;(not (repair ?))
    =>
    (assert (keyboard-drivers-up-to-date (yes-or-no "Are you keyboard drives up to date (yes/no)? "))))
    

    
    
;;;*************
;;;* XAI RULES *
;;;*************

(defrule print-explain
    (declare (salience 9))
    (repair ?)
    =>
      (printout t crlf )
      (printout t "Explanaiton: ")
      (printout t crlf crlf))

(defrule xai-no-repair
    (declare (salience 8))
    (repair "No repair needed")
    ?f1 <- (computer-starts yes)
    ?f2 <- (runs-normally yes)
    =>
     (printout t "Confidence computer starts:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer runs normally:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))
     
(defrule xai-plug-in
    (declare (salience 8))
    (repair "Plug in the computer")
    ?f1 <- (computer-starts no)
    ?f2 <- (computer-plugged-in no)
    =>
     (printout t "Confidence computer doesn't start:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer is not plugged in:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))
    
(defrule xai-broken-psu
    (declare (salience 8))
    (repair "PSU needs replacement")
    ?f1 <- (computer-starts no)
    ?f2 <- (computer-plugged-in yes)
    =>
     (printout t "Confidence computer doesn't start:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer is plugged in:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))


(defrule xai-laptop-needs-charging
    (declare (salience 8))
    (repair "Charge your laptop")
    ?f1 <- (computer-starts no)
    ?f2 <- (laptop-charged alongtimeago)
    =>
     (printout t "Confidence laptop doesn't start:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence laptop needs charging (based on how recently it was last charged):  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))

(defrule xai-dead-battery
    (declare (salience 8))
    (repair "Your battery does not work anymore and needs to be replaced")
    ?f1 <- (computer-starts no)
    ?f2 <- (laptop-charged quiterecently|recently|justnow)
    =>
     (printout t "Confidence laptop doesn't start:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence battery is damaged (based on how recently it was last charged):  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))

     
(defrule xai-hardware-problem
    (declare (salience 8))
    (repair "Your computer suffers from hardware problems (one of your components has most likely died). Visit a repair shop. ")
    ?f1 <- (bsod yes)
    ?f2 <- (freq-bsod always)
    =>
    (printout t "Confidence user gets blue screen of death:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence blue screen of death is cause by a hardware problem based on how often the user experiences bsod:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))


   
(defrule xai-hdd-failing
    (declare (salience 8))
    (repair "Your hard disk is failing. Back up your important files and replace the hard disk ")
    ?f1 <- (bsod yes)
    ?f2 <- (freq-bsod intermitent)
    =>
    (printout t "Confidence user gets blue screen of death:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence blue screen of death is cause by the failing of the hdd based on how often the user experiences bsod:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf ))
 
 (defrule xai-run-defrag
    (declare (salience 8))
    (repair "Defragment the drive")
    ?f1 <- (slow veryslow|slow|abitslow)
    (virus-scan yes)
    (fragmentation yes)
    =>
    (printout t "Confidence computer is running slower than usual:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence virus scan completed succesfully:  ")
     (printout t (* 100 ?*scan-confidence*))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer is slow because of fragmentation:  ")
     (printout t (* 100 ?*fragmentation-confidence*))
     (printout t "%")
     (printout t crlf crlf ))
    
    
 
(defrule xai-remove-bloatware
    (declare (salience 8))
    (repair "Remove bloatware (useless programs on your computer that usually open on startup and clog the processor) ")
    ?f1 <- (slow veryslow|slow|abitslow)
    (virus-scan yes)
    ?f3 <- (fragmentation no)
    =>
    (printout t "Confidence computer is running slower than usual:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence virus scan completed succesfully:  ")
     (printout t (* 100 ?*scan-confidence*))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer is slow because of bloatware:  ")
     (printout t (* 100 ?*fragmentation-confidence*))
     (printout t crlf crlf ))
     
    
(defrule xai-run-virus-scan
    (declare (salience 8))
    (repair "Run a virus scan")
    ?f1 <- (slow veryslow|slow|abitslow)
    (virus-scan no)
    =>
    (printout t "Confidence computer is running slower than usual:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf )
     (printout t "Confidence computer may be running slow because of viruses or PUP (potentially unwated programs):  ")
     (printout t (* 100 ?*scan-confidence*))
     (printout t "%")
     (printout t crlf crlf ))
    
 
(defrule xai-update-drivers
    (declare (salience 8))
    (repair "Update your graphics card drivers")
    ?f1 <- (low-res yes)
    =>
    (printout t "Confidence low resolution is caused by out-dated graphics card drivers:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf crlf))
    
    
(defrule xai-reinstall-os-computer
    (declare (salience 8))
    ?f1 <- (repair "Your Operating System might not be functioning properly. Try fixing it either with the help of a tool or a professional or reinstall it")
    ?f2 <- (freezes yes)
    =>
     (printout t "Confidence user experiences freezes:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence freezes are caused by a fault in the Operating System:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf crlf))
     

(defrule xai-bad-ram
    (declare (salience 8))
    (repair "Replace your RAM memory")
    ?f1 <- (ram-ok no)
    ?f2 <- (freezes yes)
    =>
     (printout t "Confidence user experiences freezes:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence freezes are caused by a fault in the RAM memory:  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf crlf))
     
(defrule xai-generic-computer-problem
    (declare (salience 8))
    (device computer)
    (repair "Go to a repair shop and let a professional check your hardware")
    =>
    (printout t "Your computer is suffering from a problem which cannot be predicted without an analysis of the computer itself")
    (printout t crlf crlf))

(defrule xai-defective-fans
    (declare (salience 8))
    ?f1 <- (overheating yes)
    (repair "Your fans are not functioning properly. You need to either clean your computer or change them entirely")
    =>
     (printout t "Confidence laptop overheats due to a poor cooling system (either the fans or blockage to the airflow): ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf crlf))
    
(defrule xai-dying-battery
    (declare (salience 8))
    (repair "Your laptop's battery is dying and needs to be replaced ")
    ?f2 <- (shut-down-laptop yes)
    ?f3 <- (charged-recently yes)
    =>
     (printout t "Confidence the laptop is not overheating:  ")
     (printout t (* ?*overheat-no-confidence* 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence the battery causes the laptop to shut down after a period of time:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence the laptop has been charged recently:  ")
     (printout t (* (get-cf ?f3) 100))
     (printout t "%")
     (printout t crlf crlf))
   
    
(defrule xai-charge-laptop
    (declare (salience 8))
    (repair "Charge your laptop")
    ?f2 <- (shut-down-laptop yes)
    ?f3 <- (charged-recently no)
    =>
     (printout t "Confidence the laptop is not overheating:  ")
     (printout t (* ?*overheat-no-confidence* 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence the battery causes the laptop to shut down after a period of time:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence the laptop has not been charged recently:  ")
     (printout t (* (get-cf ?f3) 100))
     (printout t "%")
     (printout t crlf crlf))
 
 
(defrule xai-generic-problem-laptop
    (declare (salience 8))
    (device laptop)
    (repair "Go to a repair shop and let a professional check your hardware")
    =>
    (printout t "Your laptop is suffering from a problem which cannot be predicted without an analysis of the laptop itself")
    (printout t crlf crlf))
    
(defrule xai-key-blockage
    (declare (salience 8))
    ?f1 <- (keyboard-problems yes)
    ?f2 <- (key-problem some)
    (repair "Either the key is damaged or there is some blockage under it. Try turning your laptop upside down and gently tapping its back ")
    =>
     (printout t "Confidence the user experiences problems with the keyboard  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence only some keys do not work:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf crlf))
    
(defrule xai-update-keyboard-drivers
    (declare (salience 8))
    ?f1 <- (keyboard-problems yes)
    ?f2 <- (key-problem all)
    ?f3 <- (keyboard-drivers-up-to-date no)
   (repair "Update your keyboard drivers")
    =>
    (printout t "Confidence the user experiences problems with the keyboard  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence all keys do not work:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
    (printout t crlf)
     (printout t "Confidence keyboard drivers are not up-to-date:  ")
     (printout t (* (get-cf ?f3) 100))
     (printout t "%")
     (printout t crlf crlf))
    
(defrule xai-loose-connection
    (declare (salience 8))
    ?f1 <- (keyboard-problems yes)
    ?f2 <- (key-problem all)
    ?f3 <- (keyboard-drivers-up-to-date yes)
    (repair "There may be a loose connection between your motherboard and your keyboard. Go to a repair shop to have it fixed ")
    =>
    (printout t "Confidence the user experiences problems with the keyboard  ")
     (printout t (* (get-cf ?f1) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence all keys do not work:  ")
     (printout t (* (get-cf ?f2) 100))
     (printout t "%")
     (printout t crlf)
     (printout t "Confidence keyboard drivers are up-to-date:  ")
     (printout t (* (get-cf ?f3) 100))
     (printout t "%")
     (printout t crlf crlf))
 
 
;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-computer-state ""
	(runs-normally yes)
	;(not (repair ?))
	=>
	(assert (repair "No repair needed")))

(defrule plug-in ""
	(computer-plugged-in no)
	;(not (repair ?))
	=>
	(assert (repair "Plug in the computer")))
	
(defrule broken-psu ""
    (computer-plugged-in yes)
    ;(not (repair ?))
    =>
    (assert (repair "PSU needs replacement")))
    
(defrule laptop-needs-charging
    (laptop-charged alongtimeago)
   ;(not (repair ?))
    =>
    (assert (repair "Charge your laptop")))
    

(defrule dead-battery
    (laptop-charged quiterecently|recently|justnow)
    ;(not (repair ?))
    =>
    (assert (repair "Your battery does not work anymore and needs to be replaced")))
    
(defrule hardware-problem
    (freq-bsod always)
    ;(not (repair ?))
    =>
    (assert (repair "Your computer suffers from hardware problems (one of your components has most likely died). Visit a repair shop. ")))
    
(defrule hdd-failing
    (freq-bsod intermitent)
    ;(not (repair ?))
    =>
    (assert (repair "Your hard disk is failing. Back up your important files and replace the hard disk ")))
    
(defrule run-virus-scan
    ;(declare (CF 0.9))
    (virus-scan no)
    ;(not (repair ?))
    =>
    (assert (repair "Run a virus scan")))
    
(defrule run-defrag
    (fragmentation yes)
    ;(not (repair ?))
    =>
    (assert (repair "Defragment the drive")))
    
(defrule remove-bloatware
    (fragmentation no)
    ;(not (repair ?))
    =>
    (assert (repair "Remove bloatware (useless programs on your computer that usually open on startup and clog the processor) ")))
    
(defrule update-drivers
    (low-res yes)
    ;(not (repair ?))
    =>
    (assert (repair "Update your graphics card drivers")))


(defrule reinstall-os-computer
    (declare (CF 0.65))
    (ram-ok yes)
    ;(not (repair ?))
    =>
    (assert (repair "Your Operating System might not be functioning properly. Try fixing it either with the help of a tool or a professional or reinstall it")))
    
(defrule bad-ram
    (ram-ok no)
    ;(not (repair ?))
    =>
    (assert (repair "Replace your RAM memory")))
    
(defrule generic-problem-computer
    (device computer)
    (freezes no)
    ;(not (repair ?))
    =>
    (assert (repair "Go to a repair shop and let a professional check your hardware")))
    
(defrule defective-fans
    (overheating yes)
    ;(not (repair ?))
    =>
    (assert (repair "Your fans are not functioning properly. You need to either clean your computer or change them entirely")))
    
(defrule dying-battery
    (declare (CF ?*overheat-no-confidence*))
    (charged-recently yes)
    ;(not (repair ?))
    =>
    (assert (repair "Your laptop's battery is dying and needs to be replaced ")))
    
(defrule charge-laptop
    (declare (CF ?*overheat-no-confidence*))
    (charged-recently no)
    ;(not (repair ?))
    =>
    (assert (repair "Charge your laptop")))
    
(defrule generic-problem-laptop
    (keyboard-problems no)
    ;(not (repair ?))
    =>
    (assert (repair "Go to a repair shop and let a professional check your hardware")))
    
(defrule key-blockage
    (key-problem some)
    ;(not (repair ?))
    =>
    (assert (repair "Either the key is damaged or there is some blockage under it. Try turning your laptop upside down and gently tapping its back ")))
    
(defrule update-keyboard-drivers
    (keyboard-drivers-up-to-date no)
    ;(not (repair ?))
    =>
    (assert (repair "Update your keyboard drivers")))
    
(defrule loose-connection
    (keyboard-drivers-up-to-date yes)
    ;(not (repair ?))
    =>
    (assert (repair "There may be a loose connection between your motherboard and your keyboard. Go to a repair shop to have it fixed ")))
    
    
;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Computer Diagnosis Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  ?f <- (repair ?item )
  =>
  (set-fuzzy-display-precision 4)
  (printout t crlf crlf)
  (printout t "Suggested Repair:")
  (printout t crlf crlf)
  (format t " %s%n" ?item)
  (printout t crlf)
  (printout t "Confidence factor: ")
  (printout t (* (get-cf ?f) 100))
  (printout t "%")
  (printout t crlf))
  


