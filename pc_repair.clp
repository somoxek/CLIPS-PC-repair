
;;;======================================================
;;;   PC Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a PC.
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

(defrule determine-pc_starts-state ""
   (not (pc_starts ?))
   (not (repair ?))
   =>
   (assert(pc_starts (yes-or-no-p "Does the PC start (yes/no)? "))))

(defrule determine-power_connection-state ""
   (pc_starts no)
   (not (repair ?))   
   =>
   (assert (power_connection (yes-or-no-p "Is the power connected (yes/no)? "))))

(defrule determine-power_supply_working-state ""
   (pc_starts no)
   (power_connection yes)
   (not (repair ?))
   =>
   (assert(power_supply_working (yes-or-no-p "Is the power supply unit working (yes/no)? "))))

(defrule determine-bios_battery_light-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (not (repair ?))
   =>
   (assert (bios_battery_light (yes-or-no-p "Does the BIOS battery light glow (yes/no)? "))))

(defrule determine-motherboard_working-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (bios_battery_light yes)
   (not (repair ?))
   =>
   (assert(motherboard_working (yes-or-no-p "Is the motherboard working (yes/no)? "))))

(defrule determine-after_disconnect_parts-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (bios_battery_light yes)
   (motherboard_working yes)
   (not (repair ?))
   =>
   (assert(after_disconnect_parts (ask-question "Try to disconnect various peripherals. After disconnecting which part is the pc working? (disk/CPU/GPU/RAM) " disk gpu cpu ram))))

(defrule determine-os_starts-state ""
   (pc_starts yes)
   (not (repair ?))
   =>
   (assert(os_starts (yes-or-no-p "Does the Operating System (OS) starts (yes/no)? "))))

(defrule determine-repair_mode-state ""
   (pc_starts yes)
   (os_starts yes)
   (not (repair ?))
   =>
   (assert(repair_mode (yes-or-no-p "Is the OS repair mode starting (yes/no)? "))))

(defrule determine-automatic_repair-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (not (repair ?))
   =>
   (assert(automatic_repair (yes-or-no-p "Is the automatic repair working (yes/no)? "))))

(defrule determine-reset_updates-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (automatic_repair no)
   (not (repair ?))
   =>
   (assert(reset_updates (yes-or-no-p "Is it possible to reset recent updates (yes/no)? "))))

(defrule determine-repair_cmd_access-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (automatic_repair no)
   (reset_updates no)
   (not (repair ?))
   =>
   (assert(repair_cmd_access (yes-or-no-p "Can you access the repair command line (yes/no)? "))))

(defrule determine-bios_access-state ""
   (pc_starts yes)
   (os_starts no)
   (not (repair ?))
   =>
   (assert(bios_access (yes-or-no-p "Can you acces the BIOS (yes/no)? "))))

(defrule determine-bios_uptodate-state ""
   (pc_starts yes)
   (os_starts no)
   (bios_access yes)
   (not (repair ?))
   =>
   (assert(bios_uptodate (yes-or-no-p "Is the BIOS up to date (yes/no)? "))))

(defrule determine-boot_order-state ""
   (pc_starts yes)
   (os_starts no)
   (bios_access yes)
   (bios_uptodate yes)
   (not (repair ?))
   =>
   (assert(boot_order (ask-question "Which item is first in the boot order? (CD/network/USB/disk/other)? " cd network usb disk other))))

(defrule determine-usb_adapter_access-state ""
   (or   (and  (pc_starts yes)
               (os_starts no)
               (bios_access yes)
               (bios_uptodate yes)
               (boot_order disk))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode yes)
               (automatic_repair no)
               (reset_updates no)
               (repair_cmd_access no))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode no)
               (login_screen no))
         (and  (pc_starts no)
               (power_connection yes)
               (power_supply_working yes)
               (bios_battery_light yes)
               (motherboard_working yes)
               (after_disconnect_parts disk)))
   (not (repair ?))
   =>
   (assert(usb_adapter_access (yes-or-no-p "Can the disk be accessed via an USB-adapter (yes/no)? "))))

(defrule determine-chkdsk-state ""
   (or   (and  (pc_starts yes)
               (os_starts no)
               (bios_access yes)
               (bios_uptodate yes)
               (boot_order disk)
               (usb_adapter_access yes))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode yes)
               (automatic_repair no)
               (reset_updates no)
               (repair_cmd_access yes)))
   (not (repair ?))
   =>
   (assert(chkdsk (yes-or-no-p "Run chkdsk /r /f /x Is your PC no working (yes/no)? "))))

(defrule determine-login_screen-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (not (repair ?))
   =>
   (assert(login_screen (yes-or-no-p "Does the login screen appear (yes/no)? "))))

(defrule determine-login_possible-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (not (repair ?))
   =>
   (assert(login_possible (yes-or-no-p "Can you login (yes/no)? "))))

(defrule determine-company_network-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (login_possible no)
   (not (repair ?))
   =>
   (assert(company_network (yes-or-no-p "Is your PC connected to the company network (yes/no)? "))))

(defrule determine-local_administrator-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (login_possible no)
   (company_network no)
   (not (repair ?))
   =>
   (assert(local_administrator (yes-or-no-p "Is it possible to connect via an local administrator account (yes/no)? "))))



;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule no_power_connection ""
   (power_connection no)
   (not (repair ?))
   =>
   (assert (repair "Connect power.")))

(defrule power_supply_defect ""
   (power_supply_working no)
   (not (repair ?))
   =>
   (assert (repair "Replace power supply unit.")))

(defrule no_bios_battery_light ""
   (bios_battery_light no)
   (not (repair ?))
   =>
   (assert (repair "Replace the BIOS battery.")))

(defrule motherboard_defect ""
   (motherboard_working no)
   (not (repair ?))
   =>
   (assert (repair "Replace the motherboard.")))

(defrule part_defect_gpu ""
   (after_disconnect_parts gpu)
   (not (repair ?))
   =>
   (assert (repair "Replace GPU. Use internal GPU in the meantime.")))

(defrule part_defect_cpu ""
   (after_disconnect_parts cpu)
   (not (repair ?))
   =>
   (assert (repair "Replace CPU.")))

(defrule part_defect_ram ""
   (after_disconnect_parts ram)
   (not (repair ?))
   =>
   (assert (repair "Remove faulty RAM stick.")))

(defrule no_bios_access ""
   (bios_access no)
   (not (repair ?))
   =>
   (assert (repair "Flash/repair BIOS via USB/network.")))

(defrule bios_is_not_uptodate ""
   (bios_uptodate no)
   (not (repair ?))
   =>
   (assert (repair "Update BIOS via USB/network.")))

(defrule boot_order_false ""
   (or   (boot_order cd)
         (boot_order network)
         (boot_order usb)
         (boot_order other))
   (not (repair ?))
   =>
   (assert (repair "Change first place in boot order to disk.")))

(defrule disk_defective ""
   (usb_adapter_access no)
   (not (repair ?))
   =>
   (assert (repair "Disk is defective.")))

(defrule chkdsk_not_working ""
   (chkdsk no)
   (not (repair ?))
   =>
   (assert (repair "Save data, reinstall OS on new disk.")))

(defrule reset_updates_works ""
   (reset_updates yes)
   (not (repair ?))
   =>
   (assert (repair "Restart and try update again.")))

(defrule connection_with_company ""
   (company_network yes)
   (not (repair ?))
   =>
   (assert (repair "Let the password change via Active Directory.")))

(defrule local_administrator_works ""
   (local_administrator yes)
   (not (repair ?))
   =>
   (assert (repair "Open browser, change PW via Webmail, connect to VPN, change user and log in with user.")))

(defrule local_administrator_not_working ""
   (local_administrator no)
   (not (repair ?))
   =>
   (assert (repair "Bring PC to company.")))

(defrule repair_done ""
   (or   (chkdsk yes)
         (automatic_repair yes)
         (login_possible yes))
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
