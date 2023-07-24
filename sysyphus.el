;;; sysyphus.el --- control sysfs from Emacs  -*- lexical-binding:t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1
;; Package-Requires: ()
;; Keywords: hardware

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(defgroup sysyphus ()
  "Control Linux sysfs(5) from Emacs."
  :group 'hardware)


;;; Sleep

(defun sysyphus--read (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

;;;###autoload
(defun sysyphus-sleep ()
  (interactive)
  (message "Going to sleep...")
  (write-region "mem" nil "/sys/power/state" nil :no-message))

(defvar sysyphus--auto-sleep-timer nil)

;;;; Auto sleep

;;;###autoload
(define-minor-mode sysyphus-auto-sleep-mode
  nil
  :global t
  ;;;;; Teardown
  (when sysyphus--auto-sleep-timer
    (cancel-timer sysyphus--auto-sleep-timer))
  (when sysyphus-auto-sleep-mode
    ;;;;; Construction
    (setq sysyphus--auto-sleep-timer (run-with-idle-timer sysyphus-auto-sleep-delay
							  :repeat
							  #'sysyphus-sleep))))

(defcustom sysyphus-auto-sleep-delay 300
  "How many seconds to wait while Emacs is idle before going to sleep automatically when `sysyphus-auto-sleep-mode' is on."
  :type 'number
  :set (lambda (symbol seconds)
	 (set symbol seconds)
	 (when sysyphus-auto-sleep-mode
	   ;; Reinitialize:
	   (sysyphus-auto-sleep-mode 1))))


;;; Backlight

(defcustom sysyphus-backlight nil
  "Device under /sys/class/backlight/ to use for brightness control.

If nil on first access, it is set to the first device found alphabetically.")

(defun sysyphus--get-backlight ()
  (interactive)
  (or sysyphus-backlight
      (setq sysyphus-backlight (car (directory-files
				  "/sys/class/backlight/"
				  nil
				  directory-files-no-dot-files-regexp)))))

(defun sysyphus--read-backlight (parameter)
  (string-to-number
   (sysyphus--read
    (file-name-concat "/sys/class/backlight/"
		      (sysyphus--get-backlight)
		      parameter))))

(defcustom sysyphus-brightness-message-format "%.0f%% brightness"
  "Format of the message to echo when getting or setting the screen brightness.")

;;;###autoload
(defun sysyphus-brightness-get ()
  (interactive)
  (let ((brightness (* 100 (/ (float (sysyphus--read-backlight "brightness"))
			      (sysyphus--read-backlight "max_brightness")))))
    (when (called-interactively-p 'interactive)
      (message sysyphus-brightness-message-format brightness))
    brightness))

;;;###autoload
(defun sysyphus-brightness-set (percent)
  (interactive "nBrightness: ")
  (let ((clamped (max 1 (min 100 percent))))
    (write-region (number-to-string (floor (* clamped 0.01 (sysyphus--read-backlight "max_brightness"))))
		  nil
		  (file-name-concat "/sys/class/backlight/"
				    (sysyphus--get-backlight)
				    "brightness")
		  nil
		  :no-message)
    (message sysyphus-brightness-message-format clamped)))

(defcustom sysyphus-brightness-increment 5
  "Amount that `sysyphus-brightness-up' and `sysyphus-brightness-down' change brightness by."
  :type 'number)

;;;###autoload
(defun sysyphus-brightness-up (&optional multiplier)
  (interactive "p")
  (sysyphus-brightness-set (+ (sysyphus-brightness-get)
			      (* sysyphus-brightness-increment
				 (or multiplier 1)))))

;;;###autoload
(defun sysyphus-brightness-down (&optional multiplier)
  (interactive "p")
  (sysyphus-brightness-up (- (or multiplier 1))))

;;;; Auto dim

(defcustom sysyphus-brightness-auto-dimming 3
  "Number of `sysyphus-brightness-increment's that `sysyphus-brightness-auto-dim-mode' decreases the brightness by."
  :type 'number)

(defvar sysyphus-brightness--auto-dim-timer nil)

(defvar sysyphus--brightness-pre-dim 100)

(defun sysyphus-brightness-auto-dim-reset ()
  (sysyphus-brightness-set sysyphus-pre-dim)
  (remove-hook 'pre-command-hook #'sysyphus-auto-dim-reset))

(defun sysyphus-brightness-auto-dim ()
  (setq sysyphus-pre-dim (sysyphus-brightness-get))
  (add-hook 'pre-command-hook #'sysyphus-auto-dim-reset)
  (sysyphus-brightness-down sysyphus-brightness-auto-dimming))

;;;###autoload
(define-minor-mode sysyphus-brightness-auto-dim-mode
  nil
  :global t
  ;;;;; Teardown
  (when sysyphus-brightness--auto-dim-timer
    (cancel-timer sysyphus-brightness--auto-dim-timer))
  (when sysyphus-brightness-auto-dim-mode
    ;;;;; Construction
    (setq sysyphus-brightness--auto-dim-timer (run-with-idle-timer sysyphus-brightness-auto-dim-delay
								   :repeat
								   #'sysyphus-brightness-auto-dim))))

(defcustom sysyphus-brightness-auto-dim-delay 270
  "How many seconds to wait while Emacs is idle before going to sleep automatically when `sysyphus-auto-sleep-mode' is on."
  :type 'number
  :set (lambda (symbol seconds)
	 (set symbol seconds)
	 (when sysyphus-brightness-auto-dim-mode
	   ;; Reinitialize:
	   (sysyphus-brightness-auto-dim-mode 1))))

(provide 'sysyphus)

;;; sysyphus.el ends here
