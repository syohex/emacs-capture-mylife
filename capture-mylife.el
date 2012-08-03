;;; capture-mylife.el --- capture my life from Emacs

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-capture-mylife

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; History:
;; Revision 0.1  2012/08/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup capture-mylife nil
  "Capture Desktop or Frame from Emacs"
  :group 'image
  :prefix 'capture-mylife:)

(defcustom capture-mylife:period 5
  "Capturing period of seconds"
  :type  'integer
  :group 'capture-mylife)

(defcustom capture-mylife:temporary-directory
  (expand-file-name "~/.emacs.d/capture-mylife/")
  "Directory for temporary capture images"
  :type 'string
  :group 'capture-mylife)

(defcustom capture-mylife:frame-ratio 1
  "Output frame size"
  :type  'number
  :group 'caputre-mylife)

(defcustom capture-mylife:fps 5
  "Frame per second"
  :type  'integer
  :group 'caputre-mylife)

(defvar capture-mylife:capture-executable
  (case system-type
    (gnu/linux "import")
    (otherwise (error "Sorry support only Linux"))))

(defvar capture-mylife:animate-executable
  (cond ((executable-find "avconv") "avconv")
        ((executable-find "ffmpeg") "ffmpeg")
        (t (error "Please install `avconv' or `ffmpeg'"))))

(defvar capture-mylife:timer)

(defun capture-mylife:capture-type 'nil)

(defun capture-mylife:capture-command-frame (file)
  (cond ((executable-find "scrot") (format "scrot -z -u -b %s" file))
        (t (error "Sorry cannot take frame screnshot on your platform"))))

(defun capture-mylife:capture-command-desktop (file)
  (cond ((executable-find "scrot") (format "scrot -z -b %s" file))
        ((executable-find "import") (format "import -window root %s" file))
        (t (error "Sorry cannot take frame screnshot on your platform"))))

(defun capture-mylife:capture-command (type file)
  (setq capture-mylife:capture-type type)
  (if (string= type "frame")
      (capture-mylife:capture-command-frame file)
    (capture-mylife:capture-command-desktop file)))

(defvar capture-mylife:file-index 0)
(defvar capture-mylife:file-format "%08d.png")
(defvar capture-mylife:type nil)

(defun capture-mylife:next-file ()
  (let ((file (concat
               (file-name-as-directory capture-mylife:temporary-directory)
               (format capture-mylife:file-format capture-mylife:file-index))))
    (incf capture-mylife:file-index)
    file))

(defun capture-mylife:timer-handler (type)
  (let* ((file (capture-mylife:next-file))
         (cmd  (capture-mylife:capture-command type file))
         (buf  (get-buffer-create "*capture mylife*")))
    (set-process-sentinel
     (start-process-shell-command "capture mylife" buf cmd)
     (lambda (process state)
       (let ((status (process-status process)))
         (when (eq status 'exit)
           (delete-process process)))))))

(defun capture-mylife:frame-width ()
  (if (string= capture-mylife:type "frame")
      (frame-pixel-width)
    (display-pixel-width)))

(defun capture-mylife:frame-height ()
  (if (string= capture-mylife:type "frame")
      (frame-pixel-height)
    (display-pixel-height)))

(defun capture-mylife:output-scale ()
  (let ((width  (capture-mylife:frame-width))
        (height (capture-mylife:frame-height)))
    (format "%dx%d"
            (floor (* width  capture-mylife:frame-ratio))
            (floor (* height capture-mylife:frame-ratio)))))

(defun capture-mylife:command-ffmpeg (file)
  (format "%s -y -f image2 -r %d -i \"%s\" -s %s %s"
          capture-mylife:animate-executable
          capture-mylife:fps
          (concat (file-name-as-directory capture-mylife:temporary-directory)
                  capture-mylife:file-format)
          (capture-mylife:output-scale)
          file))

(defun capture-mylife:support-encoding-formats ()
  (with-temp-buffer
    (let ((cmd capture-mylife:animate-executable))
      (unless (zerop (call-process-shell-command
                      (format "%s -formats" cmd) nil t))
        (error "Failed: check support encodings"))
      (goto-char (point-min))
      (loop while (re-search-forward "^ D?E \\([^ ]+\\)" nil t)
            collect (match-string-no-properties 1)))))

(defun capture-mylife:time-to-string ()
  (format-time-string "%Y-%m%d-%H%M" (current-time)))

(defun capture-mylife:read-format ()
  (completing-read "format(default avi): "
                   (capture-mylife:support-encoding-formats)
                   nil t nil nil "avi"))

(defun capture-mylife:output-file (format)
  (format "%s.%s" (capture-mylife:time-to-string) format))

(defun capture-mylife:animate-captures ()
  (interactive)
  (let* ((format (capture-mylife:read-format))
         (output (capture-mylife:output-file format))
         (cmd (capture-mylife:command-ffmpeg output))
         (buf (get-buffer-create "*capture mylife*")))
    (set-process-sentinel
     (start-process-shell-command "capture mylife" buf cmd)
     'capture-mylife:animate-sentinel)))

(defun capture-mylife:animate-sentinel (process state)
  (let ((status (process-status process)))
    (when (eq status 'exit)
      (message "Finish animated")
      (delete-process process))))

(defun capture-mylife:remove-old-images ()
  (let ((imgs (directory-files capture-mylife:temporary-directory t "\.png$")))
    (dolist (img imgs)
      (delete-file img))))

(defun capture-mylife:initialize ()
  (unless (file-directory-p capture-mylife:temporary-directory)
    (make-directory capture-mylife:temporary-directory))
  (capture-mylife:remove-old-images)
  (setq capture-mylife:file-index 0))

(defun capture-mylife:set-handler (type)
  (run-at-time 0 capture-mylife:period 'capture-mylife:timer-handler type))

(defun capture-mylife:read-capture-type ()
  (completing-read "Type >> " '("desktop" "frame") nil t))

(defun capture-mylife:start (type)
  (interactive
   (list (capture-mylife:read-capture-type)))
  (setq capture-mylife:type type)
  (capture-mylife:initialize)
  (setq capture-mylife:timer (capture-mylife:set-handler type)))

(defun capture-mylife:latest-index ()
  (let ((imgs (directory-files capture-mylife:temporary-directory nil "\.png$")))
    (unless imgs
      (error (format "Found no images at %s" capture-mylife:temporary-directory)))
    (let ((latest-img (car (last imgs))))
      (if (string-match "^0*\\([1-9][0-9]*\\)\.png$" latest-img)
          (string-to-int (match-string-no-properties 1 latest-img))))))

(defun capture-mylife:continue ()
  "Continue to capture"
  (interactive)
  (setq capture-mylife:file-index (1+ (capture-mylife:latest-index)))
  (let ((type (or capture-mylife:type (capture-mylife:read-capture-type))))
    (setq capture-mylife:timer (capture-mylife:set-handler type))))

(defun capture-mylife:finish ()
  "Finish captureing"
  (interactive)
  (cancel-timer capture-mylife:timer)
  (setq capture-mylife:timer nil)
  (capture-mylife:animate-captures))

;;; capture-mylife.el ends here
