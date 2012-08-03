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

(defcustom capture-mylife:temporary-directory
  (expand-file-name "~/.emacs.d/capture-mylife/")
  "Directory for temporary capture images"
  :type 'string
  :group 'capture-mylife)

(defvar capture-mylife:capture-active-executable
  (case system-type
    (gnu/linux "scrot")
    (otherwise nil)))

(defmacro capture-mylife:system-p (sys)
  `(eq system-type ,sys))

(defun capture-mylife:platform-capture-desktop-executables ()
  (cond ((capture-mylife:system-p 'gnu/linux)  '("scrot" "import"))
        ((capture-mylife:system-p 'darwin)     '("screencapture"))
        ((capture-mylife:system-p 'windows-nt) '("nircmd.exe"))))

(defun capture-mylife:search-executable (execs)
  (loop for exe in execs
        when (executable-find exe)
        return exe))

(defvar capture-mylife:capture-desktop-executable
  (let* ((execs (capture-mylife:platform-capture-desktop-executables))
         (exec  (capture-mylife:search-executable execs)))
    (or exec
        (error (format "Please install %s"
                       (mapconcat 'identify execs " or "))))))

(defvar capture-mylife:convert-movie-executable
  (cond ((executable-find "avconv") "avconv")
        ((executable-find "ffmpeg") "ffmpeg")
        (t nil)))

(defvar capture-mylife:timer)
(defvar capture-mylife:file-index 0)
(defvar capture-mylife:file-format "%08d.png")
(defvar capture-mylife:type nil)
(defvar capture-mylife:cycle nil)

(defmacro capture-mylife:has-active-command-p (cmd)
  `(string= capture-mylife:capture-active-executable ,cmd))

(defun capture-mylife:capture-command-active (file)
  (cond ((capture-mylife:has-active-command-p "scrot")
         (format "scrot -z -u -b %s" file))))

(defmacro capture-mylife:has-desktop-command-p (cmd)
  `(string= capture-mylife:capture-command-desktop ,cmd))

(defun capture-mylife:capture-command-desktop (file)
  (cond ((capture-mylife:has-desktop-command-p "scrot")
         (format "scrot -z -b %s" file))
        ((capture-mylife:has-desktop-command-p "import")
         (format "import -window root %s" file))
        ((capture-mylife:has-desktop-command-p "screencapture")
         (format "screencapture -x %s" file))
        ((capture-mylife:has-desktop-command-p "nircmd.exe")
         (format "nircmd.exe savescreenshot %s" file))
        (t (error "Sorry cannot take frame screnshot on your platform"))))

(defun capture-mylife:capture-command (type file)
  (if (capture-mylife:type-active-p type)
      (capture-mylife:capture-command-active file)
    (capture-mylife:capture-command-desktop file)))

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

(defun* capture-mylife:type-active-p (&optional (type capture-mylife:type))
  (string= type "active"))

(defun capture-mylife:frame-width ()
  (if (capture-mylife:type-active-p)
      (frame-pixel-width)
    (display-pixel-width)))

(defun capture-mylife:frame-height ()
  (if (capture-mylife:type-active-p)
      (frame-pixel-height)
    (display-pixel-height)))

(defun capture-mylife:format-output-scale (width height ratio)
  (format "%dx%d" (floor (* width  ratio)) (floor (* height ratio))))

(defun capture-mylife:command-ffmpeg (output scale fps)
  (format "%s -y -f image2 -r %d -i \"%s\" -s %s %s"
          capture-mylife:convert-movie-executable
          fps
          (concat (file-name-as-directory capture-mylife:temporary-directory)
                  capture-mylife:file-format)
          scale
          output))

(defun capture-mylife:x-style-p (str)
  (string-match "^[1-9][0-9]*x[1-9][0-9]*$" str))

(defun capture-mylife:format-output-size (default)
  (if default
      (format "Output size(default %s): " default)
    (format "Output size: ")))

(defun capture-mylife:read-output-scale ()
  (let (width height default)
    (when (and window-system capture-mylife:type)
      (setq width (capture-mylife:frame-width)
            height (capture-mylife:frame-height)
            default (format "%dx%d" width height)))
    (let* ((input (read-string (capture-mylife:format-output-size default)
                               nil nil default))
           (input-obj (car (read-from-string input))))
      (cond ((capture-mylife:x-style-p input) input)
            ((and default (numberp input-obj))
             (capture-mylife:format-output-scale width height ratio))
            (t (error (format "Invalid output scale: %s" input)))))))

(defun capture-mylife:support-encoding-formats ()
  (with-temp-buffer
    (let ((cmd capture-mylife:convert-movie-executable))
      (unless (zerop (call-process-shell-command
                      (format "%s -formats" cmd) nil t))
        (error "Failed: check support encodings"))
      (goto-char (point-min))
      (loop while (re-search-forward "^ D?E \\([^ ]+\\)" nil t)
            collect (match-string-no-properties 1)))))

(defun capture-mylife:time-to-string ()
  (format-time-string "%Y-%m%d-%H%M%S" (current-time)))

(defun capture-mylife:read-format ()
  (completing-read "format (default avi): "
                   (capture-mylife:support-encoding-formats)
                   nil t nil nil "avi"))

(defun capture-mylife:output-file (format)
  (format "%s.%s" (capture-mylife:time-to-string) format))

(defun capture-mylife:convert-movie-command (output scale fps)
  (cond ((member capture-mylife:convert-movie-executable '("avconv" "ffmpeg"))
         (capture-mylife:command-ffmpeg output scale fps))
        (t (error "Please install `avconv' or `ffmpeg'"))))

(defun capture-mylife:convert-movie (outformat scale fps)
  (interactive
   (list (capture-mylife:read-format)
         (capture-mylife:read-output-scale)
         (read-number "frame per second: " 10)))
  (let* ((output (concat capture-mylife:temporary-directory
                         (capture-mylife:output-file outformat)))
         (cmd (capture-mylife:convert-movie-command output scale fps))
         (buf (get-buffer-create "*capture mylife*")))
    (set-process-sentinel
     (start-process-shell-command "capture mylife" buf cmd)
     (capture-mylife:create-convert-sentinel output))))

(defun capture-mylife:create-convert-sentinel (output)
  (lexical-let ((file output))
    (lambda (process state)
      (let ((status (process-status process)))
        (when (eq status 'exit)
          (message "Finish converting and output to %s" file)
          (delete-process process))))))

(defun capture-mylife:remove-old-images ()
  (let ((imgs (directory-files capture-mylife:temporary-directory t "\.png$")))
    (dolist (img imgs)
      (delete-file img))))

(defun capture-mylife:initialize ()
  (unless (file-directory-p capture-mylife:temporary-directory)
    (make-directory capture-mylife:temporary-directory))
  (capture-mylife:remove-old-images)
  (setq capture-mylife:file-index 0))

(defun capture-mylife:set-handler (type cycle)
  (let ((timer (run-at-time 0 cycle 'capture-mylife:timer-handler type)))
    (setq capture-mylife:type type
          capture-mylife:cycle cycle
          capture-mylife:timer timer)))

(defun capture-mylife:read-capture-type ()
  (if capture-mylife:capture-active-executable
      (completing-read "Capture type: " '("desktop" "active") nil t)
    "desktop"))

(defun capture-mylife:read-capture-cycle ()
  (read-number "Capture cycle: " 10))

(defun capture-mylife:start (type cycle)
  (interactive
   (list (capture-mylife:read-capture-type)
         (capture-mylife:read-capture-cycle)))
  (capture-mylife:initialize)
  (capture-mylife:set-handler type cycle))

(defun capture-mylife:latest-index ()
  (let ((imgs (directory-files capture-mylife:temporary-directory nil "\.png$")))
    (unless imgs
      (error (format "Found no images at %s" capture-mylife:temporary-directory)))
    (let ((latest-img (car (last imgs))))
      (if (string-match "^0*\\([1-9][0-9]*\\)\.png$" latest-img)
          (string-to-int (match-string-no-properties 1 latest-img))))))

(defun capture-mylife:continue (type cycle)
  "Continue to capture"
  (interactive
   (list
    (or capture-mylife:type (capture-mylife:read-capture-type))
    (or capture-mylife:cycle (capture-mylife:read-capture-cycle))))
  (setq capture-mylife:file-index (1+ (capture-mylife:latest-index)))
  (capture-mylife:set-handler type cycle))

(defun capture-mylife:stop ()
  "Finish captureing"
  (interactive)
  (cancel-timer capture-mylife:timer)
  (setq capture-mylife:timer nil)
  (message "Stop capturing. Exec `capture-mylife:convert-movie', if you get movie"))

;;; capture-mylife.el ends here
