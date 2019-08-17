;;; sync-window.el --- Synchronize two side-by-side windows

;; Copyright (C) 2013  Zawada

;; Author: naehring <i@tn-home.de>
;; Keywords: match, convenience

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

;; This is experimental code.
;; The main function is sync-window-mode (which see).

;;; Code:

(defface sync-window-face ;; originally copied from font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Yellow" :background "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Red" :background  "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue" :background "Yellow"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue" :background "Yellow"))
    (((class color) (min-colors 8)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used to highlight regions in `sync-window-mode' slaves."
  :group 'sync-window)

(defvar sync-window-overlay nil
  "Overlay for current master region in `sync-window-mode' slaves.")
(make-variable-buffer-local 'sync-window-overlay)

(defvar sync-window-slave-buffer nil
  "The slave buffer's window is synchronized to the current buffer's window.")
(make-variable-buffer-local 'sync-window-slave-buffer)

(defvar sync-window-selected-window nil
  "Set to the currently selected window in `post-command-hook'.")

(defun sync-window-cleanup ()
  "Clean up after `sync-window-mode'."
  (interactive)
  (if (overlayp sync-window-overlay)
      (progn
    (delete-overlay sync-window-overlay)
    (setq sync-window-overlay nil))
    (remove-overlays (point-min) (point-max) 'sync-window-slave t)))

(defvar sync-window-master-hook nil
  "Hooks to be run by `sync-window' in the master window ")

(defun sync-window (&optional display-start)
  "Synchronize point position of sync-window-slave-buffer."
  (interactive)
  (let ((master-win (selected-window))
	(slave-win (and (bufferp sync-window-slave-buffer)
		    (buffer-live-p sync-window-slave-buffer)
		    (loop for w in (window-list)
			  thereis (and (equal (window-buffer w) sync-window-slave-buffer)
				       w)))))
    (when slave-win
      (let ((p (line-number-at-pos))
	    (start (line-number-at-pos (or display-start (window-start))))
	    (vscroll (window-vscroll))
	    breg ereg)
	(when (use-region-p)
	  (setq breg (line-number-at-pos (region-beginning))
		ereg  (line-number-at-pos (if (bolp) (1- (region-end)) (region-end)))))
	(run-hooks 'sync-window-master-hook)
	(select-window slave-win)
	(goto-char (point-min))
	(when breg
	  (sync-window-cleanup)
	  (overlay-put (setq sync-window-overlay (make-overlay (line-beginning-position breg) (line-end-position ereg))) 'face 'sync-window-face)
	  (overlay-put sync-window-overlay 'sync-window-slave t))
	(setq start (line-beginning-position start))
	(forward-line (1- p))
	(set-window-start (selected-window) start t)
	(set-window-vscroll (selected-window) vscroll)
	(select-window master-win)
	(unless display-start
	  (redisplay t))
	))))

(defvar sync-window-mode-hook nil
  "Hooks to be run at start of `sync-window-mode'.")

(define-minor-mode sync-window-mode
  "Synchronized view of two buffers in two side-by-side windows."
  :group 'windows
  :lighter " ⇕"
  (if sync-window-mode
      (progn
	(setq sync-window-slave-buffer (get-buffer (read-buffer "Buffer to be synchronized:" (cdr (mapcar 'window-buffer (window-list nil 'noMiniBuffers))) t)))
	(add-hook 'post-command-hook 'sync-window-post-command-function 'append t)
	(add-to-list 'window-scroll-functions 'sync-window-scroll-function)
	(run-hooks 'sync-window-mode-hook)
	(sync-window))
    (remove-hook 'post-command-hook 'sync-window-post-command-function t)
    (setq window-scroll-functions (remove 'sync-window-scroll-function window-scroll-functions))
    ))

(defun sync-window-post-command-function ()
  "Set `sync-window-selected-window' and call `sync-window'."
  (setq sync-window-selected-window (selected-window))
  (sync-window))

(defun sync-window-scroll-function (&optional window display-start)
  "This wrapper makes sure that `sync-window' is fired from `post-command-hook'
only when the buffer of the active window is in `sync-window-mode'."
  (when (window-valid-p sync-window-selected-window)
    (with-selected-window sync-window-selected-window
      (when sync-window-mode
	(sync-window display-start)))))

(defvar sync-window-hscroll-pos nil
  "`sync-window-pre-run-hscroll-functions-maybe' sets this
to the cons of the active window and the `window-hscroll'.")

(defvar-local sync-window-hscroll-functions nil
  "Functions to be run when hscroll position changes or when another window is selected.")

(defun sync-window-pre-run-hscroll-functions-maybe ()
  "Set `sync-window-hscroll-pos' to the pair of the active window and the `window-hscroll'."
  (setq sync-window-hscroll-pos (cons (selected-window) (window-hscroll))))

(defun sync-window-run-hscroll-functions-maybe ()
  "Run functions in variable `sync-window-hscroll-functions'."
  (let ((window (selected-window))
	(hscroll (window-hscroll)))
    (message "%s %s" hscroll sync-window-hscroll-pos)
    (unless (equal sync-window-hscroll-pos (cons window hscroll))
      (run-hook-with-args 'sync-window-hscroll-functions window hscroll)
      (setq sync-window-hscroll-pos (cons window hscroll)))))

(defvar-local sync-window-hscroll-buffer nil
  "Buffer to be synchronized.")

(defun sync-window-hscroll-other (window pos)
  "Synchronize hscroll position of `sync-window-hscroll-buffer' with WINDOW and POS."
  (message "*")
  (when (buffer-live-p sync-window-hscroll-buffer)
    (let* ((windows (get-buffer-window-list sync-window-hscroll-buffer))
	   (window (if (eq (car windows) (selected-window)) (cadr windows) (car windows))))
      (when window
	(with-current-buffer (window-buffer window)
	  (goto-char (+ (line-beginning-position) pos)))
	(set-window-hscroll window pos)))))

(define-minor-mode sync-window-hscroll-mode
  "Synchronize hscroll of two windows."
  :group 'sync-window
  :lighter " ↔"
  (if sync-window-hscroll-mode
      (let* ((win (cadr (window-list)))
	     (win-buf (and win (buffer-name (window-buffer win))))
	     (buf (read-buffer "Buffer:" win-buf)))
	(when buf
	  ; (add-hook 'pre-command-hook #'sync-window-pre-run-hscroll-functions-maybe t t)
	  (add-hook 'post-command-hook #'sync-window-run-hscroll-functions-maybe t t)
	  (setq sync-window-hscroll-buffer (get-buffer buf))
	  (add-hook 'sync-window-hscroll-functions
		    #'sync-window-hscroll-other
		    nil t)))
    (setq sync-window-hscroll-buffer nil)
    (remove-hook 'sync-window-hscroll-functions
		 #'sync-window-hscroll-other
		 t)
    (remove-hook 'post-command-hook #'sync-window-run-hscroll-functions-maybe t)
    ; (remove-hook 'pre-command-hook #'sync-window-pre-run-hscroll-functions-maybe t)
    ))

(provide 'sync-window)
;;; sync-window.el ends here
