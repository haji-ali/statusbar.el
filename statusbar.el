;;; statusbar.el --- Emacs statusbar          -*- lexical-binding: t -*-

;; Copyright (c) 2018 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/statusbar.el
;; Keywords: statusbar tooltip childframe exwm
;; Version: 0.1
;; Package-Requires: ((emacs "26") (posframe "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

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
;;
;; Display statusbar in childfram in the bottom right over
;; parts of the minibuffer.

;; See README.md for more details

;;; Code:

;;;;; TODO
;;;;; 1. Create posframe and save variable
;;;;; 2. write hide/show facilities based on frame variable
;;;;; 3. Write facility to check if frame is alive (framep)?
;;;;; 4. Figure out minibuffer hook
;;;;; 5. Figure out wordwrapping (disable messages somehow)
;;;;; 6. Prevent clicking on frame?

(require 'subr-x)
(require 'dash)

;;; Customization

(defgroup statusbar nil
  "Display a statusbar over the minibuffer"
  :prefix "statusbar-"
  :group 'convenience)

(defun statusbar-org-mode-clock ()
  (when (org-clock-is-active)
    org-mode-line-string))


(defcustom statusbar-modeline-format
  '(
    (eyebrowse-mode
     (:eval
      (eyebrowse-mode-line-indicator)))
    (:eval (statusbar-org-mode-clock))
    (:eval mu4e-alert-mode-line)
    " "
    display-time-string)
  "Variables to remove from the mode-line and display in the statusbar instead.
All variables listed here will be removed from `global-mode-string' and
displayed in the statusbar instead."
  :type 'list
  :group 'statusbar)

(defcustom statusbar-x-offset 10
  "Offset to the right side of the statusbar.
If you use exwm systray, Offset counts from the last systray icon."
  :type 'integer
  :group 'statusbar)

(defcustom statusbar-left-fringe 0
  "Left fringe width of the statusbar."
  :type 'integer
  :group 'statusbar)

(defcustom statusbar-right-fringe 0
  "Right fringe width of the statusbar."
  :type 'integer
  :group 'statusbar)

;;; Compatibility

;; Those will be defined be exwm and are used to calculate the
;; width of the exwm systemtray
(defvar exwm-systemtray--list)
(defvar exwm-systemtray--icon-min-size)
(defvar exwm-systemtray-icon-gap)
(defvar statusbar@refresh-on-modeline-update t)
(defvar statusbar--frame nil)
(defvar statusbar--parent-frame nil)


;;; Variables

(defvar statusbar--buffer-name " *statusbar-buffer*"
  "Name of the statusbar buffer.")


;;; Private helper functions

(defun statusbar--create ()
  "Return statusbar buffer."
  ;; TODO Maybe we can simply switch the frame?
  (when (frame-live-p statusbar--frame)
    (statusbar--delete))

  (let ((buf (get-buffer-create statusbar--buffer-name)))
    (with-current-buffer buf
      (statusbar--mode))
    (setq statusbar--parent-frame
          (selected-frame))

    (statusbar--create-frame)
    (with-selected-frame statusbar--frame
      (set-window-margins nil 0))))

(cl-defun statusbar--create-frame ()
  "Taken from posframe"
  (let ((left-fringe 0)
        (right-fringe 0)
        (internal-border-width 0)
        (buffer (statusbar--get-buffer))
        (after-make-frame-functions nil)
        (x-gtk-resize-child-frames 'resize-mode))
    (with-current-buffer buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local display-line-numbers nil)
      (setq-local frame-title-format "")
      (setq-local left-margin-width nil)
      (setq-local right-margin-width nil)
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      ;; Need to use `lines-truncate' as our keyword variable instead of
      ;; `truncate-lines' so we don't shadow the variable that we are trying to
      ;; set.
      (setq-local truncate-lines nil)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local show-trailing-whitespace nil)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local tab-line-format nil)

      (add-hook 'kill-buffer-hook (lambda ()
                                    (when (statusbar--alive)
                                      (delete-frame statusbar--frame)
                                      (setq statusbar--frame nil))) nil t)

      ;; Create child-frame
      (unless (frame-live-p statusbar--frame)
        (setq statusbar--frame
              (make-frame
               `(;;,@override-parameters
                 ;; ,(when foreground-color
                 ;;    (cons 'foreground-color foreground-color))
                 ;; ,(when background-color
                 ;;    (cons 'background-color background-color))
                 ;; ,(when font
                 ;;    (cons 'font font))
                 (parent-frame . ,(window-frame))
                 (x-pointer-shape . x-pointer-top-left-arrow)
                 (keep-ratio . nil)
                 (fullscreen . nil)
                 (no-accept-focus . t)
                 (no-focus-on-map . t)
                 (min-width  . 0)
                 (min-height . 0)
                 (border-width . 0)
                 (internal-border-width . 0)
                 (vertical-scroll-bars . nil)
                 (horizontal-scroll-bars . nil)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (menu-bar-lines . 0)
                 (tool-bar-lines . 0)
                 (line-spacing . 0)
                 (unsplittable . t)
                 (no-other-frame . t)
                 (undecorated . t)
                 (visibility . nil)
                 (cursor-type . nil)
                 (minibuffer . nil)
                 (width . 1)
                 (height . 1)
                 (no-special-glyphs . t)
                 (inhibit-double-buffering . nil)
                 ;; Do not save child-frame when use desktop.el
                 (desktop-dont-save . t))))

        (let ((window (frame-root-window statusbar--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (set-window-parameter window 'mode-line-format 'none)
          (set-window-parameter window 'header-line-format 'none)
          (set-window-parameter window 'tab-line-format 'none)
          (set-window-buffer window buffer)
          (set-window-dedicated-p window t))

        (with-selected-frame statusbar--frame
          (setq x-pointer-shape x-pointer-top-left-arrow)
          (set-mouse-color "black"))
        ;; Make sure not hide buffer's content for scroll down.
        (set-window-point (frame-root-window statusbar--frame) 0)
        (raise-frame statusbar--frame)))))

(defun statusbar--get-buffer ()
  "Return statusbar buffer."
  (get-buffer-create statusbar--buffer-name))

(defun statusbar--fit-frame-to-buffer (&optional frame max-height min-height max-width min-width only)
  "Adjust size of FRAME to display the contents of its buffer exactly.
FRAME can be any live frame and defaults to the selected one.
Fit only if FRAME's root window is live.

MAX-HEIGHT, MIN-HEIGHT, MAX-WIDTH and MIN-WIDTH specify bounds on
the new total size of FRAME's root window.  MIN-HEIGHT and
MIN-WIDTH default to the values of `window-min-height' and
`window-min-width' respectively.  These arguments are specified
in the canonical character width and height of FRAME.

If the optional argument ONLY is `vertically', resize the frame
vertically only.  If ONLY is `horizontally', resize the frame
horizontally only.

The new position and size of FRAME can be additionally determined
by customizing the options `fit-frame-to-buffer-sizes' and
`fit-frame-to-buffer-margins' or setting the corresponding
parameters of FRAME."
  (interactive)
  (unless (fboundp 'display-monitor-attributes-list)
    (user-error "Cannot resize frame in non-graphic Emacs"))
  (setq frame (window-normalize-frame frame))
  (when (window-live-p (frame-root-window frame))
    (let* ((char-width (frame-char-width frame))
           (char-height (frame-char-height frame))
           ;; WINDOW is FRAME's root window.
           (window (frame-root-window frame))
           (line-height (window-default-line-height window))
           (parent (frame-parent frame))
           (monitor-attributes
            (unless parent
              (frame-monitor-attributes frame)))
           ;; FRAME'S parent or display sizes.  Used in connection
           ;; with margins.
           (geometry
            (unless parent
              (cdr (assq 'geometry monitor-attributes))))
           (parent-or-display-width
            (if parent
                (frame-native-width parent)
              (nth 2 geometry)))
           (parent-or-display-height
            (if parent
                (frame-native-height parent)
              (nth 3 geometry)))
           ;; FRAME's parent or workarea sizes.  Used when no margins
           ;; are specified.
           (parent-or-workarea
            (if parent
                `(0 0 ,parent-or-display-width ,parent-or-display-height)
              (cdr (assq 'workarea monitor-attributes))))
           ;; The outer size of FRAME.  Needed to calculate the
           ;; margins around the root window's body that have to
           ;; remain untouched by fitting.
           (outer-edges (frame-edges frame 'outer-edges))
           (outer-width (if outer-edges
                            (- (nth 2 outer-edges) (nth 0 outer-edges))
                          ;; A poor guess.
                          (frame-pixel-width frame)))
           (outer-height (if outer-edges
                             (- (nth 3 outer-edges) (nth 1 outer-edges))
                           ;; Another poor guess.
                           (frame-pixel-height frame)))
           ;; The text size of FRAME.  Needed to specify FRAME's
           ;; text size after the root window's body's new sizes have
           ;; been calculated.
           (text-width (frame-text-width frame))
           (text-height (frame-text-height frame))
           ;; WINDOW's body size.
           (body-width (window-body-width window t))
           (body-height (window-body-height window t))
           ;; The difference between FRAME's outer size and WINDOW's
           ;; body size.
           (outer-minus-body-width (- outer-width body-width))
           (outer-minus-body-height (- outer-height body-height))
           ;; The difference between FRAME's text size and WINDOW's
           ;; body size (these values "should" be positive).
           (text-minus-body-width (- text-width body-width))
           (text-minus-body-height (- text-height body-height))
           ;; The current position of FRAME.
           (position (frame-position frame))
           (left (car position))
           (top (cdr position))
           ;; The margins specified for FRAME.  These represent pixel
           ;; offsets from the left, top, right and bottom edge of the
           ;; display or FRAME's parent's native rectangle and have to
           ;; take care of the display's taskbar and other obstacles.
           ;; If they are unspecified, constrain the resulting frame
           ;; to its workarea or the parent frame's native rectangle.
           (margins (or (frame-parameter frame 'fit-frame-to-buffer-margins)
                        fit-frame-to-buffer-margins))
           ;; Convert margins into pixel offsets from the left-top
           ;; corner of FRAME's display or parent.
           (left-margin (if (nth 0 margins)
                            (window--sanitize-margin
                             (nth 0 margins) 0 parent-or-display-width)
                          (nth 0 parent-or-workarea)))
           (top-margin (if (nth 1 margins)
                           (window--sanitize-margin
                            (nth 1 margins) 0 parent-or-display-height)
                         (nth 1 parent-or-workarea)))
           (right-margin (if (nth 2 margins)
                             (- parent-or-display-width
                                (window--sanitize-margin
                                 (nth 2 margins) left-margin
                                 parent-or-display-width))
                           (+ (nth 0 parent-or-workarea)
                              (nth 2 parent-or-workarea))))
           (bottom-margin (if (nth 3 margins)
                              (- parent-or-display-height
                                 (window--sanitize-margin
                                  (nth 3 margins) top-margin
                                  parent-or-display-height))
                            (+ (nth 1 parent-or-workarea)
                               (nth 3 parent-or-workarea))))
           ;; Minimum and maximum sizes specified for FRAME.
           (sizes (or (frame-parameter frame 'fit-frame-to-buffer-sizes)
                      fit-frame-to-buffer-sizes))
           ;; Calculate the minimum and maximum pixel sizes of FRAME
           ;; from the values provided by the MAX-HEIGHT, MIN-HEIGHT,
           ;; MAX-WIDTH and MIN-WIDTH arguments or, if these are nil,
           ;; from those provided by `fit-frame-to-buffer-sizes'.
           (max-height
            (min
             (cond
              ((numberp max-height) (* max-height line-height))
              ((numberp (nth 0 sizes)) (* (nth 0 sizes) line-height))
              (t parent-or-display-height))
             ;; The following is the maximum height that fits into the
             ;; top and bottom margins.
             (max (- bottom-margin top-margin outer-minus-body-height))))
           (min-height
            (cond
             ((numberp min-height) (* min-height line-height))
             ((numberp (nth 1 sizes)) (* (nth 1 sizes) line-height))
             (t (window-min-size window nil nil t))))
           (max-width
            (min
             (cond
              ((numberp max-width) (* max-width char-width))
              ((numberp (nth 2 sizes)) (* (nth 2 sizes) char-width))
              (t parent-or-display-width))
             ;; The following is the maximum width that fits into the
             ;; left and right margins.
             (max (- right-margin left-margin outer-minus-body-width))))
           (min-width
            (cond
             ((numberp min-width) (* min-width char-width))
             ((numberp (nth 3 sizes)) (nth 3 sizes))
             (t (window-min-size window t nil t))))
           ;; Note: Currently, for a new frame the sizes of the header
           ;; and mode line may be estimated incorrectly
           (size
            (window-text-pixel-size window t t max-width max-height))
           (width (max (car size) min-width))
           (height (max (cdr size) min-height)))
      ;; Don't change height or width when the window's size is fixed
      ;; in either direction or ONLY forbids it.
      (cond
       ((or (eq window-size-fixed 'width) (eq only 'vertically))
        (setq width nil))
       ((or (eq window-size-fixed 'height) (eq only 'horizontally))
        (setq height nil)))
      ;; Fit width to constraints.
      (when width
        (unless frame-resize-pixelwise
          ;; Round to character sizes.
          (setq width (* (/ (+ width char-width -1) char-width)
                         char-width)))
        ;; The new outer width (in pixels).
        (setq outer-width (+ width outer-minus-body-width))
        ;; Maybe move FRAME to preserve margins.
        (let ((right (+ left outer-width)))
          (cond
           ((> right right-margin)
            ;; Move frame to left.
            (setq left (max left-margin (- left (- right right-margin)))))
           ((< left left-margin)
            ;; Move frame to right.
            (setq left left-margin)))))
      ;; Fit height to constraints.
      (when height
        (unless frame-resize-pixelwise
          (setq height (* (/ (+ height char-height -1) char-height)
                          char-height)))
        ;; The new outer height.
        (setq outer-height (+ height outer-minus-body-height))
        ;; Preserve margins.
        (let ((bottom (+ top outer-height)))
          (cond
           ((> bottom bottom-margin)
            ;; Move frame up.
            (setq top (max top-margin (- top (- bottom bottom-margin)))))
           ((< top top-margin)
            ;; Move frame down.
            (setq top top-margin)))))
      ;; Apply our changes.
      (setq text-width
            (if width
                (+ width text-minus-body-width)
              (frame-text-width frame)))
      (setq text-height
            (if height
                (+ height text-minus-body-height)
              (frame-text-height frame)))
      (setq text-width (+ text-width statusbar-x-offset))
      (modify-frame-parameters
       frame `((left . ,left) (top . ,top)
               (width . (text-pixels . ,text-width))
               (height . (text-pixels . ,text-height)))))))

(defun statusbar--update-pos ()
  (let ((x-gtk-resize-child-frames 'resize-mode)
        buf-width)
    ;; (message "BEFORE %d" (frame-pixel-width statusbar--frame))
    (statusbar--fit-frame-to-buffer statusbar--frame nil nil nil nil 'horizontally)
    ;;(setq buf-width (+ statusbar-x-offset (frame-pixel-width statusbar--frame)))
    ;; (message "AFTER %d" (frame-pixel-width statusbar--frame))
    )
  (let* ((buf (statusbar--get-buffer))
         ;;(buf-width (frame-pixel-width statusbar--frame))
         (buf-width  (frame-pixel-width statusbar--frame))
         (parent-frame-width (frame-pixel-width statusbar--parent-frame))
         (parent-frame-height (frame-pixel-height statusbar--parent-frame))
         (exwm-systemtray-offset
          (if-let* ((tray-list (and (boundp 'exwm-systemtray--list) exwm-systemtray--list))
                    (icon-size (+ exwm-systemtray--icon-min-size exwm-systemtray-icon-gap))
                    (tray-width (* (length exwm-systemtray--list) icon-size)))
              tray-width
            0))
         (x-offset 0)
         (x-pos (- parent-frame-width buf-width x-offset exwm-systemtray-offset))
         (y-pos -1))
    ;; (setq buf-width (+ 50 buf-width))
    ;;(message "New width %d \n" buf-width)
    (set-frame-position statusbar--frame x-pos y-pos)
    ;; (modify-frame-parameters
    ;;  statusbar--frame `((left ,x-pos)
    ;;                     (top ,y-pos)
    ;;                     (width . (text-pixels . ,buf-width))
    ;;                     (height . (text-pixels . ,buf-width))))
    ))

(defun statusbar--alive (&optional and-visible)
  (and statusbar--frame
       (frame-live-p statusbar--frame)
       (or (not and-visible) (frame-visible-p statusbar--frame))))

(defun statusbar--delete ()
  "Delete statusbar frame and buffer.
This will only delete the frame and *NOT* remove the variable watchers."
  (kill-buffer (statusbar--get-buffer))
  (when (statusbar--alive)
    (delete-frame statusbar--frame)
    (setq statusbar--frame nil)))

(defun statusbar--hide ()
  "Hide statusbar frame."
  (when (statusbar--alive t)
    (make-frame-invisible statusbar--frame)))

(defun statusbar--unhide ()
  (statusbar-refresh)
  (make-frame-visible statusbar--frame))


;;; Public functions

(defun statusbar-refresh (&rest _)
  "Refresh statusbar with new variable values."
  (when (not (statusbar--alive))
    (statusbar--create))
  (when (frame-visible-p statusbar--frame)
    (let (;; TODO: Window and buffer should be specified correctly
          (txt (format-mode-line statusbar-modeline-format))
          (buf (statusbar--get-buffer))
          (buffer-read-only nil)
          (inhibit-read-only t))
      (with-current-buffer buf
        (erase-buffer)
        (insert txt)
        ;;(statusbar--sanitize-mode-line)
        (goto-char (point-min)))
      (statusbar--update-pos)
      )))

;;; statusbar-mode

;;;###autoload
(define-minor-mode statusbar-mode
  "Global minor mode to toggle child frame statusbar."
  :global t
  (if statusbar-mode
      ;; Enable statusbar-mode
      (progn
        ;; When we're in exwm simply use the workspace-switch-hook
        ;; instead of the normal Emacs frame functions/hooks
        ;; (statusbar-hide-on-active-mini t)
        (statusbar--unhide)
        (define-advice force-mode-line-update (:around (orig-fn &rest rest) force-statusbar-update)
          (if statusbar@refresh-on-modeline-update
              (let ((statusbar@refresh-on-modeline-update nil))
                (statusbar-refresh)
                (apply orig-fn rest)))
          (apply orig-fn rest))

        ;; TODO: When displaying the pos-frame, the modeline line changes which forces a change

        ;; (if (boundp 'exwm-workspace-switch-hook)
        ;;     (add-hook 'exwm-workspace-switch-hook #'statusbar-refresh)
        ;;   ;; Check if we're on Emacs 27 where the frame focus functions changed
        ;;   (with-no-warnings
        ;;     (if (not (boundp 'after-focus-change-function))
        ;;         (add-hook 'focus-in-hook #'statusbar-refresh)
        ;;       ;; `focus-in-hook' is obsolete in Emacs 27
        ;;       (defun statusbar--refresh-with-focus-check ()
        ;;         "Like `statusbar-refresh' but check `frame-focus-state' first."
        ;;         (when (frame-focus-state)
        ;;           (statusbar-refresh)))
        ;;       (add-function :after after-focus-change-function #'statusbar--refresh-with-focus-check))))
        )

    (advice-remove #'force-mode-line-update
                   #'force-mode-line-update@force-statusbar-update)

    ;; ;; Disable statusbar-mode
    ;; (if (boundp 'exwm-workspace-switch-hook)
    ;;     (remove-hook 'exwm-workspace-switch-hook #'statusbar-refresh)
    ;;   (with-no-warnings
    ;;     (if (not (boundp 'after-focus-change-function))
    ;;         (remove-hook 'focus-in-hook #'statusbar-refresh)
    ;;       (remove-function after-focus-change-function #'statusbar--refresh-with-focus-check))))
    ;; ;;(statusbar--remove-modeline-vars)
    (statusbar-hide-on-active-minibuffer nil)
    (statusbar--delete)))

(defun statusbar-hide-on-active-minibuffer (hide)
  (if hide
      (progn
        (add-hook #'minibuffer-setup-hook #'statusbar--hide)
        ;; TODO: minibuffer-exit-hook keeps changing for some reason
        (add-hook #'minibuffer-exit-hook #'statusbar--unhide))
    (remove-hook #'minibuffer-setup-hook #'statusbar--hide)
    (remove-hook #'minibuffer-exit-hook #'statusbar--unhide)))

(defun statusbar--sanitize-mode-line ()
  (let (match map)
    (goto-char (point-min))
    (while (setq match (text-property-search-forward 'local-map))
      (setq map (prop-match-value match))
      (when (eq (caadr map) 'mode-line)
        (add-text-properties
         (prop-match-beginning match)
         (prop-match-end match)
         `(local-map ,(cdadr map)))))))

(defvar statusbar--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] ;; [mouse-2]
      (lambda () (interactive) (message "CustomAction3"))
      )
    map))

(define-derived-mode statusbar--mode special-mode "statusbar"
  "Major mode for statubar buffers"
  (setq truncate-lines nil
        word-wrap nil)
  (fringe-mode 0)
  ;; (transient-mark-mode -1)
  ;;(toggle-word-wrap -1)
  ;;(toggle-truncate-lines -1)
  ;;(setq buffer-read-only t)
  ;;
  )


;; (statusbar-mode -1)

(provide 'statusbar)
;;; statusbar.el ends here
