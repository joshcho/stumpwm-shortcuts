(in-package :stumpwm)

(export '(toggle-window show-shortcuts add-shortcut add-shortcut-float))
;; requires group-mappings

;; There seems to be a bug with height

;; window presets
(defstruct window-preset
  width
  height
  x
  y)
(defun extract-window-preset (win)
  "Return its window preset if float window, NIL otherwise."
  (if (float-window-p win)
      (make-window-preset
       :width (float-window-last-width win)
       :height (float-window-last-height win)
       :x (float-window-last-x win)
       :y (float-window-last-y win))
      (let ((f (window-frame win)))
        (make-window-preset
         :width (- (frame-width f) 4)
         :height (frame-height f)
         :x (frame-x f)
         :y (frame-y f)))))

(defparameter *window-presets-file* "~/.stumpwm.d/window-presets.lisp")
(defun all-window-presets ()
  (with-open-file (s *window-presets-file*
                     :direction :input)
    (loop
      for labeled-preset = (read s nil)
      while labeled-preset
      collect labeled-preset)))

(defcommand save-window-preset (label)
    ((:string "Enter Label: "))
  (let ((preset (extract-window-preset (current-window))))
    (when label
      (with-open-file (s *window-presets-file*
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
        (loop for (l . p) in (all-window-presets)
              when (not (equal l label))
                do (format s "~s~%" (cons l p))
              finally (format s "~s~%" (cons label preset)))))))

(define-key *root-map* (kbd "C-s") "save-window-preset")

(defun window-preset (label)
  (with-open-file (s *window-presets-file*
                     :direction :input)
    (loop
      for (l . preset) = (read s nil)
      while l
      if (equal l label)
        return preset)))

(defun apply-window-preset (window preset)
  (if (float-window-p window)
      (float-window-move-resize
       window
       :x (window-preset-x preset)
       :y (window-preset-y preset)
       :width (window-preset-width preset)
       :height (window-preset-height preset))
      (message "Must apply preset to a float-window.")))

;; window shortcuts IDEA: floating frames, not floating windows?
(defun window-shortcuts (group)
  (unless (get 'window-shortcuts group)
    (setf (get 'window-shortcuts group) (make-hash-table :test #'equal)))
  (get 'window-shortcuts group))

(defun current-window-shortcuts ()
  (window-shortcuts (current-group)))

(defun window-by-shortcut (shortcut)
  (gethash shortcut (current-window-shortcuts)))

(defcommand toggle-window (shortcut)
    ((:string "Enter Shortcut: "))
  (if-let (win (window-by-shortcut shortcut))
    (if (equal win (current-window))
        (progn
          (setf (group-on-top-windows (current-group))
                (remove win (group-on-top-windows (current-group))))
          (hide-window (current-window)))
        (if (member win (group-windows (current-group)))
            (progn
              (when (not (find win (group-on-top-windows (current-group))))
                ;; putting win at the end makes more sense, but there
                ;; are rendering issuse
                (push win (group-on-top-windows (current-group))))
              ;; (when (and (not (find (current-window) (group-on-top-windows (current-group))
              ;;                       :test #'equalp))
              ;;            (typep (current-window) 'float-window))
              ;;   (hide-window (current-window)))
              (focus-window win))
            (progn
              (remove-shortcut-internal shortcut)
              (send-fake-key (current-window) (kbd shortcut)))))
    (message "~a is not a shortcut (yet)." shortcut)))

(defun current-shortcuts ()
  (hash-table-keys (current-window-shortcuts)))

(defcommand delete-shortcut () ()
  "Delete shortcut in this group."
  (let ((shortcut (car (select-from-menu (current-screen)
                                         (current-shortcuts)
                                         "Choose Shortcut: "))))
    (focus-window (window-by-shortcut shortcut)) ; for convenience, show the corresponding shortcut
    (remove-shortcut-internal shortcut))
  ;; TODO: ask y-or-no on whether to remove the corresponding window as well
  )

(defcommand show-shortcuts () ()
  (message "~{~a~^, ~}" (current-shortcuts)))

(define-key *root-map* (kbd "D") "delete-shortcut")
(define-key *root-map* (kbd "u") "unfloat-this")

(defun add-shortcut-internal (shortcut win)
  ;; TODO better name
  (setf (gethash shortcut (current-window-shortcuts)) win)
  (define-key-in-group *top-map* (kbd shortcut) (format nil "toggle-window ~a" shortcut) (current-group)))

(defun remove-shortcut-internal (shortcut)
  (define-key-in-group *top-map* (kbd shortcut) nil (current-group))
  (remhash shortcut (current-window-shortcuts)))

(defcommand toggle-shortcut (shortcut)
    ((:string "Shortcut: "))
  (let ((win (current-window)))
    (if (eql win (window-by-shortcut shortcut))
        (progn
          (unfloat-window win (current-group))
          (remove-shortcut-internal shortcut))
        (let* ((labeled-presets (all-window-presets))
               (menu-selection (select-from-menu (current-screen)
                                                 (append
                                                  ;; (list "Unbind")
                                                  (mapcar #'car labeled-presets)
                                                  (list "Unchanged" "Don't float"))
                                                 "Choose Preset:"))
               (action-name (car menu-selection)))
          (when menu-selection
            (switch (action-name :test #'equal)
              ("Don't float" (add-shortcut-internal shortcut win))
              ;; ("Unbind" (progn
              ;;             (unfloat-this)
              ;;             (remove-shortcut-internal shortcut)))
              (t (let* ((preset-name action-name)
                        (preset (if (equal preset-name "Unchanged")
                                    (extract-window-preset win)
                                    (window-preset preset-name))))
                   (progn
                     (when-let (shortcut-win (window-by-shortcut shortcut))
                       (progn
                         (unfloat-window shortcut-win (current-group))
                         (remove-shortcut-internal shortcut)))
                     (add-shortcut-internal shortcut win))
                   (when preset
                     (unless (typep win 'float-window)
                       (float-window win (window-group win)))
                     (apply-window-preset win preset))))))))))

(define-key-n *root-map*
    (("@" . "toggle-shortcut @")
     ("#" . "toggle-shortcut #")
     ("$" . "toggle-shortcut $")
     ("!" . "toggle-shortcut !")
     ;; F1 and F4 don't work on advantage ??
     ("F2" . "toggle-shortcut F2")
     ("F3" . "toggle-shortcut F3")
     ("F5" . "toggle-shortcut F5")
     ("F9" . "toggle-shortcut F9")
     ("F10" . "toggle-shortcut F10")
     ("F11" . "toggle-shortcut F11")
     ("F12" . "toggle-shortcut F12")
     ("Home" . "toggle-shortcut Home")
     ("SunPageUp" . "toggle-shortcut SunPageUp")))
