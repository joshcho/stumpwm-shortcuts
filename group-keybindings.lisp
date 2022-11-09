(in-package :stumpwm)
(defmacro define-key-in-group (map key cmd group)
  "Define keybinding for particular group. TODO: fix it destroying any regular bindings."
  `(progn
     (unless (get 'map-bindings ,group)
       (setf (get 'map-bindings ,group) (make-hash-table)))
     (let ((bindings (get ',map (get 'map-bindings ,group))))
       (setf (gethash ',map (get 'map-bindings ,group))
             (append
              (delete ,key bindings
                      :test #'equalp)
              (list (list ,key ,cmd)))))

     (when (equalp ,group
                   (current-group))
       (define-key ,map ,key ,cmd))))

(defun unapply-keys-in-group (group)
  (loop for map in '(*root-map* *top-map*)
        do (loop for (key _) in (gethash map
                                         (progn
                                           (unless (get 'map-bindings group)
                                             (setf (get 'map-bindings group)
                                                   (make-hash-table)))
                                           (get 'map-bindings group)))
                 do (define-key (case map
                                  ('*root-map* *root-map*)
                                  ('*top-map* *top-map*))
                      key
                      nil))))

(defun apply-keys-in-group (group)
  (loop for map in '(*root-map* *top-map* *group-map*) ; add other maps
        do (loop for (key cmd) in (gethash map
                                           (progn
                                             (unless (get 'map-bindings group)
                                               (setf (get 'map-bindings group)
                                                     (make-hash-table)))
                                             (get 'map-bindings group)))
                 do (define-key (case map
                                  ('*root-map* *root-map*)
                                  ('*top-map* *top-map*))
                      key
                      cmd))))

(progn
  ;; redefinition to support group keybindings
  (defcommand gnew (name) ((:string "Group name: "))
    (unless name
      (throw 'error :abort))
    (unapply-keys-in-group (current-group))
    (add-group (current-screen) name)
    (apply-keys-in-group (current-group))
    )

  (defcommand gnext () ()
    (unapply-keys-in-group (current-group))
    (group-forward (current-group)
                   (sort-groups (current-screen)))
    (apply-keys-in-group (current-group)))

  (defcommand gprev () ()
    (unapply-keys-in-group (current-group))
    (group-forward (current-group)
                   (reverse (sort-groups (current-screen))))
    (apply-keys-in-group (current-group)))

  (defcommand gnext-with-window () ()
    (unapply-keys-in-group (current-group))
    (group-forward-with-window (current-group)
                               (sort-groups (current-screen)))
    (apply-keys-in-group (current-group)))

  (defcommand gprev-with-window () ()
    (unapply-keys-in-group (current-group))
    (group-forward-with-window (current-group)
                               (reverse (sort-groups (current-screen))))
    (apply-keys-in-group (current-group))))
