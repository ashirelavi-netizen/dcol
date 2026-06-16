;;; TEST_TOOLBAR.lsp — בדיקת תמיכה ביצירת סרגל כלים דרך קוד
;;; הרץ: (load "נתיב/TEST_TOOLBAR.lsp") ואז הקלד TESTBAR

(defun c:TESTBAR ( / acad grps grp tbars tb )
  (vl-load-com)
  (princ "\n--- בדיקת תמיכה בסרגל כלים ---")

  ;; שלב 1
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object nil))
  (if (vl-catch-all-error-p acad)
    (progn (princ "\n[X] שלב 1: גישה לאפליקציה - נכשל") (princ) (exit))
    (princ "\n[V] שלב 1: גישה לאפליקציה - OK"))

  ;; שלב 2
  (setq grps (vl-catch-all-apply 'vla-get-menugroups (list acad)))
  (if (vl-catch-all-error-p grps)
    (progn (princ "\n[X] שלב 2: menugroups - נכשל") (princ) (exit))
    (princ "\n[V] שלב 2: menugroups - OK"))

  ;; שלב 3
  (setq grp (vl-catch-all-apply 'vla-item (list grps 0)))
  (if (vl-catch-all-error-p grp)
    (progn (princ "\n[X] שלב 3: menugroup ראשון - נכשל") (princ) (exit))
    (princ (strcat "\n[V] שלב 3: menugroup - " (vla-get-name grp) " - OK")))

  ;; שלב 4
  (setq tbars (vl-catch-all-apply 'vla-get-toolbars (list grp)))
  (if (vl-catch-all-error-p tbars)
    (progn (princ "\n[X] שלב 4: toolbars - נכשל") (princ) (exit))
    (princ "\n[V] שלב 4: toolbars - OK"))

  ;; שלב 5 — יצירה ומחיקה מיידית
  (setq tb (vl-catch-all-apply 'vla-add (list tbars "TEST_DCOL_BAR")))
  (if (vl-catch-all-error-p tb)
    (progn (princ "\n[X] שלב 5: יצירת toolbar - נכשל") (princ) (exit))
    (progn
      (princ "\n[V] שלב 5: יצירת toolbar - OK")
      (vl-catch-all-apply 'vla-delete (list tb))
      (princ "\n\nתוצאה: ZCAD תומך! ניתן להמשיך.")))

  (princ)
)

(princ "\nהקלד TESTBAR להרצת הבדיקה.")
(princ)
