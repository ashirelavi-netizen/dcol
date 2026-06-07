;;; ============================================================
;;;  acaddoc.lsp  -  טוען בלבד (loader)
;;;
;;;  קובץ קטן זה יושב בתיקיית ה-Support של AutoCAD ונטען
;;;  אוטומטית בכל פתיחת שרטוט. תפקידו: לטעון את כל תוספי הפרויקט.
;;; ============================================================

(setq *DCOL-PATH* "C:\\Users\\Owner\\Desktop\\claude\\Acad\\files\\DCOL.lsp")
(setq *NUM-PATH*  "C:\\Users\\Owner\\Desktop\\claude\\Acad\\files\\NUM.lsp")

(if (findfile *DCOL-PATH*)
  (load *DCOL-PATH*)
  (princ (strcat "\n[acaddoc] DCOL.lsp לא נמצא בנתיב: " *DCOL-PATH*))
)

(if (findfile *NUM-PATH*)
  (load *NUM-PATH*)
  (princ (strcat "\n[acaddoc] NUM.lsp לא נמצא בנתיב: " *NUM-PATH*))
)

(princ)
