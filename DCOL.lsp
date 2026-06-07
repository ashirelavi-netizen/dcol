;;; ============================================================
;;;  DCOL.lsp  -  תוסף ציור צורה כפולה עם האצ'
;;;  פקודות: DCOL , DCOLUP , DCOLSET
;;;
;;;  קובץ זה מכיל את כל קוד התוסף.
;;;  נטען ע"י acaddoc.lsp (loader) שיושב בתיקיית Support.
;;;  ראה DCOL_summary_v2.md לפרטי הפרויקט המלאים.
;;; ============================================================

(vl-load-com)

;;; ----- שמות קבועים -----
(setq *COL-DICT*  "COL_SETTINGS")   ; שם המילון לשמירת הגדרות
(setq *COL-XAPP*  "COL_PAIR")       ; שם אפליקציית XData לזיהוי זוגות
(setq *COL-VER*   "1")              ; גרסת מבנה XData

;;; ============================================================
;;;  עזר: רישום אפליקציית XData
;;; ============================================================
(defun col:regapp ()
  (if (not (tblsearch "APPID" *COL-XAPP*))
    (regapp *COL-XAPP*))
)

;;; ============================================================
;;;  הגדרות: קריאה / כתיבה במילון של הקובץ
;;;  ההגדרות נשמרות כרשימת מחרוזות ב-XRecord.
;;;  סדר השדות:
;;;   0 lineLayerSrc   1 hatchLayerSrc
;;;   2 lineLayerCpy   3 hatchLayerCpy
;;;   4 patSrc 5 scaleSrc 6 angSrc
;;;   7 patCpy 8 scaleCpy 9 angCpy
;;;  10 dX 11 dY 12 dZ
;;;  13 activeShape ("P"/"C"/"E")
;;; ============================================================

(defun col:get-settings ( / dicts d xrec data res )
  (setq dicts (namedobjdict))
  (setq d (dictsearch dicts *COL-DICT*))
  (if d
    (progn
      (setq xrec (cdr (assoc -1 d)))
      (setq data (entget xrec))
      (setq res '())
      (foreach pair data
        (if (= 1 (car pair))           ; קודי 1 = מחרוזות שנשמרו
          (setq res (cons (cdr pair) res))))
      (reverse res))
    nil)
)

(defun col:put-settings ( lst / dicts xrec data )
  ;; lst = רשימה של 14 מחרוזות
  (setq dicts (namedobjdict))
  ;; הסר מילון קיים אם יש
  (if (dictsearch dicts *COL-DICT*)
    (dictremove dicts *COL-DICT*))
  ;; בנה XRecord חדש
  (setq data (list '(0 . "XRECORD") '(100 . "AcDbXrecord")))
  (foreach s lst
    (setq data (append data (list (cons 1 s)))))
  (setq xrec (entmakex data))
  (dictadd dicts *COL-DICT* xrec)
  lst
)

;;; ערך ברירת מחדל אם אין הגדרות עדיין
(defun col:default-settings ()
  (list
    "0" "0"      ; שכבות מקור (0 = שכבת ברירת מחדל)
    "0" "0"      ; שכבות עותק
    "ANSI31" "1.0" "0.0"   ; האצ' מקור
    "ANSI31" "1.0" "0.0"   ; האצ' עותק
    "500.0" "0.0" "0.0"    ; הזחה dX dY dZ
    "P"          ; צורה פעילה  [13]
    "0"          ; שכבת קו ציר [14]
    "0" "0" "0"  ; Rectangle/Circle/Ellipse axis [15][16][17]
    "0"          ; שכבת קו חיצוני Nested [18]
    "0"          ; שכבת האצ' Nested [19]
    "ANSI31"     ; Pattern Nested [20]
    "1.0"        ; Scale Nested [21]
    "0.0"        ; Angle Nested [22]
    "0")         ; שכבת קו פנימי Nested [23]
)

;;; ============================================================
;;;  רשימת שכבות קיימות בקובץ
;;; ============================================================
(defun col:layer-list ( / l name )
  (setq l '())
  (setq name (tblnext "LAYER" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "LAYER" nil)))
  (acad_strlsort l)
)

;;; ============================================================
;;;  חלון הגדרות (DCL)
;;;  נכתב לקובץ זמני, נטען, מוצג, נמחק.
;;; ============================================================
(defun col:write-dcl ( / f path )
  (setq path (vl-filename-mktemp "col" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "col_dlg : dialog {" f)
  (write-line "  label = \"הגדרות COL/WALL & Geometry\";" f)
  ;; ===== קטע COL/WALL =====
  (write-line "  : boxed_column { label = \"COL / WALL\";" f)
  (write-line "    : row {" f)
  (write-line "      : boxed_column { label = \"מקור\";" f)
  (write-line "        : popup_list { key=\"ll_src\"; label=\"שכבת קו\"; }" f)
  (write-line "        : popup_list { key=\"hl_src\"; label=\"שכבת האצ'\"; }" f)
  (write-line "        : popup_list { key=\"pat_src\"; label=\"Pattern\"; }" f)
  (write-line "        : edit_box { key=\"sc_src\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "        : edit_box { key=\"an_src\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "      : boxed_column { label = \"עותק\";" f)
  (write-line "        : popup_list { key=\"ll_cpy\"; label=\"שכבת קו\"; }" f)
  (write-line "        : popup_list { key=\"hl_cpy\"; label=\"שכבת האצ'\"; }" f)
  (write-line "        : popup_list { key=\"pat_cpy\"; label=\"Pattern\"; }" f)
  (write-line "        : edit_box { key=\"sc_cpy\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "        : edit_box { key=\"an_cpy\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "    }" f)
  (write-line "    : boxed_row { label = \"הזחת עותק\";" f)
  (write-line "      : edit_box { key=\"dx\"; label=\"dX\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"dy\"; label=\"dY\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"dz\"; label=\"dZ\"; edit_width=8; } }" f)
  (write-line "    : boxed_column { label = \"קו ציר\";" f)
  (write-line "      : popup_list { key=\"ax_lay\"; label=\"שכבה\"; }" f)
  (write-line "      : toggle { key=\"ax_rect\"; label=\"Rectangle\"; }" f)
  (write-line "      : toggle { key=\"ax_circ\"; label=\"Circle\"; }" f)
  (write-line "      : toggle { key=\"ax_ell\";  label=\"Ellipse\"; } }" f)
  (write-line "  }" f)
  ;; ===== קטע Geometry =====
  (write-line "  : boxed_column { label = \"Geometry\";" f)
  (write-line "    : row {" f)
  (write-line "      : popup_list { key=\"nl_lay\";    label=\"קו חיצוני\"; }" f)
  (write-line "      : popup_list { key=\"nl_in_lay\"; label=\"קו פנימי\"; }" f)
  (write-line "      : popup_list { key=\"nh_lay\";    label=\"שכבת האצ'\"; } }" f)
  (write-line "    : row {" f)
  (write-line "      : popup_list { key=\"n_pat\"; label=\"Pattern\"; }" f)
  (write-line "      : edit_box { key=\"n_sc\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"n_an\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "  }" f)
  (write-line "  : button { key=\"newlay\"; label=\"שכבה חדשה...\"; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

;; רשימת patterns בסיסית (אפשר להרחיב). אלה patterns סטנדרטיים נפוצים.
(defun col:pattern-list ()
  (list "SOLID" "ANSI31" "ANSI32" "ANSI33" "ANSI37"
        "NET" "NET3" "GRAVEL" "EARTH" "DOTS"
        "GRASS" "HONEY" "BRICK" "AR-CONC" "AR-SAND")
)

(defun col:dlg ( cur / dclid path lays pats res
                       ll_src hl_src ll_cpy hl_cpy
                       pat_src pat_cpy ax_lay )
  (setq path (col:write-dcl))
  (setq lays (col:layer-list))
  (setq pats (col:pattern-list))
  (setq res nil)
  (while
    (progn
      (setq dclid (load_dialog path))
      (if (not (new_dialog "col_dlg" dclid)) (exit))
      ;; מילוי רשימות
      (start_list "ll_src") (mapcar 'add_list lays) (end_list)
      (start_list "hl_src") (mapcar 'add_list lays) (end_list)
      (start_list "ll_cpy") (mapcar 'add_list lays) (end_list)
      (start_list "hl_cpy") (mapcar 'add_list lays) (end_list)
      (start_list "pat_src") (mapcar 'add_list pats) (end_list)
      (start_list "pat_cpy") (mapcar 'add_list pats) (end_list)
      (start_list "ax_lay")  (mapcar 'add_list lays) (end_list)
      (start_list "nl_lay")    (mapcar 'add_list lays) (end_list)
      (start_list "nl_in_lay") (mapcar 'add_list lays) (end_list)
      (start_list "nh_lay")    (mapcar 'add_list lays) (end_list)
      (start_list "n_pat")     (mapcar 'add_list pats) (end_list)
      ;; ערכים נוכחיים
      (set_tile "ll_src" (itoa (col:idx (nth 0 cur) lays)))
      (set_tile "hl_src" (itoa (col:idx (nth 1 cur) lays)))
      (set_tile "ll_cpy" (itoa (col:idx (nth 2 cur) lays)))
      (set_tile "hl_cpy" (itoa (col:idx (nth 3 cur) lays)))
      (set_tile "pat_src" (itoa (col:idx (nth 4 cur) pats)))
      (set_tile "sc_src" (nth 5 cur))
      (set_tile "an_src" (nth 6 cur))
      (set_tile "pat_cpy" (itoa (col:idx (nth 7 cur) pats)))
      (set_tile "sc_cpy" (nth 8 cur))
      (set_tile "an_cpy" (nth 9 cur))
      (set_tile "dx" (nth 10 cur))
      (set_tile "dy" (nth 11 cur))
      (set_tile "dz" (nth 12 cur))
      (set_tile "ax_lay"  (itoa (col:idx (nth 14 cur) lays)))
      (set_tile "ax_rect" (nth 15 cur))
      (set_tile "ax_circ" (nth 16 cur))
      (set_tile "ax_ell"  (nth 17 cur))
      (set_tile "nl_lay"    (itoa (col:idx (nth 18 cur) lays)))
      (set_tile "nh_lay"    (itoa (col:idx (nth 19 cur) lays)))
      (set_tile "n_pat"     (itoa (col:idx (nth 20 cur) pats)))
      (set_tile "n_sc"      (nth 21 cur))
      (set_tile "n_an"      (nth 22 cur))
      (set_tile "nl_in_lay" (itoa (col:idx (nth 23 cur) lays)))
      ;; כפתור שכבה חדשה -> מסמן יציאה לרענון
      (action_tile "newlay" "(setq res \"NEWLAYER\")(done_dialog 2)")
      (action_tile "accept"
        (strcat
          "(setq res (list"
          " (nth (atoi (get_tile \"ll_src\")) lays)"
          " (nth (atoi (get_tile \"hl_src\")) lays)"
          " (nth (atoi (get_tile \"ll_cpy\")) lays)"
          " (nth (atoi (get_tile \"hl_cpy\")) lays)"
          " (nth (atoi (get_tile \"pat_src\")) pats)"
          " (get_tile \"sc_src\") (get_tile \"an_src\")"
          " (nth (atoi (get_tile \"pat_cpy\")) pats)"
          " (get_tile \"sc_cpy\") (get_tile \"an_cpy\")"
          " (get_tile \"dx\") (get_tile \"dy\") (get_tile \"dz\")"
          " \"" (nth 13 cur) "\""
          " (nth (atoi (get_tile \"ax_lay\")) lays)"
          " (get_tile \"ax_rect\") (get_tile \"ax_circ\") (get_tile \"ax_ell\")"
          " (nth (atoi (get_tile \"nl_lay\")) lays)"
          " (nth (atoi (get_tile \"nh_lay\")) lays)"
          " (nth (atoi (get_tile \"n_pat\")) pats)"
          " (get_tile \"n_sc\") (get_tile \"n_an\")"
          " (nth (atoi (get_tile \"nl_in_lay\")) lays)))"
          "(done_dialog 1)"))
      (setq result (start_dialog))
      (unload_dialog dclid)
      ;; אם נלחץ "שכבה חדשה" -> סגור הגדרות, פתח מנהל שכבות,
      ;; המתן ל-Enter מהמשתמש, רענן רשימה, חזור לדיאלוג
      (if (= res "NEWLAYER")
        (progn
          (setq res nil)
          ;; נסה CLASSICLAYER; אם לא קיים — LAYER
          (if (vl-catch-all-error-p
                (vl-catch-all-apply 'command (list "_.CLASSICLAYER")))
            (command "_.LAYER"))
          ;; המתן לסיום: המשתמש לוחץ Enter בשורת הפקודה אחרי יצירת השכבות
          (getstring "\n[DCOL] סיימת ליצור שכבות? לחץ Enter להמשך: ")
          (setq lays (col:layer-list))
          t)   ; חזור ללולאה — ההגדרות ייפתחו מחדש
        nil))   ; צא מהלולאה
  )
  (vl-file-delete path)
  res
)

;; אינדקס פריט ברשימה (0 אם לא נמצא)
(defun col:idx ( item lst / i found )
  (setq i 0 found 0)
  (foreach x lst
    (if (= x item) (setq found i))
    (setq i (1+ i)))
  found
)

;;; ============================================================
;;;  ודא שיש הגדרות; אם לא -> פתח דיאלוג (פעם ראשונה בקובץ)
;;; ============================================================
(defun col:ensure-settings ( / s def )
  (setq s (col:get-settings))
  (if (not s)
    (progn
      (princ "\nפעם ראשונה בקובץ - אנא הגדר את הפרמטרים.")
      (setq s (col:dlg (col:default-settings)))
      (if s (col:put-settings s)
            (setq s (col:put-settings (col:default-settings)))))
    ;; תאימות לאחור: אם יש פחות מ-24 שדות, השלם מברירת מחדל
    (if (< (length s) 24)
      (progn
        (setq def (col:default-settings))
        (while (< (length s) 24)
          (setq s (append s (list (nth (length s) def)))))
        (col:put-settings s))))
  s
)

;;; ============================================================
;;;  ציור צורה - JIG פשוט באמצעות GRREAD
;;;  מחזיר רשימת נקודות / פרמטרים של הצורה.
;;; ============================================================

;; פוליגון זמני פתוח (לתצוגה ו-osnap בזמן הציור)
(defun col:mk-pline-open ( pts / data ) ; פוליגון זמני תמיד על שכבה 0
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   '(8 . "0") '(100 . "AcDbPolyline")
                   (cons 90 (length pts)) '(70 . 0)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p)))))))
  (entmakex data)
)

;; Polygon: אוסף נקודות ומציג פוליגון זמני אמיתי לאורך הציור.
;; הפוליגון הזמני מאפשר osnap מלא ונעיצה לנקודות קודמות.
(defun col:draw-poly ( / pts pt tmp )
  (setq pts '() tmp nil)
  (setq pt (getpoint "\nנקודה ראשונה: "))
  (if pt
    (progn
      (setq pts (list pt))
      (while
        (progn
          (setq pt (getpoint (last pts) "\nנקודה הבאה (Enter לסיום): "))
          (if pt
            (progn
              (setq pts (append pts (list pt)))
              (if tmp (entdel tmp))
              (setq tmp (col:mk-pline-open pts))
              t)
            nil)))
      (if tmp (entdel tmp))
      (redraw)
      pts)
    (progn (redraw) nil))
)

;; Rectangle: שתי פינות אלכסוניות (getcorner מציג מלבן גומי)
(defun col:draw-rect ( / p1 p2 )
  (setq p1 (getpoint "\nפינה ראשונה: "))
  (if p1
    (progn
      (setq p2 (getcorner p1 "\nפינה אלכסונית: "))
      (if p2 (list p1 p2) nil))
    nil)
)

;; יצירת מלבן כ-LWPOLYLINE סגור משתי פינות אלכסוניות
(defun col:mk-rect ( p1 p2 lay / x1 y1 z1 x2 y2 )
  (setq x1 (car p1) y1 (cadr p1) z1 (caddr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (col:mk-pline
    (list p1 (list x2 y1 z1) p2 (list x1 y2 z1))
    lay)
)

;; Circle: מרכז + רדיוס עם תצוגה חיה
(defun col:draw-circle ( / cen gr tmp r )
  (setq cen (getpoint "\nמרכז העיגול: "))
  (if (not cen) nil
    (progn
      (setq tmp nil)
      (princ "\nרדיוס: ")
      (while
        (progn
          (setq gr (grread t 4 0))
          (cond
            ((= (car gr) 5)   ; תנועת עכבר
             (setq r (distance cen (cadr gr)))
             (if tmp (entdel tmp))
             (setq tmp (entmakex (list '(0 . "CIRCLE") '(8 . "0")
                                       (cons 10 cen) (cons 40 r))))
             t)
            ((= (car gr) 3)   ; לחיצת שמאל
             (setq r (distance cen (cadr gr)))
             (if tmp (entdel tmp))
             nil)
            ((= (car gr) 2)   ; הקלדה
             (if tmp (entdel tmp))
             (setq tmp nil)
             (setq r (getdist cen ""))
             nil)
            (t t))))
      (redraw)
      (if r (list cen r) nil)))
)

;; Ellipse: קצה ראשון -> קצה שני -> רוחב מהמרכז, עם תצוגה חיה
(defun col:draw-ellipse ( / ep1 ep2 cen width p2 gr tmp maj ratio )
  (setq ep1 (getpoint "\nקצה ראשון של ציר ראשי: "))
  (if (not ep1) nil
    (progn
      ;; שלב 1: קצה שני עם תצוגה חיה של הציר הראשי
      (setq tmp nil ep2 nil)
      (princ "\nקצה שני של ציר ראשי: ")
      (while
        (progn
          (setq gr (grread t 4 0))
          (cond
            ((= (car gr) 5)
             (if tmp (entdel tmp))
             (setq tmp (entmakex (list '(0 . "LINE") '(8 . "0")
                                       (cons 10 ep1) (cons 11 (cadr gr)))))
             t)
            ((= (car gr) 3)
             (setq ep2 (cadr gr))
             (if tmp (entdel tmp))
             nil)
            ((= (car gr) 2)
             (if tmp (entdel tmp))
             (setq tmp nil)
             (setq ep2 (getpoint ep1 ""))
             nil)
            (t t))))
      (if (not ep2) (progn (redraw) nil)
        (progn
          (setq cen (list (/ (+ (car ep1) (car ep2)) 2.0)
                          (/ (+ (cadr ep1) (cadr ep2)) 2.0)
                          (/ (+ (caddr ep1) (caddr ep2)) 2.0)))
          ;; שלב 2: רוחב עם תצוגה חיה של האליפסה
          (setq tmp nil width nil)
          (princ "\nרוחב (חצי ציר משני): ")
          (while
            (progn
              (setq gr (grread t 4 0))
              (cond
                ((= (car gr) 5)
                 (setq width (distance cen (cadr gr)))
                 (if (> width 0)
                   (progn
                     (if tmp (entdel tmp))
                     (setq maj (mapcar '- ep1 cen))
                     (setq ratio (/ width (distance cen ep1)))
                     (setq tmp (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                                               '(8 . "0") '(100 . "AcDbEllipse")
                                               (cons 10 cen) (cons 11 maj)
                                               (cons 40 ratio) '(41 . 0.0) (cons 42 (* 2 pi)))))))
                 t)
                ((= (car gr) 3)
                 (setq width (distance cen (cadr gr)))
                 (if tmp (entdel tmp))
                 nil)
                ((= (car gr) 2)
                 (if tmp (entdel tmp))
                 (setq tmp nil)
                 (setq width (getdist cen ""))
                 nil)
                (t t))))
          (redraw)
          (if (not width) nil
            (progn
              (setq p2 (list (car cen) (+ (cadr cen) width) (caddr cen)))
              (list cen ep1 p2)))))))
)

;;; ============================================================
;;;  יצירת אובייקטים בפועל (קו + האצ') + עותק
;;; ============================================================

;; וקטור הזחה מההגדרות
(defun col:disp ( s )
  (list (atof (nth 10 s)) (atof (nth 11 s)) (atof (nth 12 s)))
)

;; הוסף XData עם גרסה, pair-id ותפקיד
(defun col:tag ( ent pid role )
  (entmod
    (append (entget ent)
      (list (list -3
        (list *COL-XAPP*
          (cons 1000 *COL-VER*)
          (cons 1000 pid)
          (cons 1000 role))))))
)

;; צור polyline סגור משכבת lay מנקודות pts; מחזיר ename
(defun col:mk-pline ( pts lay / data )
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   (cons 8 lay) '(100 . "AcDbPolyline")
                   (cons 90 (length pts)) '(70 . 1)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p)))))))
  (entmakex data)
)

;; צור עיגול
(defun col:mk-circle ( cen r lay )
  (entmakex (list '(0 . "CIRCLE") (cons 8 lay)
                  (cons 10 cen) (cons 40 r)))
)

;; מוצא האצ' שנוצר אחרי before-ent ומחזיר אותו
(defun col:find-new-hatch ( before-ent lay / scan h ed )
  (setq h nil)
  (setq scan (if before-ent (entnext before-ent) (entnext)))
  (while (and scan (not h))
    (if (and (entget scan)
             (= "HATCH" (cdr (assoc 0 (entget scan)))))
      (setq h scan))
    (setq scan (entnext scan)))
  (if h
    (progn
      (setq ed (entget h))
      ;; שכבה נכונה
      (if (not (equal lay (cdr (assoc 8 ed))))
        (setq ed (subst (cons 8 lay) (assoc 8 ed) ed)))
      ;; צבע BYLAYER (256) — לא צבע ידני
      (if (assoc 62 ed)
        (setq ed (subst (cons 62 256) (assoc 62 ed) ed))
        (setq ed (append ed (list (cons 62 256)))))
      (entmod ed)
      (entupd h)
      h)
    nil)
)

;; האצ' על גבול שנבחר — _LA מגדיר שכבה בתוך פקודת HATCH
(defun col:mk-hatch ( ent lay pat sc ang / ss before )
  (setvar "HPNAME" pat)
  (setvar "HPSCALE" sc)
  (setvar "HPANG" ang)
  (setq ss (ssadd ent (ssadd)))
  (setq before (entlast))
  (command "_.-HATCH" "_LA" lay "_S" ss "" "")
  (col:find-new-hatch before lay)
)

;;; ============================================================
;;;  עזר: העתק ישות והזח אותה לשכבה אחרת; מחזיר ename של העותק
;;; ============================================================
(defun col:copy-and-move ( ent disp lay / dx dy dz ed )
  (setq dx (car disp) dy (cadr disp) dz (caddr disp))
  (command "_.COPY" ent "" "0,0,0"
           (strcat (rtos dx 2 6) "," (rtos dy 2 6) "," (rtos dz 2 6)))
  (setq ed (entget (entlast)))
  (entmod (subst (cons 8 lay) (assoc 8 ed) ed))
  (entupd (entlast))
  (entlast)
)

;;; ============================================================
;;;  מצב Attach - שיוך האצ' חיצוני
;;; ============================================================
(defun col:do-attach ( s disp pid / ss i ent all hatches lines
                                    srcHatch srcLines cpyLines cpyHatch
                                    assoc? outer inners )
  ;; בחירה אחת: האצ' + קווי מתאר יחד
  (setq ss (ssget "\nבחר האצ' וקווי מתאר (בחירה חופשית): "))
  (if (not ss) (progn (princ "\nבוטל.") (exit)))
  ;; הפרד האצ'ים מקווים
  (setq all '() i 0)
  (while (< i (sslength ss))
    (setq all (append all (list (ssname ss i))))
    (setq i (1+ i)))
  (setq hatches (vl-remove-if-not
                  '(lambda (e) (= "HATCH" (cdr (assoc 0 (entget e)))))
                  all))
  (setq lines (vl-remove-if
                '(lambda (e) (= "HATCH" (cdr (assoc 0 (entget e)))))
                all))
  ;; ולידציה
  (if (not hatches)
    (progn (princ "\nלא נבחר האצ'. בוטל.") (exit)))
  (if (> (length hatches) 1)
    (progn (princ "\nנבחרו יותר מהאצ' אחד. בחר האצ' אחד בלבד. בוטל.") (exit)))
  (if (not lines)
    (progn (princ "\nלא נבחרו קווי מתאר. בוטל.") (exit)))
  (setq srcHatch (car hatches))
  (setq srcLines lines)
  ;; בדוק אסוציאטיביות (קוד 71: 1=אסוציאטיבי 0=לא)
  (setq assoc? (= 1 (cdr (assoc 71 (entget srcHatch)))))
  ;; תייג מקור
  (col:tag srcHatch pid "SH")
  (foreach e srcLines (col:tag e pid "SL"))
  ;; צור עותק מוזח של קווי המתאר
  (setq cpyLines
    (mapcar '(lambda (e) (col:copy-and-move e disp (nth 2 s)))
            srcLines))
  ;; צור האצ' עותק
  (setq outer (car cpyLines))
  (setq inners (cdr cpyLines))
  (if assoc?
    (setq cpyHatch
      (if inners
        (col:mk-hatch-multi outer inners (nth 3 s)
          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
        (col:mk-hatch outer (nth 3 s)
          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))))
    (setq cpyHatch (col:copy-and-move srcHatch disp (nth 3 s))))
  ;; תייג עותק
  (foreach e cpyLines (if e (col:tag e pid "CL")))
  (if cpyHatch (col:tag cpyHatch pid "CH"))
  (princ "\nהשיוך הושלם.")
)

;;; ============================================================
;;;  עזרי מצב Multi
;;; ============================================================

;; ציור צורה אחת לפי בחירת המשתמש; מחזיר ("P" pt...) / ("C" cen r) / ("E" cen p1 p2)
;;; DCL לבחירת סוג צורה
(defun col:shape-dlg ( title / path f dclid res )
  (setq path (vl-filename-mktemp "shp" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "shp_dlg : dialog {" f)
  (write-line (strcat "  label = \"" title "\";") f)
  (write-line "  : radio_column {" f)
  (write-line "    : radio_button { key=\"P\"; label=\"Polygon\"; value=\"1\"; }" f)
  (write-line "    : radio_button { key=\"R\"; label=\"Rectangle\"; }" f)
  (write-line "    : radio_button { key=\"C\"; label=\"Circle\"; }" f)
  (write-line "    : radio_button { key=\"E\"; label=\"Ellipse\"; }" f)
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  (setq dclid (load_dialog path))
  (if (not (new_dialog "shp_dlg" dclid))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))
  (set_tile "P" "1")
  (setq res "P")
  (action_tile "P" "(setq res \"P\")")
  (action_tile "R" "(setq res \"R\")")
  (action_tile "C" "(setq res \"C\")")
  (action_tile "E" "(setq res \"E\")")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclid)
  (vl-file-delete path)
  res
)

(defun col:ask-more-dlg ( / ans )
  (initget "Yes No")
  (setq ans (getkword "\nהוסף חלוקה פנימית נוספת? [Yes/No] <No>: "))
  (= ans "Yes")
)

(defun col:draw-shape ( prompt / inp opt pts cr rp )
  (initget "Polygon Rectangle Circle Ellipse")
  (setq inp (getkword (strcat "\n" prompt " [Polygon/Rectangle/Circle/Ellipse] <Polygon>: ")))
  (if (not inp) (setq inp "Polygon"))
  (setq opt (cond
    ((= inp "R") "Rectangle")
    ((= inp "Rectangle") "Rectangle")
    ((= inp "C") "Circle")
    ((= inp "Circle") "Circle")
    ((= inp "E") "Ellipse")
    ((= inp "Ellipse") "Ellipse")
    (t "Polygon")))
  (cond
    ((= opt "Polygon")
     (setq pts (col:draw-poly))
     (if (>= (length pts) 3) (cons "P" pts) nil))
    ((= opt "Rectangle")
     (setq rp (col:draw-rect))
     (if rp (list "R" (car rp) (cadr rp)) nil))
    ((= opt "Circle")
     (setq cr (col:draw-circle))
     (if cr (list "C" (car cr) (cadr cr)) nil))
    ((= opt "Ellipse")
     (setq pts (col:draw-ellipse))
     (if pts (list "E" (car pts) (cadr pts) (caddr pts)) nil)))
)

;; יצירת ישות מתיאור צורה על שכבה נתונה; מחזיר ename
(defun col:mk-ent-from-shape ( shp lay )
  (cond
    ((= (car shp) "P") (col:mk-pline (cdr shp) lay))
    ((= (car shp) "R") (col:mk-rect (cadr shp) (caddr shp) lay))
    ((= (car shp) "C") (col:mk-circle (cadr shp) (caddr shp) lay))
    ((= (car shp) "E") (col:mk-ellipse (list (cadr shp) (caddr shp) (cadddr shp)) lay)))
)

;; הזחת תיאור צורה
(defun col:offset-shape ( shp disp )
  (cond
    ((= (car shp) "P")
     (cons "P" (col:offset-pts (cdr shp) disp)))
    ((= (car shp) "R")
     (list "R" (mapcar '+ (cadr shp) disp) (mapcar '+ (caddr shp) disp)))
    ((= (car shp) "C")
     (list "C" (mapcar '+ (cadr shp) disp) (caddr shp)))
    ((= (car shp) "E")
     (list "E"
       (mapcar '+ (cadr shp) disp)
       (mapcar '+ (caddr shp) disp)
       (mapcar '+ (cadddr shp) disp))))
)

;; האצ' על מספר גבולות: חיצוני + רשימת פנימיים (חורים)
(defun col:mk-hatch-multi ( outer inners lay pat sc ang / ss before )
  (setvar "HPNAME" pat)
  (setvar "HPSCALE" sc)
  (setvar "HPANG" ang)
  (setq ss (ssadd))
  (ssadd outer ss)
  (foreach inn inners (ssadd inn ss))
  (setq before (entlast))
  (command "_.-HATCH" "_LA" lay "_S" ss "" "")
  (col:find-new-hatch before lay)
)

;; איסוף גבולות ל-Multi: מחזיר (outer-shp (inner-shp ...))
(defun col:draw-multi ( lay / outer outer-ent inners inners-ents shp more )
  (setq outer (col:draw-shape "גבול חיצוני"))
  (if (not outer) (progn (princ "\nבוטל.") (exit)))
  (setq outer-ent (col:mk-ent-from-shape outer lay))
  (setq inners '() inners-ents '())
  (setq shp (col:draw-shape "חלוקה פנימית ראשונה"))
  (if (not shp) (progn (princ "\nבוטל.") (exit)))
  (setq inners (list shp))
  (setq inners-ents (list (col:mk-ent-from-shape shp lay)))
  (while
    (progn
      (setq more (col:ask-more-dlg))
      more)
    (setq shp (col:draw-shape "חלוקה פנימית נוספת"))
    (if shp
      (progn
        (setq inners (append inners (list shp)))
        (setq inners-ents (append inners-ents (list (col:mk-ent-from-shape shp lay)))))))
  (list outer outer-ent inners inners-ents)
)

;;; ============================================================
;;;  הפקודה הראשית COL
;;; ============================================================
(defun c:DCOL ( / s shape top-opt opt pts cen r p1 p2
                 srcLine srcHatch cpyLine cpyHatch
                 disp pid srcAxis cpyAxis axLay
                 mdata srcOuter srcInners cpyOuter cpyInners )
  (col:regapp)
  (setq s (col:ensure-settings))
  (setq shape (nth 13 s))
  ;; תפריט עליון
  (initget "Settings ColWall Geometry")
  (setq top-opt (getkword "\n[Settings / COL/WALL / Geometry] <COL/WALL>: "))
  (if (not top-opt) (setq top-opt "ColWall"))
  (cond
    ;; --- הגדרות ---
    ((= top-opt "Settings")
     (setq s (col:dlg s))
     (if s (col:put-settings s))
     (exit))
    ;; --- Geometry ---
    ((= top-opt "Geometry")
     (col:do-nested s)
     (exit)))
  ;; --- COL/WALL: תפריט משני ---
  (initget "Polygon Rectangle Circle Ellipse Multi Attach Update")
  (setq opt (getkword
    (strcat "\nCOL/WALL [Polygon/Rectangle/Circle/Ellipse/Multi/Attach/Update] <"
            (cond ((= shape "P") "Polygon")
                  ((= shape "R") "Rectangle")
                  ((= shape "C") "Circle")
                  ((= shape "E") "Ellipse")
                  ((= shape "M") "Multi")
                  (t "Polygon")) ">: ")))
  (cond
    ((= opt "Polygon")   (setq shape "P"))
    ((= opt "Rectangle") (setq shape "R"))
    ((= opt "Circle")    (setq shape "C"))
    ((= opt "Ellipse")   (setq shape "E"))
    ((= opt "Multi")     (setq shape "M"))
    ((= opt "Attach")
     (col:put-settings (append (col:sublist s 0 13) (list shape) (col:sublist s 14 (length s))))
     (setq s (col:get-settings))
     (col:do-attach s (col:disp s) (col:newid))
     (exit))
    ((= opt "Update")
     (c:DCOLUP)
     (exit)))
  ;; שמור צורה פעילה
  (col:put-settings
    (append (col:sublist s 0 13) (list shape) (col:sublist s 14 (length s))))
  (setq s (col:get-settings))
  (setq disp (col:disp s))
  (setq pid (col:newid))
  ;; ----- ציור ויצירה -----
  (cond
    ;; --- Rectangle ---
    ((= shape "R")
     (setq pts (col:draw-rect))
     (if pts
       (progn
         (setq srcLine (col:mk-rect (car pts) (cadr pts) (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-rect
                         (mapcar '+ (car pts) disp)
                         (mapcar '+ (cadr pts) disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
         (if (= "1" (nth 15 s))
           (progn
             (setq axLay (nth 14 s))
             (setq srcAxis (col:mk-axis "R" pts axLay))
             (setq cpyAxis (col:mk-axis "R"
               (list (mapcar '+ (car pts) disp) (mapcar '+ (cadr pts) disp)) axLay)))))))
    ;; --- Polygon ---
    ((= shape "P")
     (setq pts (col:draw-poly))
     (if (>= (length pts) 3)
       (progn
         (setq srcLine (col:mk-pline pts (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-pline (col:offset-pts pts disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))))))
    ;; --- Circle ---
    ((= shape "C")
     (setq cen (col:draw-circle))
     (setq r (cadr cen) cen (car cen))
     (setq srcLine (col:mk-circle cen r (nth 0 s)))
     (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                      (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
     (setq cpyLine (col:mk-circle (mapcar '+ cen disp) r (nth 2 s)))
     (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                      (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
     (if (= "1" (nth 16 s))
       (progn
         (setq axLay (nth 14 s))
         (setq srcAxis (col:mk-axis "C" (list cen r) axLay))
         (setq cpyAxis (col:mk-axis "C" (list (mapcar '+ cen disp) r) axLay)))))
    ;; --- Ellipse ---
    ((= shape "E")
     (setq pts (col:draw-ellipse))
     (if pts
       (progn
         (setq srcLine (col:mk-ellipse pts (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-ellipse (col:offset-ell pts disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
         (if (= "1" (nth 17 s))
           (progn
             (setq axLay (nth 14 s))
             (setq srcAxis (col:mk-axis "E" pts axLay))
             (setq cpyAxis (col:mk-axis "E" (col:offset-ell pts disp) axLay)))))))
    ;; --- Multi ---
    ((= shape "M")
     (setq mdata (col:draw-multi (nth 0 s)))
     (setq srcOuter  (cadr mdata))
     (setq srcInners (cadddr mdata))
     (setq srcHatch  (col:mk-hatch-multi srcOuter srcInners (nth 1 s)
                       (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
     (setq cpyOuter  (col:mk-ent-from-shape
                       (col:offset-shape (car mdata) disp) (nth 2 s)))
     (setq cpyInners (mapcar '(lambda (shp)
                                (col:mk-ent-from-shape (col:offset-shape shp disp) (nth 2 s)))
                              (caddr mdata)))
     (setq cpyHatch  (col:mk-hatch-multi cpyOuter cpyInners (nth 3 s)
                       (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))))
  ;; ----- תיוג הזוג -----
  (cond
    ((= shape "M")
     (if srcOuter  (col:tag srcOuter  pid "SOL"))
     (foreach e srcInners (if e (col:tag e pid "SIL")))
     (if srcHatch  (col:tag srcHatch  pid "SH"))
     (if cpyOuter  (col:tag cpyOuter  pid "COL"))
     (foreach e cpyInners (if e (col:tag e pid "CIL")))
     (if cpyHatch  (col:tag cpyHatch  pid "CH")))
    (t
     (if srcLine  (col:tag srcLine  pid "SL"))
     (if srcHatch (col:tag srcHatch pid "SH"))
     (if cpyLine  (col:tag cpyLine  pid "CL"))
     (if cpyHatch (col:tag cpyHatch pid "CH"))
     (if srcAxis (foreach e srcAxis (if e (col:tag e pid "SAX"))))
     (if cpyAxis (foreach e cpyAxis (if e (col:tag e pid "CAX"))))))
  (princ)
)

;; אליפסה מ-(cen p1 p2)
(defun col:mk-ellipse ( prm lay / cen p1 p2 maj ratio )
  (setq cen (car prm) p1 (cadr prm) p2 (caddr prm))
  (setq maj (mapcar '- p1 cen))
  (setq ratio (/ (distance cen p2) (distance cen p1)))
  (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                  (cons 8 lay) '(100 . "AcDbEllipse")
                  (cons 10 cen) (cons 11 maj)
                  (cons 40 ratio) '(41 . 0.0) (cons 42 (* 2 pi))))
)

;;; ============================================================
;;;  קווי ציר
;;; ============================================================

(defun col:mk-line ( p1 p2 lay )
  (entmakex (list '(0 . "LINE") (cons 8 lay) (cons 10 p1) (cons 11 p2)))
)

;; יוצר 2 קווי ציר לצורה; מחזיר רשימת enames
;; יצירת קווי ציר מישות קיימת (CIRCLE / ELLIPSE / LWPOLYLINE-rect)
(defun col:mk-axis-from-ent ( ent axLay / ed type cen maj ratio mlen mnlen ep1 p2 ux uy pts z )
  (if (not (and ent (entget ent))) nil
    (progn
      (setq ed   (entget ent))
      (setq type (cdr (assoc 0 ed)))
      (cond
        ((= type "CIRCLE")
         (col:mk-axis "C"
           (list (cdr (assoc 10 ed)) (cdr (assoc 40 ed)))
           axLay))
        ((= type "ELLIPSE")
         (setq cen   (cdr (assoc 10 ed)))
         (setq maj   (cdr (assoc 11 ed)))   ; וקטור יחסי למרכז
         (setq ratio (cdr (assoc 40 ed)))
         (setq mlen  (distance '(0 0 0) maj))
         (if (> mlen 0)
           (progn
             (setq mnlen (* ratio mlen))
             (setq ep1 (mapcar '+ cen maj))
             ;; כיוון ניצב לציר ראשי → ציר משני
             (setq ux (/ (- (cadr maj)) mlen))
             (setq uy (/ (car  maj)    mlen))
             (setq p2 (list (+ (car  cen) (* ux mnlen))
                            (+ (cadr cen) (* uy mnlen))
                            (caddr cen)))
             (col:mk-axis "E" (list cen ep1 p2) axLay))
           nil))
        ((= type "LWPOLYLINE")
         (setq pts (mapcar 'cdr (vl-remove-if-not
                                  '(lambda (x) (= (car x) 10)) ed)))
         (if (= (length pts) 4)
           (progn
             (setq z (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
             (col:mk-axis "R"
               (list (list (car (car   pts)) (cadr (car   pts)) z)
                     (list (car (caddr pts)) (cadr (caddr pts)) z))
               axLay))
           nil))
        (t nil))))
)

(defun col:mk-axis ( shape-type geom lay / cx cy cz
                     hw hh r
                     cen ep1 ep2 mlen mnlen
                     ux uy px py res )
  (setq res '())
  (cond
    ;; Rectangle
    ((= shape-type "R")
     (setq cx (/ (+ (car  (car geom)) (car  (cadr geom))) 2.0))
     (setq cy (/ (+ (cadr (car geom)) (cadr (cadr geom))) 2.0))
     (setq cz (caddr (car geom)))
     (setq hw (* (abs (- (car  (cadr geom)) (car  (car geom)))) 0.85))
     (setq hh (* (abs (- (cadr (cadr geom)) (cadr (car geom)))) 0.85))
     (setq res (list
       (col:mk-line (list (- cx hw) cy cz) (list (+ cx hw) cy cz) lay)
       (col:mk-line (list cx (- cy hh) cz) (list cx (+ cy hh) cz) lay))))
    ;; Circle
    ((= shape-type "C")
     (setq cen (car geom) r (* (cadr geom) 1.7))
     (setq cx (car cen) cy (cadr cen) cz (caddr cen))
     (setq res (list
       (col:mk-line (list (- cx r) cy cz) (list (+ cx r) cy cz) lay)
       (col:mk-line (list cx (- cy r) cz) (list cx (+ cy r) cz) lay))))
    ;; Ellipse
    ((= shape-type "E")
     (setq cen (car geom) ep1 (cadr geom) ep2 (caddr geom))
     (setq cx (car cen) cy (cadr cen) cz (caddr cen))
     (setq mlen  (* (distance cen ep1) 1.7))
     (setq mnlen (* (distance cen ep2) 1.7))
     ;; כיוון ציר ראשי
     (setq ux (/ (- (car  ep1) cx) (distance cen ep1)))
     (setq uy (/ (- (cadr ep1) cy) (distance cen ep1)))
     ;; כיוון ניצב (ציר משני)
     (setq px (- uy))
     (setq py ux)
     (setq res (list
       (col:mk-line (list (- cx (* ux mlen)) (- cy (* uy mlen)) cz)
                    (list (+ cx (* ux mlen)) (+ cy (* uy mlen)) cz) lay)
       (col:mk-line (list (- cx (* px mnlen)) (- cy (* py mnlen)) cz)
                    (list (+ cx (* px mnlen)) (+ cy (* py mnlen)) cz) lay)))))
  res
)

;;; ----- עזרי הזחה -----
(defun col:offset-pts ( pts disp )
  (mapcar '(lambda (p) (mapcar '+ p disp)) pts)
)
(defun col:offset-ell ( prm disp )
  (mapcar '(lambda (p) (mapcar '+ p disp)) prm)
)

;;; ----- תת-רשימה -----
(defun col:sublist ( lst start end / i r )
  (setq i 0 r '())
  (foreach x lst
    (if (and (>= i start) (< i end)) (setq r (cons x r)))
    (setq i (1+ i)))
  (reverse r)
)

;;; ----- מזהה זוג ייחודי (מונה + שעון) -----
(if (not *COL-COUNTER*) (setq *COL-COUNTER* 0))
(defun col:newid ()
  (setq *COL-COUNTER* (1+ *COL-COUNTER*))
  (strcat (rtos (getvar "MILLISECS") 2 0) "-" (itoa *COL-COUNTER*))
)

;;; ============================================================
;;;  Nested — חישוב צורה פנימית
;;; ============================================================

;; מחזיר 4 נקודות של הצורה הפנימית למרובע: (TL TR P BL)
(defun col:nested-inner-rect ( p1 p2 / tl tr bl dx dy z px py )
  (setq tl p1)
  (setq z  (caddr p1))
  (setq dx (- (car  p2) (car  p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq tr (list (car p2) (cadr p1) z))
  (setq bl (list (car p1) (cadr p2) z))
  (setq px (+ (car  tl) (* 0.15 dx)))
  (setq py (+ (cadr tl) (* 0.15 dy)))
  (list tl tr (list px py z) bl)
)

;; מחזיר 4 נקודות של הצורה הפנימית לפוליגון: (TL TR P BL)
;; S1 = הצלע הארוכה, S2 = הצלע הסמוכה הארוכה יותר, TL = הפינה המשותפת
(defun col:nested-inner-poly ( pts / n i p1 p2 len max-len max-idx
                                    s1-p1 s1-p2
                                    prev-i prev-p1 prev-p2 prev-len
                                    next-p1 next-p2 next-len
                                    s2-len tl tr bl z px py )
  (setq n (length pts))
  (if (< n 3) nil
    (progn
      (setq max-len 0  max-idx 0  i 0)
      (while (< i n)
        (setq p1 (nth i pts))
        (setq p2 (nth (rem (1+ i) n) pts))
        (setq len (distance p1 p2))
        (if (> len max-len) (progn (setq max-len len) (setq max-idx i)))
        (setq i (1+ i)))
      (setq s1-p1 (nth max-idx pts))
      (setq s1-p2 (nth (rem (1+ max-idx) n) pts))
      (setq prev-i   (rem (+ n (1- max-idx)) n))
      (setq prev-p1  (nth prev-i pts))
      (setq prev-p2  s1-p1)
      (setq prev-len (distance prev-p1 prev-p2))
      (setq next-p1  s1-p2)
      (setq next-p2  (nth (rem (+ max-idx 2) n) pts))
      (setq next-len (distance next-p1 next-p2))
      (if (>= prev-len next-len)
        (progn
          (setq tl s1-p1  tr s1-p2  bl prev-p1  s2-len prev-len))
        (progn
          (setq tl s1-p2  tr s1-p1  bl next-p2  s2-len next-len)))
      (setq z  (caddr tl))
      (setq px (+ (car  tl) (* 0.15 (- (car  tr) (car  tl)))
                             (* 0.15 (- (car  bl) (car  tl)))))
      (setq py (+ (cadr tl) (* 0.15 (- (cadr tr) (cadr tl)))
                             (* 0.15 (- (cadr bl) (cadr tl)))))
      (list tl tr (list px py z) bl)))
)

;; פונקציה ראשית: ציור Nested — חיצוני + פנימי + האצ' + עותק
(defun col:do-nested ( s / disp pid choice make-inner pts p1 p2 inner-pts
                           outer-ent inner-ent hatch-ent
                           cpy-outer cpy-inner cpy-hatch
                           lay-nest in-nest hl-nest pat-nest sc-nest ang-nest )
  (setq disp     (col:disp s))
  (setq pid      (col:newid))
  (setq lay-nest (nth 18 s))
  (setq hl-nest  (nth 19 s))
  (setq pat-nest (nth 20 s))
  (setq sc-nest  (atof (nth 21 s)))
  (setq ang-nest (atof (nth 22 s)))
  (setq in-nest  (nth 23 s))
  (initget "Polygon Rectangle")
  (setq choice (getkword "\nסוג צורה [Polygon/Rectangle] <Rectangle>: "))
  (if (not choice) (setq choice "Rectangle"))
  (initget "Inner NoInner")
  (setq make-inner (getkword "\n[Inner/NoInner] <Inner>: "))
  (if (not make-inner) (setq make-inner "Inner"))
  (cond
    ((= choice "Rectangle")
     (setq pts (col:draw-rect))
     (if (not pts) (progn (princ "\nבוטל.") (exit)))
     (setq p1 (car pts)  p2 (cadr pts))
     (setq outer-ent (col:mk-rect p1 p2 lay-nest))
     (if (= make-inner "Inner")
       (progn
         (setq inner-pts (col:nested-inner-rect p1 p2))
         (setq inner-ent (col:mk-pline inner-pts in-nest))
         (setq hatch-ent (col:mk-hatch inner-ent hl-nest pat-nest sc-nest ang-nest)))
       (setq inner-ent nil  hatch-ent nil))
     (setq cpy-outer (col:mk-rect (mapcar '+ p1 disp) (mapcar '+ p2 disp) lay-nest))
     (if (= make-inner "Inner")
       (progn
         (setq cpy-inner (col:mk-pline (col:offset-pts inner-pts disp) in-nest))
         (setq cpy-hatch (col:mk-hatch cpy-inner hl-nest pat-nest sc-nest ang-nest)))
       (setq cpy-inner nil  cpy-hatch nil)))
    ((= choice "Polygon")
     (setq pts (col:draw-poly))
     (if (< (length pts) 3) (progn (princ "\nבוטל.") (exit)))
     (setq outer-ent (col:mk-pline pts lay-nest))
     (setq inner-pts nil)
     (if (= make-inner "Inner")
       (progn
         (setq inner-pts (col:nested-inner-poly pts))
         (if inner-pts
           (progn
             (setq inner-ent (col:mk-pline inner-pts in-nest))
             (setq hatch-ent (col:mk-hatch inner-ent hl-nest pat-nest sc-nest ang-nest)))
           (progn
             (princ "\nלא ניתן לחשב צורה פנימית.")
             (setq inner-ent nil  hatch-ent nil))))
       (setq inner-ent nil  hatch-ent nil))
     (setq cpy-outer (col:mk-pline (col:offset-pts pts disp) lay-nest))
     (if (and (= make-inner "Inner") inner-pts)
       (progn
         (setq cpy-inner (col:mk-pline (col:offset-pts inner-pts disp) in-nest))
         (setq cpy-hatch (col:mk-hatch cpy-inner hl-nest pat-nest sc-nest ang-nest)))
       (setq cpy-inner nil  cpy-hatch nil))))
  (if outer-ent (col:tag outer-ent pid "NOL"))
  (if inner-ent (col:tag inner-ent pid "NIL"))
  (if hatch-ent (col:tag hatch-ent pid "NH"))
  (if cpy-outer (col:tag cpy-outer pid "CNOL"))
  (if cpy-inner (col:tag cpy-inner pid "CNIL"))
  (if cpy-hatch (col:tag cpy-hatch pid "CNH"))
  (princ "\nGeometry נוצר.")
)

;;; ============================================================
;;;  DCOLUP - עזרים
;;; ============================================================

;; קריאת XData: מחזיר (pid role) או nil
(defun col:read-xdata ( ent / xd app-data fields n )
  (setq xd (assoc -3 (entget ent (list *COL-XAPP*))))
  (if xd
    (progn
      (setq app-data (assoc *COL-XAPP* (cdr xd)))
      (if app-data
        (progn
          (setq fields (mapcar 'cdr (cdr app-data)))
          (setq n (length fields))
          (cond
            ((= n 3) (list (cadr fields) (caddr fields)))  ; ver+pid+role
            ((= n 2) (list (car fields) (cadr fields)))    ; pid+role (ישן)
            (t nil)))
        nil))
    nil)
)

;; קיבוץ רשימת (pid role ename) לפי pid
;; מחזיר ((pid (role ename ...) ...) ...)
(defun col:group-by-pid ( lst / result pid role ent pg rg )
  (setq result '())
  (foreach item lst
    (setq pid (car item) role (cadr item) ent (caddr item))
    (setq pg (assoc pid result))
    (if pg
      (progn
        (setq rg (assoc role (cdr pg)))
        (if rg
          (setq result (subst
            (cons pid (subst (append rg (list ent)) rg (cdr pg)))
            pg result))
          (setq result (subst
            (append pg (list (list role ent)))
            pg result))))
      (setq result (cons (list pid (list role ent)) result))))
  result
)

;; שכפול האצ' עם הזחה + שינוי דפוס/שכבה דרך entmakex (שומר גאומטריה וטרים)
;; מסיר הגדרות קווי הדפוס המקורי כך ש-ZCAD טוען את הדפוס החדש מקובץ ה-pattern
(defun col:clone-hatch-offset ( sh disp lay pat sc ang / ed dx dy dz new-ed item code solid-dst edge-type )
  (if (not (and sh (entget sh))) nil
    (progn
      (setq ed (entget sh))
      (setq dx (car disp) dy (cadr disp) dz (caddr disp))
      (setq solid-dst (= "SOLID" pat))
      (setq new-ed '())
      (setq edge-type 0) ; מעקב סוג קצה: 3=אליפסה (code 11 יחסי)
      (foreach item ed
        (setq code (car item))
        ;; עדכן סוג קצה (72: 1=קו 2=קשת 3=אליפסה 4=ספליין)
        (if (= code 72) (setq edge-type (cdr item)))
        (cond
          ;; קודים ייחודיים לישות — מדלגים
          ((member code '(-1 -2 -3 5 102 330 340 360)) nil)
          ;; הגדרות קווי דפוס — מדלגים (ZCAD טוען מקובץ pattern)
          ((member code '(53 43 44 45 46 49 79)) nil)
          ;; מספר שורות דפוס — אפס
          ((= code 78) (setq new-ed (append new-ed (list (cons 78 0)))))
          ;; שכבה
          ((= code 8)  (setq new-ed (append new-ed (list (cons 8  lay)))))
          ;; שם דפוס
          ((= code 2)  (setq new-ed (append new-ed (list (cons 2  pat)))))
          ;; דגל solid
          ((= code 70) (setq new-ed (append new-ed (list (cons 70 (if solid-dst 1 0))))))
          ;; סוג דפוס
          ((= code 76) (setq new-ed (append new-ed (list (cons 76 (if solid-dst 0 1))))))
          ;; scale
          ((= code 41) (if (not solid-dst)
                         (setq new-ed (append new-ed (list (cons 41 sc))))))
          ;; angle
          ((= code 52) (if (not solid-dst)
                         (setq new-ed (append new-ed (list (cons 52 ang))))))
          ;; צבע — BYLAYER
          ((= code 62) (setq new-ed (append new-ed (list (cons 62 256)))))
          ;; code 10 — תמיד נקודה מוחלטת — הזח
          ((= code 10)
           (setq new-ed (append new-ed (list
             (cons 10 (list (+ (car  (cdr item)) dx)
                            (+ (cadr (cdr item)) dy)
                            (caddr (cdr item))))))))
          ;; code 11 — יחסי רק באליפסה (edge-type=3) — אחרת מוחלט
          ((= code 11)
           (if (= edge-type 3)
             (setq new-ed (append new-ed (list item)))       ; יחסי — לא מזיח
             (setq new-ed (append new-ed (list              ; מוחלט — מזיח
               (cons 11 (list (+ (car  (cdr item)) dx)
                              (+ (cadr (cdr item)) dy)
                              (caddr (cdr item)))))))))
          ;; כל השאר — שמור
          (t (setq new-ed (append new-ed (list item))))))
      (entmakex new-ed)))
)

;; מחק ישות אם קיימת ולא נמחקה
(defun col:safe-del ( ent )
  (if (and ent (entget ent)) (entdel ent))
)

;; יצירת עותק מוזח של ישות קו (LWPOLYLINE / CIRCLE / ELLIPSE)
(defun col:clone-line-offset ( src disp lay / ed type )
  (if (not (and src (entget src))) (progn (princ "\n[DCOLUP] מקור חסר.") nil)
    (progn
      (setq ed (entget src))
      (setq type (cdr (assoc 0 ed)))
      (cond
        ((= type "LWPOLYLINE")
         (col:mk-pline
           (col:offset-pts
             (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) ed))
             disp)
           lay))
        ((= type "CIRCLE")
         (col:mk-circle (mapcar '+ (cdr (assoc 10 ed)) disp)
                        (cdr (assoc 40 ed)) lay))
        ((= type "ELLIPSE")
         (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                   (cons 8 lay) '(100 . "AcDbEllipse")
                   (cons 10 (mapcar '+ (cdr (assoc 10 ed)) disp))
                   (cons 11 (cdr (assoc 11 ed)))
                   (cons 40 (cdr (assoc 40 ed)))
                   '(41 . 0.0) (cons 42 (* 2 pi)))))
        (t nil))))
)

;; סנכרון זוג בודד
(defun col:sync-pair ( pg s / pid roles has-dup disp assoc?
                         sh sl-list cl-list sil-list cil-list
                         sol col-ent
                         new-cl-list new-ch new-col new-cil
                         new-sax new-cax ax-src-type ax-need
                         nol-ent nil-line lay-nest in-nest hl-nest pat-nest
                         sc-nest ang-nest new-cnol new-cnil new-cnh )
  (setq pid (car pg))
  (setq roles (cdr pg))
  (setq disp (col:disp s))
  ;; בדוק כפילויות: תפקיד עם יותר מאובייקט אחד = COPY של המשתמש
  (setq has-dup nil)
  (foreach r roles
    (if (and (member (car r) '("SH" "CH"))
             (> (length (cdr r)) 1))
      (setq has-dup t)))
  (if has-dup
    (princ (strcat "\n[DCOLUP] זוג " pid ": כפילויות — מדולג."))
    (progn
      (setq sh      (cadr (assoc "SH"  roles)))
      (setq sl-list (cdr  (assoc "SL"  roles)))
      (setq cl-list (cdr  (assoc "CL"  roles)))
      (setq sol     (cadr (assoc "SOL" roles)))
      (setq col-ent (cadr (assoc "COL" roles)))
      (setq sil-list (cdr (assoc "SIL" roles)))
      (setq cil-list (cdr (assoc "CIL" roles)))
      (cond
        ;; --- זוג רגיל / Attach (יש SL) ---
        (sl-list
         ;; מחק עותק ישן
         (col:safe-del (cadr (assoc "CH" roles)))
         (foreach e cl-list (col:safe-del e))
         ;; בנה קווי עותק חדשים
         (setq new-cl-list
           (vl-remove nil
             (mapcar '(lambda (e) (col:clone-line-offset e disp (nth 2 s)))
                     sl-list)))
         ;; האצ' עותק: העתק מהמקור (שומר טרים) — fallback לבנייה מחדש אם אין מקור
         (setq new-ch
           (if (and sh (entget sh))
             (col:clone-hatch-offset sh disp (nth 3 s)
               (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
             (if new-cl-list
               (if (= 1 (length new-cl-list))
                 (col:mk-hatch (car new-cl-list) (nth 3 s)
                   (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
                 (col:mk-hatch-multi (car new-cl-list) (cdr new-cl-list) (nth 3 s)
                   (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
               nil)))
         (foreach e new-cl-list (if e (col:tag e pid "CL")))
         (if new-ch
           ;; ודא שה-new-ch הוא באמת האצ' ולא הקו שנוצר
           (if (= "HATCH" (cdr (assoc 0 (entget new-ch))))
             (col:tag new-ch pid "CH")
             (princ "\n[DCOLUP] אזהרה: entlast אחרי HATCH הוא לא האצ'!"))))
        ;; --- זוג Multi ---
        (sol
         ;; מחק עותק ישן
         (col:safe-del (cadr (assoc "CH" roles)))
         (col:safe-del col-ent)
         (foreach e cil-list (col:safe-del e))
         ;; בנה עותק חדש
         (setq new-col (col:clone-line-offset sol disp (nth 2 s)))
         (setq new-cil
           (vl-remove nil
             (mapcar '(lambda (e) (col:clone-line-offset e disp (nth 2 s)))
                     sil-list)))
         (setq new-ch
           (if new-col
             (col:mk-hatch-multi new-col new-cil (nth 3 s)
               (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
             nil))
         (if new-col (col:tag new-col pid "COL"))
         (foreach e new-cil (if e (col:tag e pid "CIL")))
         (if new-ch (col:tag new-ch pid "CH")))
        ;; --- זוג Nested ---
        ((cadr (assoc "NOL" roles))
         (setq nol-ent  (cadr (assoc "NOL"  roles)))
         (setq nil-line (cadr (assoc "NIL"  roles)))
         (setq lay-nest (nth 18 s))
         (setq in-nest  (nth 23 s))
         (setq hl-nest  (nth 19 s))
         (setq pat-nest (nth 20 s))
         (setq sc-nest  (atof (nth 21 s)))
         (setq ang-nest (atof (nth 22 s)))
         (col:safe-del (cadr (assoc "CNOL" roles)))
         (col:safe-del (cadr (assoc "CNIL" roles)))
         (col:safe-del (cadr (assoc "CNH"  roles)))
         (setq new-cnol (col:clone-line-offset nol-ent  disp lay-nest))
         (setq new-cnil (if nil-line (col:clone-line-offset nil-line disp in-nest) nil))
         (setq new-cnh  (if new-cnil (col:mk-hatch new-cnil hl-nest pat-nest sc-nest ang-nest) nil))
         (if new-cnol (col:tag new-cnol pid "CNOL"))
         (if new-cnil (col:tag new-cnil pid "CNIL"))
         (if new-cnh  (col:tag new-cnh  pid "CNH")))
        (t
         (princ (strcat "\n[DCOLUP] זוג " pid ": מבנה לא מוכר — מדולג."))))
      ;; --- עדכון קווי ציר (SAX/CAX) ---
      ;; תמיד מנסה לפי סוג הצורה + הגדרות (לא תלוי אם SAX/CAX קיימים ב-roles)
      (if sl-list
        (progn
          ;; מחק ישנים
          (foreach e (cdr (assoc "SAX" roles)) (col:safe-del e))
          (foreach e (cdr (assoc "CAX" roles)) (col:safe-del e))
          (setq ax-src-type (cdr (assoc 0 (entget (car sl-list)))))
          (setq ax-need
            (cond
              ((and (= ax-src-type "CIRCLE")     (= "1" (nth 16 s))) t)
              ((and (= ax-src-type "ELLIPSE")    (= "1" (nth 17 s))) t)
              ((and (= ax-src-type "LWPOLYLINE") (= "1" (nth 15 s))
                    (= 4 (length (vl-remove-if-not
                                   '(lambda (x) (= (car x) 10))
                                   (entget (car sl-list)))))) t)
              (t nil)))
          (if ax-need
            (progn
              (setq new-sax (col:mk-axis-from-ent (car sl-list) (nth 14 s)))
              (setq new-cax
                (if new-cl-list
                  (col:mk-axis-from-ent (car new-cl-list) (nth 14 s))
                  nil))
              (foreach e new-sax (if e (col:tag e pid "SAX")))
              (foreach e new-cax (if e (col:tag e pid "CAX")))))))
    ))
)

;;; ============================================================
;;;  DCOLUP - סנכרון כל הזוגות
;;; ============================================================
(defun c:DCOLUP ( / ss i ent pairs pair-list s xd )
  (col:regapp)
  (setq s (col:ensure-settings))
  (princ "\nמסנכרן זוגות...")
  (setq ss (ssget "X" (list (list -3 (list *COL-XAPP*)))))
  (if (not ss)
    (princ "\nאין זוגות בשרטוט.")
    (progn
      (setq pairs '() i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq xd (col:read-xdata ent))
        (if xd (setq pairs (cons (list (car xd) (cadr xd) ent) pairs)))
        (setq i (1+ i)))
      (setq pair-list (col:group-by-pid pairs))
      (foreach pg pair-list (col:sync-pair pg s))
      (princ (strcat "\nסנכרון הושלם. "
                     (itoa (length pair-list)) " זוגות עובדו."))))
  (princ)
)

;;; ============================================================
;;;  DCOLSET- פתיחת הגדרות ידנית
;;; ============================================================
(defun c:DCOLSET ( / s )
  (setq s (col:ensure-settings))
  (setq s (col:dlg s))
  (if s (col:put-settings s))
  (princ "\nההגדרות נשמרו.")
  (princ)
)

(princ "\n=== COL/WALL נטען. פקודות: DCOL , DCOLUP , DCOLSET ===")
(princ)
