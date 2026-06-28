;=========================================================
;
; GScheme
; GS_Core.lsp
;
; Общие функции проекта
;
;=========================================================

(vl-load-com)

;---------------------------------------------------------
; Конфигурация
;---------------------------------------------------------

(setq GS:*Config*
 '(
   (ColWidth . 10.0)
   (RowHeight . 6.0)
   (TextHeight . 2.5)
   (TextStyle . "Standard")
 )
)

(defun GS:GetConfig (key)
 (cdr (assoc key GS:*Config*))
)

;---------------------------------------------------------
; AutoCAD
;---------------------------------------------------------

(defun GS:Acad ()
 (vlax-get-acad-object)
)

(defun GS:Doc ()
 (vla-get-ActiveDocument (GS:Acad))
)

(defun GS:Database ()
 (vla-get-Database (GS:Doc))
)

(defun GS:ModelSpace ()
 (vla-get-ModelSpace (GS:Doc))
)

(defun GS:PaperSpace ()
 (vla-get-PaperSpace (GS:Doc))
)

(defun GS:CurrentSpace ()
 (if (= (getvar "CVPORT") 1)
   (GS:PaperSpace)
   (GS:ModelSpace)
 )
)

;---------------------------------------------------------
; Геометрия
;---------------------------------------------------------

(defun GS:Point3D (pt)
 (vlax-3d-point
   (list
     (car pt)
     (cadr pt)
     (if (caddr pt) (caddr pt) 0.0)
   )
 )
)

;---------------------------------------------------------
; Проверки
;---------------------------------------------------------

(defun GS:IsBlockReference (obj)
 (= (vla-get-ObjectName obj) "AcDbBlockReference")
)

(defun GS:HasAttributes (obj)
 (= (vla-get-HasAttributes obj) :vlax-true)
)

;---------------------------------------------------------
; Атрибуты
;---------------------------------------------------------

(defun GS:GetAttribute (obj tag / att value)

 (setq value "")

 (if (GS:HasAttributes obj)

   (foreach att (vlax-invoke obj 'GetAttributes)

     (if (= (strcase (vla-get-TagString att))
            (strcase tag))

       (setq value
             (vla-get-TextString att))

     )

   )

 )

 value

)

(defun GS:SetAttribute (obj tag newValue / att)

 (if (GS:HasAttributes obj)

   (foreach att (vlax-invoke obj 'GetAttributes)

     (if (= (strcase (vla-get-TagString att))
            (strcase tag))

       (vla-put-TextString att newValue)

     )

   )

 )

 newValue

)

;---------------------------------------------------------
; Блоки
;---------------------------------------------------------

(defun GS:GetBlocks (/ ss i lst)

 (setq lst nil)

 (if (setq ss (ssget "_X" '((0 . "INSERT"))))

   (progn

     (setq i 0)

     (repeat (sslength ss)

       (setq lst

         (cons

           (vlax-ename->vla-object
             (ssname ss i)
           )

           lst

         )

       )

       (setq i (1+ i))

     )

   )

 )

 (reverse lst)

)

;---------------------------------------------------------
; Чтение блока
;---------------------------------------------------------

(defun GS:ReadBlock (obj)

 (list

   (cons 'E (GS:GetAttribute obj "E"))

   (cons 'G (GS:GetAttribute obj "G"))

   (cons 'N (GS:GetAttribute obj "N"))

   (cons 'P (GS:GetAttribute obj "P"))

   (cons 'OBJ obj)

 )

)

;---------------------------------------------------------
; Чтение проекта
;---------------------------------------------------------

(defun GS:ReadProject (/ data)

 (setq data nil)

 (foreach obj (GS:GetBlocks)

   (setq data

     (cons

       (GS:ReadBlock obj)

       data

     )

   )

 )

 (reverse data)

)

;---------------------------------------------------------
; Отладка
;---------------------------------------------------------

(defun c:GTESTCORE ()

 (foreach rec (GS:ReadProject)

   (print rec)

 )

 (princ)

)

(princ "\nGS_Core loaded.")
(princ)
