;;;***************************************************************************;;;
;;; vlex-vlisp.lsp                                ;;;
;;; Assorted Visual LISP ActiveX Extention Functions for AutoCAD 2004         ;;;
;;; Copyright (C)2003 Kama Whaley, All rights reserved.               ;;;
;;; Some functional code adapted from public sources.                 ;;;
;;; Latest Modify Date : Friday 27th December 2003                ;;;
;;;***************************************************************************;;;
;;; Version 2004 1.00 12/2003: Initial release (compile to VLX)           ;;;
;;;***************************************************************************;;;

(vl-Load-COM)
;; load ActiveX support in Visual LISP

;;; ***********************   <   First  Session   >   ***********************;;;

;;;***************************************************************************;;;
;;; MODULE: vlex-AcadObject ()                            ;;;
;;; DESCRIPTION: Returns COM handle to application object             ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-AcadObject) returns ActiveX object                 ;;;
;;;***************************************************************************;;;

(setq *acad-object* nil) ; Initialize global variable
(defun vlex-AcadObject ()
    (cond (*acad-object*) ; Return the cached object
                (T
					(setq *acad-object* (vlax-Get-Acad-Object))
                )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ActiveDocument ()                        ;;;
;;; DESCRIPTION: Returns active document object from application object       ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-ActiveDocument) returns ActiveX object             ;;;
;;;***************************************************************************;;;

(setq *vlex-ActiveDocument* nil) ; Initialize global variable
(defun vlex-ActiveDocument ()
    (cond   (*vlex-ActiveDocument*) ; Return the cached object
                (T
                 (setq *vlex-ActiveDocument*
                                (vla-Get-ActiveDocument (vlex-AcadObject))
                 )
                )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ModelSpace ()                            ;;;
;;; DESCRIPTION: Returns vlex-ModelSpace collection object of active document ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-ModelSpace) returns ActiveX object                 ;;;
;;;***************************************************************************;;;

(setq *vlex-ModelSpace* nil) ; Initialize global variable
(defun vlex-ModelSpace ()
    (cond   (*vlex-ModelSpace*) ; Return the cached object
                (T
                 (setq *vlex-ModelSpace* (vla-Get-ModelSpace (vlex-ActiveDocument)))
                )
    )
)


;;;***************************************************************************;;;
;;; MODULE: vlex-PaperSpace                           ;;;
;;; DESCRIPTION: Returns paper-space collection object of active document     ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-PaperSpace) returns ActiveX object                 ;;;
;;;***************************************************************************;;;

(setq *vlex-PaperSpace* nil) ; Intialize global variable
(defun vlex-PaperSpace ()
    (cond   (*vlex-PaperSpace*) ; Return the cached object
                (T
                 (setq *vlex-PaperSpace* (vla-Get-PaperSpace (vlex-ActiveDocument)))
                )
    )
)

(defun vlex-ActiveSpace ()
    (if (= 1 (vlax-get-Property (vlex-ActiveDocument) 'ActiveSpace))
        (vlex-ModelSpace)
        (vlex-PaperSpace)
    )               ; endif
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ActiveSpace-Name ()                          ;;;
;;; DESCRIPTION: Returns name(string) of current "space"              ;;;
;;                (either "Model" or "Paper")                     ;;;
;;;***************************************************************************;;;

(defun vlex-ActiveSpace-Name ()
    (if (= 1 (vla-get-ActiveSpace (vlex-ActiveDocument)))
        "Model"
        "Paper"
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AcadPrefs ()                             ;;;
;;; DESCRIPTION: Returns AcadPreferences object                   ;;;
;;; ARGS: none                                        ;;;
;;; EXAMPLE: (vlex-AcadPrefs) returns vla-object                  ;;;
;;;***************************************************************************;;;

(setq *vlex-AcadPrefs* nil) ; Initialize global variable
(defun vlex-AcadPrefs   ()
    (cond (*vlex-AcadPrefs*)
                (T
					(setq *vlex-AcadPrefs*
                                (vlax-Get-Property
                                    (vlex-AcadObject)
                                    'Preferences
                                )
                 )
                )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-GetPrefKey (tabname keyname)                     ;;;
;;; DESCRIPTION: Returns value of specified preferences setting           ;;;
;;; ARGS: tabname(string), keyname(string)                    ;;;
;;; EXAMPLE: (vlex-GetPrefKey 'Files 'SupportPath)                ;;;
;;;***************************************************************************;;;

(defun vlex-GetPrefKey (TabName KeyName)
    (vlax-get-property
        (vlax-get-property
            (vlex-AcadPrefs)
            TabName
        )
        KeyName
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SetPrefKey (tabname keyname new-value)                       ;;;
;;; DESCRIPTION: Modifies preferences setting with new value                  ;;;
;;; ARGS: tabname(string), keyname(string), new-value(varies)                 ;;;
;;; EXAMPLE: (vlex-SetPrefKey "OpenSave" "IncrementalSavePercent" 0)          ;;;
;;;***************************************************************************;;;

(defun vlex-SetPrefKey (TabName KeyName NewVal)
    (vlax-put-property
        (vlax-get-property
            (vlex-AcadPrefs)
            TabName
        )
        KeyName
        NewVal
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AcadProp (propname)                          ;;;
;;; DESCRIPTION: Returns value of acad-object property                ;;;
;;; ARGS: propname(string)                            ;;;
;;; EXAMPLE: (vlex-AcadProp 'FullName)                        ;;;
;;;***************************************************************************;;;

(defun vlex-AcadProp (PropName)
    (vlax-get-property (vlex-AcadObject) PropName)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Name (obj)                           ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE: (vlex-Name (vlex-AcadObject)) returns "AutoCAD"              ;;;
;;;***************************************************************************;;;

(defun vlex-Name (obj)
    (if (vlax-property-available-p obj 'Name)
        (vlax-get-property obj 'Name)
        "<NONE_NAME>"
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-GetDocsCollection                        ;;;
;;; DESCRIPTION: Returns the documents collection object              ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE:
;;;***************************************************************************;;;

(defun vlex-GetDocsCollection   ()
    (vlex-AcadCollection "Documents")
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AcadCollection (name)                        ;;;
;;; DESCRIPTION: Return a root collection of the AcadApplication object       ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:
;;;***************************************************************************;;;

(defun vlex-AcadCollection (Cname)
    (vlax-Get-Property (vlex-AcadObject) Cname)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DocsCount ()                             ;;;
;;; DESCRIPTION: Returns the count of the documents collection            ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (setq NumDocsOpen (vlex-DocsCount))                  ;;;
;;;***************************************************************************;;;

(defun vlex-DocsCount   ()
    (vlex-CollectionCount (vlex-GetDocsCollection))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-CollectionCount (collection)                     ;;;
;;; DESCRIPTION: Return the count of a given collection object            ;;;
;;; ARGS: collection-object                           ;;;
;;; EXAMPLE: (setq LayCount (vlex-CollectionCount (vlex-GetLayers)))          ;;;
;;;***************************************************************************;;;

(defun vlex-CollectionCount (Collection)
    (vlax-get-property Collection 'Count)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DocsList (verbose)                       ;;;
;;; DESCRIPTION: Returns a list of all opened document names              ;;;
;;; ARGS: Verbose<boolean>                            ;;;
;;; EXAMPLE: (setq alldocs (vlex-DocsList T))                     ;;;
;;; NOTES: Verbose returns full path+filename for each document in the list   ;;;
;;;        if set to T (true), otherwise only the filenames are returned.     ;;;
;;;***************************************************************************;;;

(defun vlex-DocsList (verbose / docname out)
    (setq out '())
    (vlax-for   each (vlex-GetDocsCollection)
        (if verbose
            (setq   docname
                         (strcat
                             (vlax-get-property each 'Path)
                             "\\"
                             (vlex-Name each)
                         )
            )
            (setq docname (vlex-Name each))
        )           ; endif
        (setq out (cons docname out))
    )
    (reverse out)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DumpIt                               ;;;
;;; DESCRIPTION: Dump all methods and properties for selected objects         ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLES:
;;;***************************************************************************;;;

(defun vlex-DumpIt (/ ent)
    (while (setq ent (entsel))
        (vlax-Dump-Object
            (vlax-Ename->Vla-Object (car ent))
        )
    )
    (princ)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Get____ ()                           ;;;
;;; DESCRIPTION: Various collection functions to return collection objects    ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (setq collLayers (vlex-GetLayers))                   ;;;
;;;***************************************************************************;;;

(defun vlex-GetLayers () (vlex-DocCollection 'Layers))
(defun vlex-GetLtypes () (vlex-DocCollection 'Linetypes))
(defun vlex-GetTextStyles   ()
    (vlex-DocCollection 'TextStyles)
)
(defun vlex-GetDimStyles () (vlex-DocCollection 'DimStyles))
(defun vlex-GetLayouts () (vlex-DocCollection 'Layouts))
(defun vlex-GetDictionaries ()
    (vlex-DocCollection 'Dictionaries)
)
(defun vlex-GetBlocks () (vlex-DocCollection 'Blocks))
(defun vlex-GetPlotConfigs ()
    (vlex-DocCollection 'PlotConfigurations)
)
(defun vlex-GetViews () (vlex-DocCollection 'Views))
(defun vlex-GetViewports () (vlex-DocCollection 'Viewports))
(defun vlex-GetGroups () (vlex-DocCollection 'Groups))
(defun vlex-GetRegApps ()
    (vlex-DocCollection 'RegisteredApplications)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DocCollection (name)                         ;;;
;;; DESCRIPTION: Return a collection from the vlex-ActiveDocument object      ;;;
;;; ARGS: collection-name(string or quote)                    ;;;
;;; EXAMPLE: (setq all-ltypes (vlex-DocCollection 'LineTypes))            ;;;
;;;***************************************************************************;;;

(defun vlex-DocCollection   (Cname)
    (vlax-Get-Property (vlex-ActiveDocument) Cname)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ListCollectionMemberNames (collection)               ;;;
;;; DESCRIPTION: Return list of all collection member names           ;;;
;;; ARGS: collection<object>                              ;;;
;;; EXAMPLE: (vlex-List-Collection-Member-Names (vlex-GetLayers))         ;;;
;;;***************************************************************************;;;

(defun vlex-ListCollectionMemberNames   (collection / itemname out)
    (setq out '())
    (vlax-for   each collection
        (setq   itemname (vlex-Name each)
                    out          (cons itemname out)
        )
    )
    (reverse out)
)

;;;***************************************************************************;;;
;;; List Collection Member Names                          ;;;
;;;***************************************************************************;;;

(defun vlex-ListLtypes ()
    (vlex-ListCollectionMemberNames (vlex-GetLtypes))
)
(defun vlex-ListLayers ()
    (vlex-ListCollectionMemberNames (vlex-GetLayers))
)
(defun vlex-ListTextStyles ()
    (vlex-ListCollectionMemberNames (vlex-GetTextStyles))
)
(defun vlex-ListDimStyles   ()
    (vlex-ListCollectionMemberNames (vlex-GetDimStyles))
)
(defun vlex-ListLayouts ()
    (vlex-ListCollectionMemberNames (vlex-GetLayouts))
)
(defun vlex-ListDictionaries ()
    (vlex-ListCollectionMemberNames (vlex-GetDictionaries))
)
(defun vlex-ListBlocks ()
    (vlex-ListCollectionMemberNames (vlex-GetBlocks))
)
(defun vlex-ListPlotConfigs ()
    (vlex-ListCollectionMemberNames (vlex-GetPlotConfigs))
)
(defun vlex-ListViews   ()
    (vlex-ListCollectionMemberNames (vlex-GetViews))
)
(defun vlex-ListViewPorts   ()
    (vlex-ListCollectionMemberNames (vlex-GetViewports))
)
(defun vlex-ListGroups ()
    (vlex-ListCollectionMemberNames (vlex-GetGroups))
)
(defun vlex-ListRegApps ()
    (vlex-ListCollectionMemberNames (vlex-GetRegApps))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-CountLtypes ()                           ;;;
;;; DESCRIPTION: Returns the count of the linetypes collection            ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (setq NumLtypes (vlex-CountLtypes))                  ;;;
;;;***************************************************************************;;;

(defun vlex-CountLtypes ()
    (vlex-CollectionCount (vlex-GetLtypes))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AcadCollection (name)                        ;;;
;;; DESCRIPTION: Return a root collection of the AcadApplication object       ;;;
;;; ARGS:
;;; EXAMPLE:
;;;***************************************************************************;;;

(defun vlex-AcadCollection (Cname)
    (vlax-Get-Property (vlex-AcadObject) Cname)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SortPoints (points-list sortfield)               ;;;
;;; DESCRIPTION: Sorts a list of point-list on x, y or z coordinates          ;;;
;;; ARGS: list of points (lists), sortfield(char "X", "Y" or "Z")         ;;;
;;; EXAMPLE: (vlex-SortPoints myPoints "Y") sorts on Y-coord values       ;;;
;;;***************************************************************************;;;

(defun vlex-SortPoints (points-list xyz)
    (setq xyz (strcase xyz))
    (cond
        ((= xyz "Z")
         ;; 3-point lists required!
         (if
             (apply '=
                            (mapcar
                                '(lambda (lst) (length lst))
                                points-list
                            )
             )
                (vl-sort
                    points-list
                    (function
                        (lambda (p1 p2) (< (caddr p1) (caddr p2)))
                    )
                )
                (princ "\nCannot sort on Z-coordinates with 2D points!")
         )      ; endif
        )           ;
        ((= xyz "X")
         (vl-sort
             points-list
             (function
                 (lambda (p1 p2) (< (car p1) (car p2)))
             )
         )
        )           ;
        ((= xyz "Y")
         (vl-sort
             points-list
             (function
                 (lambda (p1 p2) (< (cadr p1) (cadr p2)))
             )
         )
        )           ;
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-CollectionList (collection)                      ;;;
;;; DESCRIPTION: Return a list of collection member names                 ;;;
;;; ARGS: collection<object>                              ;;;
;;; EXAMPLE: (vlex-CollectionList (vlex-GetLtypes))               ;;;
;;;***************************************************************************;;;

(defun vlex-CollectionList (Collection / name out)
    (setq out '())
    (vlax-for   each Collection
        (setq name (vlex-Name each))
        (setq out (cons name out))
    )
    (reverse out)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DumpCollection (collection)                      ;;;
;;; DESCRIPTION: Display methods and properties for each collection member    ;;;
;;; ARGS: collection<object>                              ;;;
;;; EXAMPLE: (vlex-DumpCollection (vlex-GetLayers))               ;;;
;;;***************************************************************************;;;

(defun vlex-DumpCollection (Collection)
    (vlex-MapCollection Collection 'vlax-dump-object)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-MapCollection (collection function-expression)           ;;;
;;; DESCRIPTION: Apply a function to all members of a given collection        ;;;
;;; ARGS: collection(vla-object), function                    ;;;
;;; EXAMPLE: (vlex-MapCollection all-arcs 'vlex-DeleteObject)             ;;;
;;;***************************************************************************;;;

(defun vlex-MapCollection   (Collection qFunction)
    (vlax-map-collection Collection qFunction)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DeleteObject (object)                        ;;;
;;; DESCRIPTION: Invokes the Delete method on a given object to erase it      ;;;
;;; ARGS: object                                  ;;;
;;; EXAMPLE: (vlex-DeleteObject arc-object1)                      ;;;
;;;***************************************************************************;;;

(defun vlex-DeleteObject (obj)
    (princ "\n***DeleteObject")
    (cond
        ((and
             (not (vlax-erased-p obj))
             (vlax-read-enabled-p obj)
             (vlax-write-enabled-p obj)
         )
         (vlax-invoke-method obj 'Delete)
         (if (not (vlax-object-released-p obj))
             (vlax-release-object obj)
         )
        )           ;
        (T (princ "\nCannot delete object!"))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-MakeObject (object-or-ename)                     ;;;
;;; DESCRIPTION: Converts an ENAME type into a Vla-Object             ;;;
;;; ARGS: ename-or-object                             ;;;
;;; EXAMPLE: (setq myobj (vlex-MakeObject (car (entsel))) )           ;;;
;;;***************************************************************************;;;

(defun vlex-MakeObject (entname)
    (cond
        ((= (type entname) 'ENAME)
         (vlax-ename->vla-object entname)
        )
        ((= (type entname) 'VLA-OBJECT)
         entname
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ObjectType (object)                          ;;;
;;; DESCRIPTION: Returns ObjectName value for given object            ;;;
;;; ARGS: object                                  ;;;
;;; EXAMPLE: (= "AcDbArc" (vlex-ObjectType myobject))                 ;;;
;;;***************************************************************************;;;

(defun vlex-ObjectType (obj)
    (vlax-get-property obj 'ObjectName)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-UndoBegin ()                             ;;;
;;; DESCRIPTION: Begins an UNDO-MAKE group                    ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-UndoBegin)                             ;;;
;;;***************************************************************************;;;

(defun vlex-UndoBegin   ()
    (vlax-invoke-method (vlex-ActiveDocument) 'StartUndoMark)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-UndoEnd ()                           ;;;
;;; DESCRIPTION: Closes an UNDO group                         ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE: (vlex-UndoEnd)                           ;;;
;;;***************************************************************************;;;

(defun vlex-UndoEnd ()
    (vlax-invoke-method (vlex-ActiveDocument) 'EndUndoMark)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-CopyProp (property source-obj target-obj)            ;;;
;;; DESCRIPTION: Copy named property from one object to another           ;;;
;;; ARGS: property(string or quotedval), source(object), target(object)       ;;;
;;; EXAMPLE: (vlex-CopyProp "Layer" arc-object1 arc-object2)              ;;;
;;;***************************************************************************;;;

(defun vlex-CopyProp (propName source target)
    (cond
        ((member (strcase propName)
                         '("LAYER" "LINETYPE"   "COLOR" "LINETYPESCALE" "LINEWEIGHT" "PLOTSTYLENAME" "ELEVATION" "THICKNESS")
         )
         (cond
             ((and
                    (not (vlax-erased-p source))
                    ;; source not erased?
                    (not (vlax-erased-p target))
                    ;; target not erased?
                    (vlax-read-enabled-p source)
                    ;; can read from source object?
                    (vlax-write-enabled-p target)
                    ;; can write to target object?
                )
                (vlax-put-property
                    target
                    propName
                    (vlax-get-property source propName)
                )
             )  ;
             (T (princ "\nOne or more objects inaccessible!"))
         )      ; cond
        )           ;
        (T (princ "\nInvalid property-key request!"))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-MapPropertyList (properties source-obj target-obj)       ;;;
;;; DESCRIIPTION: Copies a list of properties from one object to another      ;;;
;;; ARGS: properties(list), source(object), target(object)            ;;;
;;; EXAMPLE: (vlex-MapPropertyList '("Layer" "Color") arc-object1 arc-object2 ;;;
;;;***************************************************************************;;;

(defun vlex-MapPropertyList (propList source target)
    (foreach prop   propList
        (vlex-CopyProp prop source target)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileImport (profile-name arg-file)                ;;;
;;; DESCRIPTION: Imports ARG file as new profile                  ;;;
;;; ARGS: profile-name(string), arg-file(string)                  ;;;
;;; EXAMPLE: (vlex-ProfileImport "MyProfile" "c:/test.arg")           ;;;
;;;***************************************************************************;;;
;;; VBA equivalent:                               ;;;
;;;     ThisDrawing.Application.preferences._                     ;;;
;;;     Profiles.ImportProfile _                          ;;;
;;;       strProfileToImport, strARGFileSource, True                  ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileImport   (pName ARGfile)
    (cond
        ((findfile ARGfile)
         (vlax-invoke-method
             (vlax-get-property (vlex-AcadPrefs) "Profiles")
             'ImportProfile
             pName
             ARGfile
             (vlax-make-variant 1 :vlax-vbBoolean)
             ;; == TRUE
         )
        )           ;
        (T (princ "\nARG file not found to import!"))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileExport (arg-file profile-name T )             ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS: arg-file(string), profile-name(string), T(Boolean)                  ;;;
;;; EXAMPLE: (vlex-ProfileImport "MyProfile" "c:/test.arg" T)             ;;;
;;;***************************************************************************;;;
;;; NOTES:                                    ;;;
;;; Exports the active profile so it can be shared with other users.          ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileExport   (strName strFilename BooleReplace)
    (if (vlex-ProfileExists-p strName)
        (if (not (findfile strFilename))
            (progn
                (vlax-Invoke-Method
                    (vlax-Get-Property (vlex-AcadPrefs) "Profiles")
                    'ExportProfile
                    strName
                    strFilename
                )
                T
                ;; return TRUE
            )
            (if BooleReplace
                (progn
                    (vl-file-delete (findfile strFilename))
                    (if (not (findfile strFilename))
                        (progn
                            (vlax-Invoke-Method
                                (vlax-Get-Property (vlex-AcadPrefs) "Profiles")
                                'ExportProfile
                                strName
                                strFilename
                            )
                            T
                            ;; return TRUE
                        ) ; progn
                        (princ "\nCannot replace ARG file, aborted.")
                    ) ; endif
                )   ; progn
                (princ (strcat "\n" strFilename " already exists, aborted.")
                )
            )       ; endif
        )           ; endif
    )               ; endif
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileDelete (profile-name)                     ;;;
;;; DESCRIPTION: Deletes a profile from the AcadApplication object        ;;;
;;; ARGS: profile-name(string)                            ;;;
;;; EXAMPLE: (vlex-ProfileDelete "MyProfile")                     ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileDelete   (pName)
    (vlax-invoke-method
        (vlax-get-property (vlex-AcadPrefs) "Profiles")
        'DeleteProfile
        pName
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileExists-p (profile-name)                   ;;;
;;; DESCRIPTION: Boolean test for profile existence               ;;;
;;; ARGS: profile-name(string)                            ;;;
;;; EXAMPLE: (if (vlxx-ProfileExists-p "MyProfile") ...)              ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileExists-p (pName)
;;; Search for CAPS profile-name in CAPS list of profiles
    (not
        (not
            (member
                (strcase pName)
                (mapcar 'strcase (vlex-ProfileList))
            )
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileList ()                           ;;;
;;; DESCRIPTION: Returns a list of all profile                    ;;;
;;; ARGS: none                                    ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileList (/ hold)
    (vlax-invoke-method
        (vlax-get-property (vlex-AcadPrefs) "Profiles")
        'GetAllProfileNames
        'hold
    )
    (if hold
        (vlax-safearray->list hold)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-CloseALlDocs                             ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Closes all open documents without saving                      ;;;
;;;***************************************************************************;;;

(defun vlex-CloseAllDocs (/ item cur)
    (vlax-For   item (vla-Get-Documents (vlex-AcadObject))
        (if (= (vla-Get-Active item) :vlax-False)
            (vla-Close item :vlax-False)
            (setq cur item)
        )
    )
    (vla-SendCommand cur "_.CLOSE")
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SaveALlDocs                              ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Saves all open documents without saving                   ;;;
;;;***************************************************************************;;;

(defun vlex-SaveAllDocs (/ item cur)
    (vlax-for   item (vla-Get-Document (vlex-AcadObject))
        (vla-save item)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Saved-p ()                           ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Tests to determine if the Active Document is saved                ;;;
;;;***************************************************************************;;;

(defun vlex-Saved-p ()
    (= (vla-get-saved (vlex-ActiveDocument)) :vlax-True)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SaveAs...                            ;;;
;;; DESCRIPTION: Save the ActiveDocument in different acSaveAsType        ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

;;;SaveAsType   acSaveAsType enum; read-write
;;;
;;;acR12_DXF
;;; AutoCAD Release12/LT2 DXF (*.dxf)
;;; 
;;;ac2000_dwg
;;; AutoCAD 2000 DWG (*.dwg)
;;; 
;;;ac2000_dxf
;;; AutoCAD 2000 DXF (*.dxf)
;;; 
;;;ac2000_Template
;;; AutoCAD 2000 Drawing Template File (*.dwt)
;;; 
;;;ac2004_dwg
;;; AutoCAD 2004 DWG (*.dwg)
;;; 
;;;ac2004_dxf
;;; AutoCAD 2004 DXF (*.dxf)
;;; 
;;;ac2004_Template
;;; AutoCAD 2004 Drawing Template File (*.dwt)
;;; 
;;;acNative
;;; A synonym for the current drawing release format. If you want your application to save the drawing in the format of whatever version of AutoCAD the application is running on, then use the acNative format.
;;; 
;;;AcUnknown
;;; Read-only. The drawing type is unknown or invalid.


(defun vlex-SaveAs2000 (name)
    (vla-saveas (vlex-ActiveDocument) name acR15_DWG)
)

(defun vlex-SaveAsR14   (name)
    (vla-saveas (vlex-ActiveDocument) name acR14_DWG)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-PurgeAllDocs                             ;;;
;;; DESCRIPTION: Purges all documents currently opened.               ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

(defun vlex-PurgeAllDocs (/ item cur)
    (vlax-for   item (vla-Get-Document (vlex-AcadObject))
        (vla-PurgeAll item)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ChangeAttributes (lst)                       ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE: (vlex-ChangeAttributes (list blk (cons "tag" "new-value")))      ;;;
;;;***************************************************************************;;;
;;; Arguments:
;;; A list containing one atom and one or more dotted pairs.
;;; The atom is the entity name of the block to change.
;;; The dotted pairs consist of the attribute tag and the new value for that attribute.
;;;
;;; Notes:
;;; Modifies the specified attribute in the specified block reference
;;;***************************************************************************;;;

(defun vlex-ChangeAttributes (lst / blk itm atts)
    (setq   blk (vlax-Ename->vla-Object (car lst))
                lst (cdr lst)
    )
    (if (= (vla-Get-HasAttributes blk) :vlax-true)
        (progn
            (setq   atts (vlax-SafeArray->list
                                     (vlax-Variant-Value (vla-GetAttributes blk))
                                 )
            )       ; setq
            (foreach item   lst
                (mapcar
                    '(lambda (x)
                         (if
                             (= (strcase (car item)) (strcase (vla-Get-TagString x)))
                                (vla-Put-TextString x (cdr item))
                         ) ; endif
                     )
                    atts
                )   ; mapcar
            )       ; foreach
            (vla-Update blk)
        )
    )               ; endif
)

;;;***************************************************************************;;;
;;; MODULE: vlex-GetAttributes (ent)                          ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Arguments
;;; The entity name of an attributed block
;;;
;;; Example
;;; (ax::GetAttributes (car (entsel)))
;;; Returns a list of attribute tags and associated values
;;;***************************************************************************;;;

(defun vlex-GetAttributes   (ent / blkref lst)
    (if (= (vla-Get-ObjectName
                     (setq blkref (vlax-Ename->vla-Object ent))
                 )
                 "AcDbBlockReference"
            )
        (if (vla-Get-HasAttributes blkref)
            (mapcar
                '(lambda (x)
                     (setq
                         lst (cons
                                     (cons (vla-Get-TagString x) (vla-Get-TextString x))
                                     lst
                                 )
                     )
                 )
                (vlax-safearray->list
                    (vlax-variant-value (vla-GetAttributes blkref))
                )
            )       ; mapcar
        )           ; endif
    )               ; endif
    (reverse lst)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ParseString (str delim)                      ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Arguments
;;; A delimited string and the delimiter character.
;;;
;;; Example:
;;; (vlex-ParseString (getenv "ACAD") ";")
;;;
;;; Notes:
;;; AutoLISP does not correctly interpret any character code outside the range of
;;; 1 to 255, so you cannot parse a null-delimited string.
;;; Returns a list containing all tokens in a delimited string
;;;***************************************************************************;;;

(defun vlex-ParseString (str delim / lst pos token)
    (setq pos (vl-String-Search delim str))
    (while pos
        (setq   lst (cons
                                (if (= (setq token (substr str 1 pos)) delim)
                                    nil
                                    token
                                ) ; endif
                                lst
                            )
                    str (substr str (+ (strlen delim) pos 1))
                    pos (vl-String-Search delim str)
        )           ; setq
    )               ; while
    (if (> (strlen str) 0)
        (setq lst (cons str lst))
    )
    (reverse lst)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-PolyCentroid (poly)                          ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS: poly(entity name)                           ;;;
;;; EXAMPLE:
;;;***************************************************************************;;;
;;; Arguments:
;;; The entity name of a closed, planar polyline
;;;
;;; Example:
;;; (ax:Centroid (car (entsel)))
;;;
;;; Returns the centroid of a closed polyline
;;; Thanks to Tony T for the original concept
;;;***************************************************************************;;;

(defun vlex-PolyCentroid (poly / pl ms va reg cen)
    (setq   pl (vlax-Ename->vla-Object poly)
                ms (vlex-ModelSpace)
                va (vlax-Make-SafeArray vlax-vbObject '(0 . 0))
    )
    (vlax-SafeArray-Put-Element va 0 pl)
    (setq   reg (car (vlax-SafeArray->list
                                     (vlax-Variant-Value (vla-AddRegion ms va))
                                 )
                        )
                cen (vla-Get-Centroid reg)
    )
    (vla-Delete reg)
    (vlax-SafeArray->list (vlax-Variant-Value cen))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Massoc                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Originally written by Tony Tanzillo
;;; Returns a list containing cdrs for every occurence of key in alist
;;; Arguments:
;;; An integer and an entity definition list
;;;
;;; Usage:
;;; (vlex-Massoc 10 (entget (car (entsel))))
;;;
;;; Notes:
;;; This is especially useful for retrieving all points associated with a lightweight polyline.
;;;***************************************************************************;;;

(defun vlex-Massoc (key alist)
    (apply
        'append
        (mapcar '(lambda (x)
                             (if (eq (car x) key)
                                 (list (cdr x))
                             )
                         )
                        alist
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Extents                              ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Originally written by Tony Tanzillo
;;; Returns a list containing the min and max points
;;;
;;; Arguments
;;; A list with three or more points
;;;
;;; Example
;;; (vlex-Extents '((1 0 0) (2 2 0) (1 2 0)))
;;;***************************************************************************;;;

(defun vlex-Extents (plist /)
    (list
        (apply 'mapcar (cons 'min plist))
        (apply 'mapcar (cons 'max plist))
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-RectCenter                           ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Returns the "center" of a rectangle
;;;
;;; Arguments
;;; The entity name of a rectangle
;;;
;;; Example
;;; (vlex-RectCenter (car (entsel)))
;;;***************************************************************************;;;

(defun vlex-RectCenter (rec)
    (vlex-Mid (vlex-Extents (vlex-Massoc 10 (entget rec))))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Mid (pts)                            ;;;
;;; DESCRIPTOIN:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Originally written by Michael Weaver
;;; Returns the point midway between two others
;;;
;;; Arguments
;;; A list of two points
;;;
;;; Example
;;; (mid '((1 1 0) (5 5 0)))
;;;***************************************************************************;;;

(defun vlex-Mid (pts / p0 p1)
    (setq   p0 (nth 0 pts)
                p1 (nth 1 pts)
    )
    (mapcar '(lambda (ord1 ord2) (/ (+ ord1 ord2) 2.0)) p0 p1)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-GetPolySegment (poly pt)                     ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:
;;;***************************************************************************;;;
;;; Returns a list containing the endpoints of the selected lwpoly segment    ;;;
;;; Thanks to Tony Tanzillo for showing me how to improve my routine          ;;;
;;;
;;; Arguments:
;;; The entity name of an lwpolyline and the point at which it was selected
;;;
;;; Example:
;;; (apply 'getseg (entsel))
;;;***************************************************************************;;;

(defun vlex-GetPolySegment (poly pt / pts i)
    (setq   pts (vlex-Massoc 10 (entget poly))
                i       (caddar (ssnamex (ssget pt)))
    )
    (list
        (nth (1- i) pts)
        (if
            (and
                (vlex-IsClosed poly)
                (= i (length pts))
            )
             (car pts)
             (nth i pts)
        )           ; endif
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-IsClosed (pl)                            ;;;
;;; DESCRIPTION: Specifies whether the 3D polyline, lightweight polyline,     ;;;
;;;              polyline, or spline is open or closed.               ;;;
;;; ARGS: The entity name of an lwpolyline, polyline, or spline.          ;;;
;;; EXAMPLE: (vlex-IsClosed (car (entsel)))                       ;;;
;;;***************************************************************************;;;
;;; Returns:
;;; T if the object has the specified 'Closed and it is really closed;
;;; nil, if the object hasn't the 'Closed property.
;;;***************************************************************************;;;

(defun vlex-IsClosed (epl / vpl)
    (setq vpl (vlex-MakeObject epl))
    (if (vlax-property-available-p vpl 'Closed)
        (= (vlax-get-property vpl 'Closed) :vlax-true)
    )
)

;;;***************************************************************************;;;
;;; MODULE:                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; Example function that convert ARC objects into CIRCLE objects by first    ;;;
;;; creating a CIRCLE in place of the ARC and then inheriting the various     ;;;
;;; properties of the ARC before deleting the ARC itself.             ;;;
;;;***************************************************************************;;;

(defun vlex-CloseArc (/ arcent arcobj trapobj circ)
    (while (setq arcent (entsel "\nSelect ARC object: "))
        (setq arcobj (vlex-MakeObject (car arcent)))
        (cond
            ((= "AcDbArc" (vlex-ObjectType arcobj))

             (vlex-UndoBegin)

             (setq circ
                            (vla-addCircle
                                (vlex-ModelSpace)
                                (vla-Get-center arcobj)
                                (vla-Get-radius arcobj)
                            )
             )
             (vlex-MapPropertyList
                 '("Layer" "Color" "Thickness" "Linetype" "LinetypeScale")
                 arcobj
                 circ
             )
             (vlex-DeleteObject arcobj)
             (vlax-Release-Object circ)

             (vlex-UndoEnd)
            )       ;
            (T (princ "\nNot an ARC object, try again..."))
        )           ; cond
    )               ; endwhile
    (princ)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Ltype-Exists-p (strLtype)                    ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE: (vlex-Ltype-Exists-p "DASHED")                   ;;;
;;;***************************************************************************;;;

(defun vlex-Ltype-Exists-p (strLtype)
    (cond
        ((member
             (strcase strLtype)
             (mapcar 'strcase (vlex-ListLtypes))
         )
         T
        )           ;
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Apply-Ltype (obj strLtype)                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE: (vlex-Apply-Ltype cirobj "DASHED")                   ;;;
;;;***************************************************************************;;;

(defun vlex-Apply-Ltype (obj strLtype / entlist)
    (cond
        ((vlex-Ltype-Exists-p strLtype)
         (cond
             ((and
                    (vlax-Read-Enabled-p obj)
                    ;; object can be read from
                    (vlax-Write-Enabled-p obj)
                    ;; object can be modified
                )
                (vla-Put-Linetype obj strLtype)
                T
                ;; return TRUE
             )  ;
             (T (princ "\nVlex-Apply-Ltype: Unable to modify object!"))
         )
        )           ;
        (T
         (princ (strcat "\nVlex-Apply-Ltype: Linetype ["
                                        strLtype
                                        "] not loaded."
                        )
         )
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE:                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; EXAMPLE: (vlex-AddLine (vlex-ModelSpace) pt1 pt2 "DOORS" 4 "DASHED")
;;; NOTES: <intColor> and <strLtype> can each be 'nil'
;;;***************************************************************************;;;

(defun vlex-AddLine (StartPt EndPt strLayer intColor strLtype / obj)
    (cond
        ((and StartPt (listp StartPt) EndPt (listp EndPt))
         (setq obj (vla-addLine
                                 (vlex-ModelSpace)
                                 (vlax-3D-Point StartPt)
                                 (vlax-3D-Point EndPt)
                             )
         )      ; setq
         (cond
             ((vlax-Write-Enabled-p obj)
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
                (vlex-MxRelease obj)
                (entlast)
             )  ;
             (T (princ "\nUnable to modify object properties..."))
         )
        )           ;
        (T (princ "\nVlex-AddLine: Invalid parameter list..."))
    )
)                   ; defun

(defun vlex-MxRelease (obj) (vlax-Release-Object obj))

;;;***************************************************************************;;;
;;; MODULE:                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;
;;; EXAMPLE: (vlex-AddArc (vlex-ModelSpace) pt1 0.5 0 90 "0" 3 "DASHED")
;;; NOTES:
;;;    <StartAng> and <EndAng> are in DEGREE values, not Radians
;;;    <intColor> and <strLtype> can each be 'nil'
;;;***************************************************************************;;;

(defun vlex-AddArc
                                     (CenterPt Radius   StartAng EndAng strLayer intColor   strLtype / obj)
    (cond
        ((and CenterPt (listp CenterPt) Radius StartAng EndAng)
         (setq obj
                        (vla-addArc
                            objSpace
                            (vlax-3D-Point CenterPt)
                            Radius
                            (vlex-DTR StartAng)
                            (vlex-DTR EndAng)
                        )
         )
         (cond
             ((vlax-Write-Enabled-p obj)
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
                (vlex-MxRelease obj)
                (entlast)
             )  ;
             (T (princ "\nUnable to modify object properties..."))
         )
        )           ;
        (T (princ "\nVlex-AddArc: Invalid parameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE:                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                         ;;;
;;; EXAMPLE:                                          ;;;
;;;***************************************************************************;;;
;;; EXAMPLE: (vlex-AddCircle (vlex-ModelSpace) pt1 0.5 "0" 3 "DASHED")
;;; NOTES: <intColor> and <strLtype> can each be 'nil'
;;;***************************************************************************;;;

(defun vlex-AddCircle
                                            (CenterPt Radius strLayer intColor strLtype / obj)
    (cond
        ((and CenterPt (listp CenterPt) Radius)
         (setq obj (vla-addCircle
                                 (vlex-ModelSpace)
                                 (vlax-3D-Point CenterPt)
                                 Radius
                             )
         )
         (cond
             ((vlax-Write-Enabled-p obj)
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
                (vlex-MxRelease obj)
                (entlast)
             )
             (T (princ "\nUnable to modify object properties..."))
         )      ; cond
        )           ;
        (T (princ "\nVlex-AddCircle: Invalid parameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DTR (a)                              ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                          ;;;
;;;***************************************************************************;;;

(defun vlex-DTR (a) (* pi (/ a 180.0)))

;;;***************************************************************************;;;
;;; MODULE: vlex-RTD (a)                              ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                          ;;;
;;;***************************************************************************;;;

(defun vlex-RTD (a) (/ (* a 180.0) pi))

;;;***************************************************************************;;;
;;; MODULE: vlex-AddPline (space ptlist layer closed color ltype width)       ;;;
;;; DESCRIPTION: Create LwPolyline with given properties              ;;;
;;; ARGS: space, points-list, layername, closed(T or nil), <color> is         ;;;
;;;   integer, <ltype> is string name, <width> is double/real number          ;;;
;;; EXMAPLE: (vlex-AddPline (vlex-ModelSpace) ptlist "0" T 3 "DASHED" 0.125)  ;;;
;;; NOTES: <Bclosed> <intColor> <dblWidth> and <strLtype> can each be 'nil'   ;;;
;;;   which is ByLayer.
;;;***************************************************************************;;;

(defun vlex-AddPline
                                         (ptlist strLayer   Bclosed intColor strLtype   dblWidth / vrtcs lst plgen plist plpoints   obj)
    (cond
        ((and ptlist (listp ptlist) (listp (car ptlist)))
         (setq plist        (apply 'append (mapcar '3dpoint->2dpoint ptlist))
                     plpoints   (vlex-List->VariantArray plist)
                     obj            (vla-AddLightWeightPolyline (vlex-ModelSpace) plpoints)
         )
         (cond
             ((and
                    (vlax-Read-Enabled-p obj)
;;; if able to read
                    (vlax-Write-Enabled-p obj)
;;; if open for change...
                )
                (if Bclosed
                    (vla-Put-Closed obj :vlax-True)
                )
                ;; make closed
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                ;; apply layer
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                ;; apply color
                (if dblWidth
                    (vla-Put-ConstantWidth obj dblWidth)
                )
                ;; apply constant width
                (if strLtype
                    ;; apply linetype and linetype generation
                    (progn
                        (vlex-Apply-Ltype obj strLtype)
                        ;; apply linetype
                        (vla-Put-LinetypeGeneration obj :vlax-True)
                        ;; apply linetype-gen
                    )
                )
                (vla-Update obj)
                ;; force graphic update
                (vlex-MxRelease obj)
                (entlast)
             )  ;
             (T (princ "\nVlex-AddPline: Unable to modify object!"))
         )      ; cond
        )           ;
        (T (princ "\nVlex-AddPline: Invalid parameter list...."))
    )               ; cond
)

(defun 3dpoint->2dpoint (3dpt / 2dpt)
    (setq 2dpt (list (car 3dpt) (cadr 3dpt)))
)

(defun 3dpoint-list->2dpoint-list   (3dplist / 2dplist)
    (cond
        ((and 3dplist (listp 3dplist) (listp (car 3dplist)))
         (setq 2dplist
                        (mapcar '(lambda (pt) (list (car pt) (cadr pt))) 3dplist)
         )
        )
        (T
         (princ
             "\n3dpoint-list->2dpoint-list: Invalid parameter list..."
         )
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-List->VariantArray (LIST)                    ;;;
;;; DESCRIPTION: Convert a LIST into a vla-Variant SafeArray date type        ;;;
;;; ARGS: LIST                                    ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

(defun vlex-DblList->VariantArray   (nList / ArraySpace sArray)
                    ; allocate space for an array of 2d points stored as doubles
    (setq   ArraySpace
                 (vlax-Make-SafeArray
                     vlax-vbDouble ; element type
                     (cons 0
                                 (- (length nList) 1)
                     )
                 )
    )
    (setq sArray (vlax-SafeArray-Fill ArraySpace nList))

                    ; return array variant
    (vlax-Make-Variant sArray)
)

(defun vlex-IntList->VarArray   (aList)
    (vlax-SafeArray-Fill
        (vlax-Make-SafeArray
            vlax-vbInteger ; (2) Integer
            (cons 0 (- (length aList) 1))
        )
        aList
    )
)

(defun vlex-VarList->VarArray   (aList)
    (vlax-SafeArray-Fill
        (vlax-Make-SafeArray
            vlax-vbVariant ;(12) Variant
            (cons 0 (- (length aList) 1))
        )
        aList
    )
)

;;;***************************************************************************;;;
;;; MODULE:                                   ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

(defun vlex-AddLineC
                                         (ptlist Bclosed strLayer intColor strLtype / pt1 ptz)
    (cond
        ((and ptlist (listp ptlist) (listp (car ptlist)))
         (setq pt1 (car ptlist)
                     ;; save first point
                     ptz (last ptlist)
                             ;; save last point
         )
         (while (and ptlist (>= (length ptlist) 2))
             (vlex-AddLine
                 (vlex-ModelSpace)
                 (car ptlist)
                 (cadr ptlist)
                 strLayer
                 intColor
                 strLtype
             )
             (setq ptlist (cdr ptlist))
         )
         (if (= Bclosed T)
             (vlex-AddLine
                 (vlex-ModelSpace)
                 pt1
                 ptz
                 strLayer
                 intColor
                 strLtype
             )
         )
        )           ;
        (T (princ "\nMakeLineC: Invalid parameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Roll-Ratio (Angle)                       ;;;
;;; DESCRIPTION: Converts ANGLE<degrees> into ratio for Ellipse roll angles   ;;;
;;; ARGS: angle<degrees>                              ;;;
;;; EXAMPLE: (setq roll-ratio (vlex-Roll-Ratio 45.0))                 ;;;
;;;***************************************************************************;;;

(defun vlex-Roll-Ratio (RollAngle)
    (cos (vlex-DTR RollAngle))
)

;;;***************************************************************************;;;

;;;***************************************************************************;;;
;;; MODULE: vlex-AddEllipse (space ctr hmaj roll layer color ltype)       ;;;
;;; DESCRIPTION: Create ELLIPSE object with given properties              ;;;
;;; ARGS: space centerpt hmajorpt rollangle layer color ltype             ;;;
;;; EXAMPLE: (vlex-AddEllipse (vlex-ModelSpace) l1 p2 45 "PARTS" nil nil)     ;;;
;;;***************************************************************************;;;
;;; NOTES: <space> is object, <centerpt> and <hmajorpt> are point lists       ;;;
;;;   <roll> is degrees angle, <layer> is string name, <color> is integer,    ;;;
;;;   <ltype> is string name. <color> <ltype> may be 'nil' == ByLayer         ;;;
;;;***************************************************************************;;;

(defun vlex-AddEllipse
                                             (ctr hmpt roll strLayer intColor strLtype / lst obj)
    (cond
        ((and ctr (listp ctr) hmpt (listp hmpt) roll)
         (setq hmpt (list
                                    (- (car hmpt) (car ctr))
                                    (- (cadr hmpt) (cadr ctr))
                                )
                     obj    (vla-addEllipse
                                    (vlex-ModelSpace)
                                    (vlax-3D-Point ctr)
                                    (vlax-3D-Point hmpt)
                                    (vlex-Roll-Ratio roll)
                                )
         )
         (cond
             ((vlax-Write-Enabled-p obj)
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
             )  ;
             (T (princ "\nUnable to modify object properties..."))
         )      ; cond
         (MxRelease obj)
         (entlast)
        )           ;
        (T (princ "\nInvalid paprameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddEllipseArc1                           ;;;
;;; DESCRIPTION:                                  ;;;
;;; ARGS:                                     ;;;
;;; EXAMPLE:                                      ;;;
;;;***************************************************************************;;;

(defun vlex-AddEllipseArc1
                                                     (ctr   hmpt roll   StartAng EndAng strLayer intColor   strLtype / obj rang)
    (cond
        ((and ctr (listp ctr) hmpt roll)
         (setq hmpt (list
                                    (- (car hmpt) (car ctr))
                                    (- (cadr hmhp) (cadr ctr))
                                )
                     obj    (vla-addEllipse
                                    (vlex-ModelSpace)
                                    (vlax-3D-Point ctr)
                                    (vlax-3D-Point hmpt)
                                    (vlex-Roll->Ratio roll)
                                )
         )
         (cond
             ((vlax-Write-Enabled-p obj)
                (vla-Put-StartAngle obj (vlex-DTR StartAng))
                (vla-Put-EndAngle obj (vlex-DTR EndAng))
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
                (MxRelease obj)
                (entlast)
             )  ;
             (T (princ "\nUnable to modify object properties..."))
         )      ; cond
        )           ;
        (T (princ "\nMakeArcEllipse1: Invalid parameter list..."))
    )               ; cond
)

;;;*************************************************************************;;;
;;; MODULE:                                                                 ;;;
;;; DESCRIPTION:                                                            ;;;
;;; ARGS:                                                                   ;;;
;;; EXAMPLE:                                                                ;;;
;;;*************************************************************************;;;

(defun vlex-AddEllipseArc2
                                                     (ctr   hmpt hmin   StartAng EndAng strLayer intColor   strLtype / obj rang)
    (cond
        ((and ctr (listp ctr) hmpt (listp hmpt) hmin)
         (setq hmpt (list
                                    (- (car hmpt) (car ctr))
                                    (- (cadr hmpt) (cadr ctr))
                                )
                     obj    (vla-addEllipse
                                    (vlex-ModelSpace)
                                    (vlax-3D-Point ctr)
                                    (vlax-3D-Point hmpt)
                                    hmin
                                )
         )
         (cond
             ((vlax-Write-Enabled-p obj)
                (vla-Put-StartAngle obj (vlex-DTR StartAng))
                (vla-Put-EndAngle obj (vlex-DTR EndAng))
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
                (vla-Update obj)
                (MxRelease obj)
                (entlast)
             )  ;
             (T (princ "\nUnable to modify object properties..."))
         )      ; cond
        )           ;
        (T (princ "\nMakeArcEllipse2: Invalid parameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE:                                                                   ;;;
;;; DESCRIPTION: Returns a list consistof start point and end point of the    ;;;
;;;              arc, line, or ellipse.                       ;;;
;;; ARGS:                                                                     ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-GetEllipseArcPoints
                                                                (ellent / ename-ellipse vlaobject-ellipse p-start p-end out)
    (setq   vlaObject-Ellipse   (vlex-MakeObject ellent)
                ;; convert ename to object
                p-start                     (vla-Get-StartPoint vlaObject-Ellipse)
                p-end                           (vla-Get-EndPoint vlaObject-Ellipse)
                out                             (list
                                                        (vlax-SafeArray->List (vlax-Variant-Value p-start))
                                                        (vlax-SafeArray->List (vlax-Variant-Value p-end))
                                                    )
    )
    out
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddPoint                             ;;;
;;; DESCRIPTION: Creates POINT object with specified properties           ;;;
;;; ARGS: point, layer                                ;;;
;;; EXAMPLE: (vlex-AddPoint p1 "DEFPOINTS")
;;;***************************************************************************;;;

(defun vlex-AddPoint (pt strLayer / obj)
    (cond
        ((and pt (listp pt))
         (setq obj (vla-addPoint (vlex-ModelSpace) (vlax-3D-Point pt)))
         (if (vlax-Write-Enabled-p obj)
             (progn
                 (if strLayer
                     (vla-Put-Layer obj strLayer)
                 )
                 (vla-Update obj)
                 (MxRelease obj)
                 (entlast)
             )
             (princ "\nVlex-AddPoint: Unable to modify object!")
         )      ; if
        )           ;
        (T (princ "\nVlex-AddPoint: Invalid parameter list..."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddText                              ;;;
;;; DESCRIPTION: Creates TEXT object with sepecified properties           ;;;
;;; ARGS: string, point, justification, style, hgt, wid, rot, lay, color      ;;;
;;; EXAMPLE: (vlex-AddText "ABC" p1 "MC" "STANDARD" 0.25 1.0 0 "TEXT" nil)    ;;;
;;;***************************************************************************;;;

(defun vlex-AddText
                                        (strTxt pt Just strStyle dblHgt dblWid dblRot   strLay intCol   /   txtobj)
    (cond
        ((setq txtobj
                        (vla-AddText
                            (vlex-ActiveSpace)
                            strTxt
                            (if (not (member (strcase Just) '("A" "F")))
                                (vlax-3d-Point pt)
                                (vlax-3d-Point (car pt))
                            ) ; endif
                            dblHgt
                            ;; ignored if Just = "A" (aligned)
                        )
         )
         (vla-put-StyleName txtobj strStyle)
         (vla-put-Layer txtobj strLay)
         (if intCol
             (vla-put-Color txtobj intCol)
         )
         (setq Just (strcase Just))
         ;; force to upper case for comparisons...

         ;; Left/Align/Fit/Center/Middle/Right/BL/BC/BR/ML/MC/MR/TL/TC/TR 
         ;; Note that "Left" is not a normal default.
         ;;
         ;; ALIGNMENT TYPES...
         ;; AcAlignmentLeft=0
         ;; AcAlignmentCenter=1
         ;; AcAlignmentRight=2
         ;; AcAlignmentAligned=3
         ;; AcAlignmentMiddle=4
         ;; AcAlignmentFit=5
         ;; AcAlignmentTopLeft=6
         ;; AcAlignmentTopCenter=7
         ;; AcAlignmentTopRight=8
         ;; AcAlignmentMiddleLeft=9
         ;; AcAlignmentMiddleCenter=10
         ;; AcAlignmentMiddleRight=11
         ;; AcAlignmentBottomLeft=12
         ;; AcAlignmentBottomCenter=13
         ;; AcAlignmentBottomRight=14
         ;;                                                               
         ;; HORIZONTAL JUSTIFICATIONS...                                  
         ;; AcHorizontalAlignmentLeft=0                                   
         ;; AcHorizontalAlignmentCenter=1                                 
         ;; AcHorizontalAlignmentRight=2                                  
         ;; AcHorizontalAlignmentAligned=3                                
         ;; AcHorizontalAlignmentMiddle=4                                 
         ;; AcHorizontalAlignmentFit=5                                    
         ;;                                                               
         ;; VERTICAL JUSTIFICATIONS...                                    
         ;; AcVerticalAlignmentBaseline=0                                 
         ;; AcVerticalAlignmentBottom=1                                   
         ;; AcVerticalAlignmentMiddle=2                                   
         ;; AcVerticalAlignmentTop=3                                      

         (cond
             ((= Just "L")
                ;; Left
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "C")
                ;; Center
                (vla-put-Alignment txtobj 1)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "R")
                ;; Right
                (vla-put-Alignment txtobj 2)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "A")
                ;; Alignment
                (vla-put-Alignment txtobj 3)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
             )
             ((= Just "M")
                ;; Middle
                (vla-put-Alignment txtobj 4)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "F")
                ;; Fit
                (vla-put-Alignment txtobj 5)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
             )
             ((= Just "TL")
                ;; Top-Left
                (vla-put-Alignment txtobj 6)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "TC")
                ;; Top-Center
                (vla-put-Alignment txtobj 7)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "TR")
                ;; Top-Right
                (vla-put-Alignment txtobj 8)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "ML")
                ;; Middle-Left
                (vla-put-Alignment txtobj 9)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "MC")
                ;; Middle-Center
                (vla-put-Alignment txtobj 10)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "MR")
                ;; Middle-Right
                (vla-put-Alignment txtobj 11)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "BL")
                ;; Bottom-Left
                (vla-put-Alignment txtobj 12)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "BC")
                ;; Bottom-Center
                (vla-put-Alignment txtobj 13)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
             ((= Just "BR")
                ;; Bottom-Right
                (vla-put-Alignment txtobj 14)
                (vla-put-TextAlignmentPoint txtobj (vlax-3d-point pt))
                (vla-put-ScaleFactor txtobj dblWid)
                (vla-put-Rotation txtobj (DTR dblRot))
             )
         )
         (vla-Update txtobj)
         (vlax-Release-Object txtobj)
         (entlast)
        )           ;
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddPolygon                           ;;;
;;; DESCRIPTION: Creates a circumscribed polygon                  ;;;
;;; ARGS: center, radius, sides, flag, width, layer, color, ltype         ;;;
;;; EXAMPLE: (vlex-AddPolygon pt1 1.0 6 nil 0 "0" nil "DASHED")           ;;;
;;;***************************************************************************;;;

(defun vlex-AddPolygon
                                             (ctrpt dblRad intSides strType dblWid strLay   intCol strLtype /   pa dg   ptlist deg)
    (setq   pa  (polar ctrpt 0 dblRad)
                dg  (/ 360.0 intSides)
                ;; get angles between faces
                deg dg
    )
    (repeat intSides
        (setq   ptlist
                     (if ptlist
                         (append ptlist (list (polar ctrpt (vlex-DTR deg) dblRad)))
                         (list (polar ctrpt (vlex-DTR deg) dblRad))
                     )
        )
        (setq deg (+ dg deg))
    )               ; repeat
    (vlex-AddPline ptlist strLay T intCol strLtype dblWid)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddRectangle                             ;;;
;;; DESCRIPTION: Creates a rectangle with sepecified properties           ;;;
;;; ARGS: p1(lower left), p3(upper right), layer, color, linetype, width      ;;;
;;; EXAMPLE: (vlex-AddRectangle p1 p3 "0" nil "DASHED" 0.25)                  ;;;
;;;***************************************************************************;;;

(defun vlex-AddRectangle
                                                 (p1 p3 strLayer intColor strLtype dblWid / p2 p4 obj)
    (setq   p2 (list (car p1) (cadr p3))
                p4 (list (car p3) (cadr p1))
    )
    (cond
        ((setq obj (vlex-AddPline
                                 (list p1 p2 p3 p4)
                                 strLayer
                                 T
                                 intColor
                                 strLtype
                                 dblWidth
                             )
         )
         obj
         ;; raise object (entity name)
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-AddSolid                             ;;;
;;; DESCRIPTION: Creates a Solid with sepecified properties           ;;;
;;; ARGS: points-list, layer(string), color(integer)                      ;;;
;;; EXAMPLE: (vlex-AddSolid ptlist "0" nil)                       ;;;
;;;***************************************************************************;;;

(defun vlex-AddSolid (ptlist strLayer intColor / plist obj)
    (cond
        ((and ptlist (listp ptlist) (listp (car ptlist)))
         (if (= (length ptlist) 3)
             (setq plist (append ptlist (list (last ptlist))))
             (setq plist ptlist)
         )
         (vlex-DPR "\nMaking solid object...")
         (cond
             ((setq obj (vla-addSolid
                                        (vlex-ActiveSpace)
                                        (vlax-3D-Point (car plist))
                                        (vlax-3D-Point (cadr plist))
                                        (vlax-3D-Point (caddr plist))
                                        (vlax-3D-Point (cadddr plist))
                                    )
                )
                (if strLayer
                    (vla-Put-Layer obj strLayer)
                )
                (if intColor
                    (vla-Put-Color obj intColor)
                )
                (vla-Update obj)
                (vlax-release-object obj)
                (entlast)
             )  ;
             (T (princ "\nUnable to create object..."))
         )      ; cond
        )           ;
        (T (princ "\nVlex-AddSolid: Invalid parameter list..."))
    )               ; cond
)

(defun vlex-DPR (msg)
    ;; debugging status printer
    (if $DBG
        (princ msg)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Apply-LtScale (object ltscale)                   ;;;
;;; DESCRIPTION: Apply object linetype scaling                    ;;;
;;; ARGS: ename or object, scale (real)                       ;;;
;;; EXAMPLE: (vlex-Apply-LtScale objLine 24.0)                    ;;;
;;;***************************************************************************;;;

(defun vlex-Apply-LtScale   (obj dblLtScale)
    (cond
        ((and
             (vlax-Read-Enabled-p obj)
             ;; object can be read from
             (vlax-Write-Enabled-p obj)
             ;; object can be modified
         )
         (vla-Put-Linetype dblLtScale)
         T
         ;; return TRUE
        )           ;
        (T (princ "\nVlex-Apply-LtScale: Unable to modify object!"))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-VarSave (vlist)                                              ;;;
;;; DESCRIPTION: Save sysvars to global list for restoring later.             ;;;
;;; ARGS:                                                                     ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(setq G$VARS nil) ; Initialize global variable
(defun vlex-VarSave (vlist / n)
    (foreach n vlist
        (setq   G$VARS
                     (if G$VARS
                         (append G$VARS (list (list n (getvar n))))
                         (list (list n (getvar n)))
                     )
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-VarRestore ()                                                ;;;
;;; DESCRIPTION: Restore sysvars from global list for restoring later.        ;;;
;;; ARGS:                                                                     ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-VarRestore (/ $orr #err)
    (defun #err (s)
        (princ (strcat "\nError: " s))
        (setq G$VARS nil)
        (setq *error* $orr)
        (princ)
    )
    (setq   $orr *error*
                *error* #err
    )
    (cond
        ((and G$VARS (listp G$VARS))
         (foreach   n   G$VARS
             (cond
                 ((= (strcase (car n)) "CLAYER")
                    (command "_.layer" "_s" (cadr n) "")
                 )
                 ((= (strcase (car n)) "VIEWPORT")
                    (command "_.viewres" "_Y" (cadr n) "")
                 )
                 (T (setvar (car n) (cadr n)))
             )  ; cond
         )      ; foreach
         (setq G$VARS nil)
        )
    )               ; cond
    (setq   *error* $orr
                $orr nil
    )
)

;;; ***********************   <   Second Session   >   ***********************;;;

;;; Layers -->>

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerTable ()                                                ;;;
;;; DESCRIPTION: Get Document Layers collection object                        ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerTable ()
    (vla-get-Layers (vlex-ActiveDocument))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayZero ()                                                   ;;;
;;; DESCRIPTION: Set Active Layer in Document to zero "0"                     ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayZero ()
    (vla-put-ActiveLayer
        (vlex-ActiveDocument)
        (vla-Item (vlex-LayerTable) 0)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayActive (name)                                             ;;;
;;; DESCRIPTION: Set active layer to <name> if it exists                      ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayActive   (name / iloc out)
    (cond
        ((and
             (tblsearch "layer" name)
             (setq iloc (vl-Position name (vlex-ListLayers)))
         )
         (vla-put-ActiveLayer
             (vlex-ActiveDocument)
             (vla-Item (vlex-LayerTable) iloc)
         )
         (setq out name)
        )           ;
        (T (princ (strcat "\nLayer not defined: " name)))
    )               ; cond
    out
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerOn (LayList)                                            ;;;
;;; DESCRIPTION: Turn ON all layers in given list                             ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerOn (LayList)
    (vlax-for   each (vla-get-layers (vlex-ActiveDocument))
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-LayerOn each :vlax-True)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerOff (LayList)                                           ;;;
;;; DESCRIPTION: Turn OFF all layers in given list                            ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerOff (LayList)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-LayerOn each :vlax-False)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerFreeze (LayList)                                        ;;;
;;; DESCRIPTION: Freeze all layers in given list                              ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerFreeze (LayList)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-Freeze each :vlax-True)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerThaw (LayList)                                          ;;;
;;; DESCRIPTION: Thaw all layers in given list                                ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerThaw   (LayList)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-Freeze each :vlax-False)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerNoPlot (LayList)                                        ;;;
;;; DESCRIPTION: Toggle Plot/No-Plot setting for layers.                      ;;;
;;; EXAMPLE: (vlex-LayerNoPlot '("DOORS" "WINDOWS") T)                ;;;
;;;                sets layers to NOT plot                        ;;;
;;;          (vlex-LayerNoPlot '("DOORS" "WINDOWS") nil)              ;;;
;;;                sets layers to PLOT                        ;;;
;;;***************************************************************************;;;

(defun vlex-LayerNoPlot (LayList On-Off)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (if On-Off
                    (vla-put-Plottable each :vlax-True)
                    (vla-put-Plottable each :vlax-False)
                )
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerLock (LayList)                                          ;;;
;;; DESCRIPTION: Lock all layers in given list                                ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerLock   (LayList)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-Lock each :vlax-True)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LayerUnLock (LayList)                                        ;;;
;;; DESCRIPTION: Unlock all layers in given list                              ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-LayerUnLock (LayList)
    (vlax-for   each (vlex-LayerTable)
        (if (member (strcase (vla-get-name each)) LayList)
            (if (vlax-write-enabled-p each)
                (vla-put-Lock each :vlax-False)
            )
        )
        (vlax-release-object each)
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ListLayers-Locked ()                                         ;;;
;;; DESCRIPTION: Returns a list of layers that are currently Locked           ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-ListLayers-Locked   (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "Lock") :vlax-true)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)
;;;***************************************************************************;;;
;;; MODULE: vlex-ListLayers-Frozen ()                                         ;;;
;;; DESCRIPTION: Returns a list of layers that are currently frozen or 'nil'  ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-ListLayers-Frozen   (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "Freeze") :vlax-true)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ListLayers-Off ()                                            ;;;
;;; DESCRIPTION: Returns a list of layers that are currently turned OFF       ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-ListLayers-Off (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "LayerOn") :vlax-false)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)

(defun vlex-ListLayers-On   (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "LayerOn") :vlax-true)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)


;;;***************************************************************************;;;
;;; MODULE: vlex-ListLayers-Plottable ()                                      ;;;
;;; DESCRIPTION: Returns a list of layers that are currently Plottable        ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-ListLayers-Plottable (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "Plottable") :vlax-true)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ListLayers-Plottable-Not ()                                  ;;;
;;; DESCRIPTION: Returns a list of layers that are currently NOT Plottable    ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-ListLayers-Plottalbe-Not (/ each out)
    (vlax-for   each (vlex-LayerTable)
        (if (= (vlax-get-property each "Plottable") :vlax-false)
            (setq out (cons (vla-get-name each) out))
        )
    )
    out
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Layer-Frozen-p (lname)                                       ;;;
;;; DESCRIPTION: Returns T or nil if named layer is currently frozen          ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-Layer-Frozen-p (lname / each)
    (if
        (and
            (setq fl (vlex-ListLayers-Frozen))
            ;; any frozen layers?
            (member (strcase lname) (mapcar 'strcase fl))
        )
         T
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SetLweight (obj intLwt)                                      ;;;
;;; DESCRIPTION: Set LineWeight index property for given object (or layer)    ;;;
;;; EXAMPLE:                                                                  ;;;
;;; NOTES:                                    ;;;
;;;   "ByLwDefault" = -3                              ;;;
;;;   "ByBlock" = -2                                  ;;;
;;;   "ByLayer" = -1                                  ;;;
;;;   Other values are 0, 5, 9, 13, 15, 18, 20, 25, 30, 35, 40, 50, 53, 60,   ;;;
;;;   70, 80, 90, 100, 106, 120, 140, 158, 200, 211               ;;;
;;;***************************************************************************;;;

(defun vlex-SetLweight (obj intLwt)
    (cond
        ((member intLwt
                         '(0 5 9 13 15 18   20 25   30 35   40 50   60 70   80 90   100 106 120 140 158 200 211)
         )
         (vla-put-LineWeight obj ineLwt)
         T
         ;; return TRUE
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DefineLayer (strName intColor strLtype booleCur)             ;;;
;;; DESCRIPTION: Returns name if named layer is correctly created.            ;;;
;;; EXAMPLE: (vlex-DefineLayer "MyLayer1" 3 "DASHED" T)                       ;;;
;;;***************************************************************************;;;

(defun vlex-DefineLayer
                                                (strName intColor strLtype booleCur / iloc obj out)
    (cond
        ((not (tblsearch "layer" strName))
         (setq obj (vla-add (vlex-LayerTable) strName))
         (setq iloc (vl-position strName (vlex-ListLayers)))
         (cond
             ((vlax-Write-Enabled-p obj)
                (if intColor
                    (vla-put-Color obj intColor)
                )
                (if strLtype
                    (vlex-Apply-Ltype obj strLtype)
                )
             )
             (T (princ "\nUnable to modify object properties..."))
         )      ; cond
         (if booleCur
             (vla-put-ActiveLayer
                 (vlex-ActiveDocument)
                 (vla-Item (vlex-LayerTable) iloc)
             )
         )
         (setq out strName)
        )
        (T
         (princ (strcat "\nLayer already exists: " strName))
        )
    )
    out
)


;;; Selection Sets -->>

;;;***************************************************************************;;;
;;; MODULE: vlex-SSetExists-p (Name)                                          ;;;
;;; NOTES: Boolean test if Selection Set <name> exists in drawing session     ;;;
;;;***************************************************************************;;;

(defun vlex-SSetExists-p (Name)
    (not
        (vl-Catch-All-Error-p
            (vl-Catch-All-Apply
                'vla-Item
                (list (vla-Get-SelectionSets (vlex-ActiveDocument)) Name)
            )
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SelectByType (objtype)                                       ;;;
;;; NOTES: Return Selection Set of Objects by type (string value)             ;;;
;;; EXAMPLE: (setq myset (vlex-SelectByType "CIRCLE"))                ;;;
;;;***************************************************************************;;;

(defun vlex-SelectByType (objtype / ss)
    (if (vlex-SSetExists-p "%TEMP_SET")
        (vla-Delete
            (vla-Item
                (vla-get-SelectionSets (vlex-ActiveDocument))
                "%TEMP_SET"
            )
        )
    )
    (setq   ss
                 (vla-Add
                     (vla-get-SelectionSets (vlex-ActiveDocument))
                     "%TEMP_SET"
                 )
    )
    (vla-Select
        ss
        ACSelectionSetAll
        nil
        nil
        (vlex-IntList->VarArray (list 0))
        (vlex-VarList->VarArray (list objtype))
    )
    ss
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SelectOnScreen-Filter (GroupCodes FilterLists)               ;;;
;;; NOTES: Return Selection Set by Filtering During On-Screen Selection       ;;;
;;;***************************************************************************;;;

(defun vlex-SelectOnScreen-Filter   (GroupCodes FilterLists / ss)
    (if (vlex-SSetExists-p "%TEMP_SET")
        (vla-Delete
            (vla-Item
                (vla-get-SelectionSets (vlex-ActiveDocument))
                "%TEMP_SET"
            )
        )
    )
    (setq   ss
                 (vla-Add
                     (vla-get-SelectionSets (vlex-ActiveDocument))
                     "%TEMP_SET"
                 )
    )
    (vla-Select
        ss
        ACSelectionSetAll
        nil
        nil
        (vlex-IntList->VarArray GroupCodes)
        (vlex-VarList->VarArray FilterLists)
    )
    ss
)

;;;***************************************************************************;;;
;;; MODULE: vlex-PICKCIRCLES                                                  ;;;
;;; NOTES: Return Selection Set of CIRCLEs on layer "0" only                  ;;;
;;;***************************************************************************;;;

(defun vlex-PICKCIRCLES ()
    (if
        (setq ss (vlex-SelectOnScreen-Filter '(0 8) '("CIRCLE" "0")))
         (vlax-For item ss
             (princ (vla-get-ObjectName item))
             (terpri)
         )
    )               ; if
    (terpri)
    ss
)

;;;***************************************************************************;;;
;;; MODULE: vlex-GETCIRCLES                                                   ;;;
;;; NOTES: Return Selection Set of CIRCLE Objects only                        ;;;
;;;***************************************************************************;;;

(defun C:GETCIRCLES ()
    (if (setq ss (vlex-SelectByType "CIRCLE"))
        (vlax-For   item ss
            (princ (vla-get-ObjectName item))
            (terpri)
        )
    )
    ss
)

;;; PROFILES . . . -->>

;;;***************************************************************************;;;
;;; MODULE: vlex-Profiles ()                                                  ;;;
;;; NOTES: Get Profiles collection object                                 ;;;
;;;***************************************************************************;;;

(defun vlex-Profiles ()
    (vla-get-Profiles (vlex-AcadPrefs))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileReLoad (name ARGname)                                 ;;;
;;; NOTES: Import profile from ARG to replace existing profile definition     ;;;
;;; EXAMPLE: (vlex-ProfileReLoad "profile1" "c:\\profiles\\profile1.arg")     ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileReLoad   (name ARGname)
    (cond
        ((= (vlax-get-property (vlex-Profiles) 'ActiveProfile) name)
                    ; or following code.
                    ;(= (vla-get-ActiveProfile (vlex-Profiles)) name)
         (princ "\nCannot delete a profile that is in use.")
        )           ;
        ((and
             (vlex-ProfileExists-p name)
             (findfile ARGname)
         )
         (vlex-ProfileDelete name)
         (vlex-ProfileImport name ARGname)
         (vla-put-ActiveProfile (vlex-Profiles) name)
        )           ;
        ((and
             (not (vlex-ProfileExists-p name))
             (findfile ARGname)
         )
         (vlex-ProfileImport name ARGname)
         (vla-put-ActiveProfile (vlex-Profiles) name)
        )           ;
        ((not (findfile ARGname))
         (princ (strcat "\nCannot locate ARG source: " ARGname))
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileExportX (pName ARGfile)                               ;;;
;;; NOTES: Export an existing profile to a new external .ARG file             ;;;
;;; EXAMPLE: (vlex-ProfileExportX "profile1" "c:/profiles/profile1.arg")      ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileExportX (pName ARGfile)
    (cond
        ((vlex-ProfileExists-p pName)
         (vlax-invoke-method
             (vlex-Profiles)
             'ExportProfile
             pName
             ARGfile
             (vlax-make-variant 1 :vlax-vbBoolean)
             ;; == TRUE
         )
        )           ;
        (T (princ "\nNo such profile exists to export."))
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileCopy (Name1 Name2)                                    ;;;
;;; NOTES: Copies an existing profile to a new profile                        ;;;
;;; EXAMPLE: (vlex-ProfileCopy pName newName)                                 ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileCopy (Name1 Name2)
    (cond
        ((and
             (vlex-ProfileExists-p Name1)
             (not (vlex-ProfileExists-p Name2))
         )
         (vlax-invoke-method
             (vlex-Profiles)
             'CopyProfile
             Name1
             Name2
         )
        )           ;
        ((not (vlex-ProfileExists-p Name1))
         (princ "\nError: No such profile exists.")
        )           ;
        ((vlex-ProfileExists-p Name2)
         (princ "\nProfile already exists, copy failed.")
        )
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileRename (oldName newName)                              ;;;
;;; NOTES: Renames an existing profile                                        ;;;
;;; EXAMPLE: (vlex-ProfileRename oldName newName)                             ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileRename   (oldName newName)
    (cond
        ((and
             (vlex-ProfileExists-p oldName)
             (not (vlex-ProfileExists-p newName))
         )
         (vlax-invoke-method
             (vlex-Profiles)
             'RenameProfile
             oldName
             newName
         )
        )           ;
        (T (princ))
        ;; add your error handling here?
    )               ; cond
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ProfileReset (strName)                                       ;;;
;;; NOTES: Reset given profile to default settings                            ;;;
;;; EXAMPLE: (vlex-ProfileReset "profile1")                                   ;;;
;;;***************************************************************************;;;

(defun vlex-ProfileReset (strName)
    (if (vlex-ProfileExists-p strName)
        (vlax-Invoke-Method
            (vlex-Profiles)
            'ResetProfile
            strName
        )
        (princ (strcat "\nProfile [" strName "] does not exist."))
    )               ; endif
)

;;; APPLICATION STATE . . . -->>
;;; These functions provide interaction with the AcadApplication object to enable
;;; control over the window state and visibility of the session object itself.

;;;***************************************************************************;;;
;;; MODULE: vlex-GetWindowState ()                                            ;;;
;;; NOTES: Get the AutoCAD application window state                           ;;;
;;;        enumerated constants (VB/VBA): acEnum 1=Min 2=Normal 3=Max         ;;;
;;; EXAMPLE: (vlex-GetWindowState) return 1, 2 or 3                           ;;;
;;;***************************************************************************;;;

(defun vlex-GetWindowState ()
    (vla-get-WindowState (vlex-AcadObject))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-SetWindowState ()                                            ;;;
;;; NOTES: Modify the AutoCAD application window state                        ;;;
;;;        enumerated constants (VB/VBA): acEnum 1=Min 2=Normal 3=Max         ;;;
;;; EXAMPLE: (vlex-SetWindowState 3) maximizes the window display             ;;;
;;;***************************************************************************;;;

(defun vlex-SetWindowState (acEnum)
    (vla-put-WindowState (vlex-AcadObject) acEnum)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-HideAutoCAD ()                                               ;;;
;;; NOTES: Hide AutoCAD application                                           ;;;
;;; EXAMPLE: (vlex-HideAutoCAD)                               ;;;
;;;***************************************************************************;;;

(defun vlex-HideAutoCAD ()
    (vla-put-Visible (vlex-AcadObject) :vlax-False)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ShowAutoCAD ()                                               ;;;
;;; NOTES: Display AutoCAD application (if hidden)                            ;;;
;;; EXAMPLE: (vlex-ShowAutoCAD)                               ;;;
;;;***************************************************************************;;;

(defun vlex-ShowAutoCAD ()
    (vla-put-Visible (vlex-AcadObject) :vlax-True)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-HideShowTest (delay-time)                                    ;;;
;;; NOTES: Temporarily Hides AutoCAD applicaiton to demonstrate the two       ;;;
;;;        functions given above. Time value is in milliseconds.          ;;;
;;; EXAMPLE: (vlex-HideShowTest 500)                              ;;;
;;;***************************************************************************;;;

(defun vlex-HideShowTest (delay-time)
    (vlex-HideAutoCAD)
    ;; hide AutoCAD...
    (vl-cmdf "delay" delay-time)
    ;; wait for <x> milliseconds...
    (vlex-ShowAutoCAD)
    ;; show AutoCAD again
)

;;;***************************************************************************;;;
;;; MODULE: vlex-DocPrefs ()                                              ;;;
;;; NOTES: Provides object access to Document/Database-Preferences collection ;;;
;;;***************************************************************************;;;

(defun vlex-DocPrefs ()
    (vla-get-Preferences (vlex-ActiveDocument))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-LWdisplayON/OFF ()                                       ;;;
;;; NOTES: Turn Lineweight Display setting ON or OFF                  ;;;
;;;***************************************************************************;;;

(defun vlex-LWdisplayON ()
    (vla-put-LineWeightDisplay (vlex-DocPrefs) :vlax-True)
)

(defun vlex-LWdisplayOFF ()
    (vla-put-LineWeightDisplay (vlex-DocPrefs) :vlax-False)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-ObjectSorBySnapON/OFF ()                             ;;;
;;; NOTES: Turn Object-Sort (SORTENTS) option for "sort by snap" ON or OFF    ;;;
;;;***************************************************************************;;;

(defun vlex-ObjectSortBySnapON ()
    (vla-put-ObjectSortBySnap (vlex-DocPrefs) :vlax-True)
)

(defun vlex-ObjectSortBySnapOFF ()
    (vla-put-ObjectSortBySnap (vlex-DocPrefs) :vlax-False)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-XrefEditON/OFF ()                                    ;;;
;;; NOTES: Turn XREF Editing option ON or OFF                     ;;;
;;;***************************************************************************;;;

(defun vlex-XrefEditON ()
    (vla-put-XrefEdit (vlex-DocPrefs) :vlax-True)
)

(defun vlex-XrefEditOFF ()
    (vla-put-XrefEdit (vlex-DocPrefs) :vla-False)
    ]
)

;;; Menus & Toolbars. . . -->>

;;;***************************************************************************;;;
;;; MODULE: vlex-MenuGroups ()                                        ;;;
;;; NOTES: Returns VLA-OBJECT for menugroups collection               ;;;
;;;***************************************************************************;;;

(defun vlex-MenuGroups ()
    (vla-get-menugroups (vlex-AcadObject))
)

;;;***************************************************************************;;;
;;; MODULE: vlex-MenuGroups-ListAll ()                                    ;;;
;;; NOTES: Returns a list of all defined menugroups names             ;;;
;;;***************************************************************************;;;

(defun vlex-MenuGroups-ListAll (/ out)
    (vlax-for   each (vlex-MenuGroups)
        (setq out (cons (vla-get-name each) out))
    )
    (reverse out)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-MenuGroup-Exists-p ()                                    ;;;
;;; NOTES: Returns ordinal position of menugroup name in collection(list) of  ;;;
;;;        all currently defined menugroups. If not found, returns 'nil'      ;;;
;;;***************************************************************************;;;

(defun vlex-MenuGroup-Exists-p (name)
    (if
        (member
            (strcase name)
            (mapcar 'strcase (vlex-MenuGroups-ListAll))
        )
         (vl-position name (vlex-MenuGroups-ListAll))
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbars (mgroup)                                    ;;;
;;; NOTES: Returns VLA-OBJECT(collection object) for all toolbars associated  ;;;
;;;        with a given menugroup. If menugroup is not found, returns nil.    ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbars (mgroup)
    (if (vlex-MenuGroup-Exists-p mgroup)
        (vla-get-toolbars
            (vla-item
                (vlex-MenuGroups)
                (vl-position
                    (strcase mgroup)
                    (mapcar 'strcase (vlex-MenuGroups-ListAll))
                )
            )
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbars-ListAll (mgroup)                                ;;;
;;; NOTES: Returns a list of all toolbar names for a given menugroup. If      ;;;
;;;        menugroup not found, or if no toolbars are found for menugroup,    ;;;
;;;        returns 'nil                                                       ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbars-ListAll (mgroup / tb out)
    (if (setq tb (vlex-Toolbars mgroup))
        (vlax-for   each tb
            (setq out (cons (vla-get-name each) out))
        )
    )
    (reverse out)
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbar-Exists-p (mgroup tbname)                         ;;;
;;; NOTES: Returns ordinal position of toolbar name with menugroup toolbars   ;;;
;;;        collection. If menugroup is not found, or if toolbar name is not   ;;;
;;;        found in collection, returns 'nil'.                                ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbar-Exists-p (mgroup tbname)
    (if
        (and
            (vlex-MenuGroup-Exists-p mgroup)
            (member
                (strcase tbname)
                (mapcar 'strcase (vlex-Toolbars-ListAll mgroup))
            )
        )
         (vl-position tbname (vlex-Toolbars-ListAll mgroup))
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbar (mgroup tbname / loc)                            ;;;
;;; NOTES: Returns VLA-OBJECT to given(named) toolbar within a given          ;;;
;;;        menugroup. If menugroup or toolbar not found, returns 'nil'.       ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbar (mgroup tbname / loc)
    (if (setq loc (vlex-Toolbar-Exists-p mgroup tbname))
        (vla-item (vlex-Toolbars mgroup) loc)
    )
)

;;;*****************************************************************************;;;
;;; MODULE: vlex-Toolbar-Show (mgroup tbname / tb)                          ;;;
;;; NOTES: Show a given toolbar(set "visible" to "TRUE"), given a menugroup     ;;;
;;; and toolbar name to apply this to. Returns T if successful, 'nil' otherwise.;;;
;;;*****************************************************************************;;;

(defun vlex-Toolbar-Show (mgroup tbname / tb)
    (if (setq tb (vlex-Toolbar mgroup tbname))
        (if (= (vla-get-visible tb) :vlax-false)
            (progn
                (vla-put-visible tb :vlax-true)
                T
            )
        )
    )
)

;;;*****************************************************************************;;;
;;; MODULE: vlex-Toolbar-Hide (mgroup tbname / tb)                          ;;;
;;; NOTES: Hide a given toolbar(set "visible" to "TRUE"), given a menugroup     ;;;
;;; and toolbar name to apply this to. Returns T if successful, 'nil' otherwise.;;;
;;;*****************************************************************************;;;

(defun vlex-Toolbar-Hide (mgroup tbname / tb)
    (if (setq tb (vlex-Toolbar mgroup tbname))
        (if (= (vla-get-visible tb) :vlax-true)
            (progn
                (vla-put-visible tb :vlax-false)
                T
            )
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbar-Dock (mgroup tbname dock)                            ;;;
;;; DESCRIPTION: Dock a given toolbar along TOP, BOTTOM, LEFT or RIGHT edged  ;;;
;;; of window.                                                                ;;;
;;; NOTES: Allowable <dock> values are 0(top), 1(bottom), 2(left),            ;;;
;;;        and 3(right). Returns 1 if successful, -1 if toolbar is not        ;;;
;;;        visible, -2 if parameter is invalid, or 0 if toolbar not found.    ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbar-Dock (mgroup tbname dock / tb)
    (if (setq tb (vlex-Toolbar mgroup tbname))
        (if (= (vla-get-visible tb) :vlax-true)
            (if (member dock '(0 1 2 3))
                (progn
                    (vlax-invoke-method tb 'Dock dock)
                    1
                )
                -2
                ;; invalid dockstatus parameter
            )
            -1
            ;; toolbar not visible
        )
        0
        ;; toolbar not found
    )
)

;;;***************************************************************************;;;
;;; MODULE: vlex-Toolbar-Folat (mgroup tbname top left rows)                  ;;;
;;; DESCRIPTION: Float a given toolbar at specified position(top and left)    ;;;
;;;   and display with specified number of rows. Returns 1 if successful,     ;;;
;;;   -1 if toolbar is not visible, 0 if toolbar is not found.                ;;;
;;;***************************************************************************;;;

(defun vlex-Toolbar-Folat   (mgroup tbname top left rows)
    (if (setq tb (vlex-Toolbar mgroup tbname))
        (if (= (vla-get-visible tb) :vlax-true)
            (progn
                (vlax-invoke-method tb 'Float top left rows)
                1
            )
            -1
            ;; toolbar not visible
        )
        0
        ;; toolbar not found
    )
)

;;; Reactors. . . -->>

;;; Summary of Reactor Types and Events                             ;;;
;;;     Reactor             Events                      ;;;
;;;     vlr-Dwg-Reactor         : vlr-BeginClose                    ;;;
;;;                 : vlr-BeginDwgOpen                  ;;;
;;;                 : vlr-BeginSave                     ;;;
;;;                 : vlr-EndDwgOpen                    ;;;
;;;                     : vlr-SaveComplete                  ;;;
;;;                                             ;;;
;;; vlr-Lisp-Reactor        : vlr-LispEnded                     ;;;
;;;                 : vlr-LispCancelled                 ;;;
;;;                 : vlr-LispWillStart (first line of lisp code string)    ;;;
;;;                                             ;;;
;;; vlr-Command-Reator      : vlr-CommandWillStart                  ;;;
;;;                 : vlr-CommandEnded                  ;;;
;;;                 : vlr-CommandCancelled                  ;;;
;;;                 : vlr-CommandFailed                 ;;;
;;;                                             ;;;
;;; vlr-Mouse-Reactor       : vlr-BeginDoubleClick                  ;;;
;;;                 : vlr-BeginRightClick                   ;;;

;;;     Other Reactor Types...
;;;     vlr-Object-Reactor      ;;;
;;;     vlr-Linker-Reactor      ;;;
;;;         vlr-AcDb-Reactor        ;;;
;;;     vlr-Editor-Reactor      ;;;
;;;     vlr-DXF-Reactor         ;;;
;;;     vlr-Undo-Reactor        ;;;
;;;     vlr-Toolbar-Reactor     ;;;
;;;     vlr-SysVar-Reactor      ;;;
;;;     vlr-Wblock-Reactor      ;;;
;;;     vlr-Window-Reactor      ;;;
;;;     vlr-Xref-Reactor        ;;;
;;;     vlr-Miscellaneous-Reactor   ;;;

;;;                                             ;;;
;;;                                             ;;;

;;;***************************************************************************;;;
;;; EXAMPLE: Function Examples using Command-Reactors and Dwg-Reactors        ;;;
;;;***************************************************************************;;;

;;;(vlr-command-reactor  ;; trap command events...
;;;  nil ; No data? yet?
;;;  ;; define call backs
;;;  '(
;;;    (:vlr-commandwillstart . TrapCommandStart)
;;;    (:vlr-commandended . TrapCommandEnded)
;;;    (:vlr-commandcancelled . TrapCommandCancelled)
;;;    (:vlr-commandfailed . TrapCommandFailed)
;;;  )
;;;)
;;;
;;;(vlr-dwg-reactor  ;; trap drawing session events...
;;;  nil ; No data? yet?
;;;  ;; define call backs
;;;  '(
;;;    (:vlr-beginclose . TrapBeginDwgClose)
;;;    (:vlr-beginsave . TrapBeginSave)
;;;    (:vlr-savecomplete . TrapSaveComplete)
;;;    (:vlr-begindwgopen . TrapBeginDwgOpen)
;;;    (:vlr-enddwgopen . TrapEndDwgOpen)
;;;  )
;;;)

;;; This is a vlr-command-reactor to commandWillStart                 ;;;
;;; It initializes currentCommandName global, used by other reactors          ;;;

(defun TrapCommandStart (reactor callbackData)
    ;; Reset all reactor globals
    (setq   #*someGlobal*   nil
                #*anotherGlobal* nil
                currentCommandName
                 (cond
                     ((car callbackData))
                     ((getvar "CMDNAMES"))
                 )
    )
    (cond
        ((= currentCommandName "PLOT")
;;; Do your stuff here, call another function, etc.
;;;
        )           ;
        ((= currentCommandName "PRINT")
;;; Do your stuff here...
        )           ;
        ((= (substr currentCommandName 1 3))
;;; Do your stuff here...
        )
        (T
;;;
         (prompt
             (strcat
                 "\nTesting "
                 currentCommandName
                 " CommandWillStart reactor..."
             )
         )
        )
        (T nil)
    )
    (princ)
)

;;; This is a good method for firing off routines when a drawing is closed.   ;;;
;;; I have used this to capture work information to save to database and      ;;;
;;; track contract hours per drawing, not just per Acad session.          ;;;

(defun TrapBeginDwgClose (reactor callbackData)
    ;; Reset all reactor globals to nil
    (setq   #*someGlobal*   nil
                #*anotherGlobal* nil
    )
    (cond
        (T
         (prompt (strcat currentCommandName " beginClose"))
         (CleanAllReactors)
        )
    )
    (princ)
)

;;; Remove all references to reactors from given event types...           ;;;

(defun CleanAllReactors ()
    (mapcar 'vlr-Remove-All
                    '(:vlr-acdb-reactor :vlr-dwg-reactor :vlr-command-reactor   :vlr-linker-reactor :vlr-object-reactor
                        :vlr-mouse-reactor :vlr-lisp-reactor)
    )
)

;;; Example using a simple command reactor to set a layer current whenever a  ;;;
;;; DIMENSION command is executed. It restores the previous layer after the   ;;;
;;; command completes or if the command either is cancelled or fails for some ;;;
;;; reason (other than a genarl AutoCAD failure like power loss).             ;;;

;;; funciton to define layer of given name.

;;; NOTES: You cannot issue a (command) or (vl-cmdf) function call within a   ;;;
;;;  command reactor. That would cause an infinite recursive loop. Therefore, ;;;
;;;  you should make sure you define the layer before or outside of the       ;;;
;;;  reator callback function and simply issue a layer-set operation in the   ;;;
;;;  callback function.                               ;;;

;;;(setq G$LAYC (vlex-DefineLayer "DIMENSIONS" nil nil nil))

;;; Make sure not to reload reactor attachments over top of each other!       ;;;

(defun vlex-Load-Command-Reactors   ()
    (if (null G$VLEX1)
        (progn
            (vlr-Command-Reactor
                ;; trap command events...
                nil ; No data? yet?
                ;; define call backs
                '
                 (
                    (:vlr-CommandWillStart . vlex-CommandStart)
                    (:vlr-CommandEnded . vlex-CommandEnded)
                    (:vlr-CommandCancelled . vlex-CommandCancelled)
                    (:vlr-CommandFailed . vlex-CommandFailed)
                 )
            )
            (setq G$VLEX1 T)
        )
    )
)

                    ;(vlex-Load-Command-Reactors)

;;;***************************************************************************;;;
;;;     React to command ending properly                                      ;;;
;;;***************************************************************************;;;

(defun vlex-CommandEnded (reactor callbackData)
    ;; Reset all reactor  globals
    (setq   #*someGlobal*   nil
                #*anotherGlobal* nil
                currentCommandName
                 (cond
                     ((car callbackData))
                     ((getvar "CMDNAMES"))
                 )
    )               ; setq
    (cond
        ((= "DIM" (substr currentCommandName 1 3))
         (M2k_RestoreLayer)
        )
        (T nil)
    )
    (princ)
)

;;;***************************************************************************;;;
;;;     Function to Restore Saved Layer                                       ;;;
;;;***************************************************************************;;;

(defun M2k_RestoreLayer ()
    (if G$LAYX
        (setvar "clayer" G$LAYX)
    )
    (setq G$LAYX nil)
)

;;;***************************************************************************;;;
;;;     React to command getting ready to execute                             ;;;
;;;***************************************************************************;;;

(defun TrapCommandStart (reactor callbackData)
    ;; Reset all reactor globals
    (setq   #*someGlobal*   nil
                #*anotherGlobal* nil
                currentCommandName
                 (cond
                     ((car callbackData))
                     ((getvar "CMDNAMES"))
                 )
    )               ; setq
    (cond
        ((= "DIM" (substr currentCommandName 1 3))
         (setq G$LAYX (getvar "clayer"))
         (cond
             ((and
                    G$LAYC
                    (tblsearch "layer" G$LAYC)
                )
                (setvar "clayer" G$LAYC)
             )
             (T (princ "\nLayer (DIMENSIONS) has not been defined."))
         )
        )
        (T nil)
    )
    (princ)
)

;;;***************************************************************************;;;
;;;     React to cancelled command                                            ;;;
;;;***************************************************************************;;;

(defun vlex-CommandCancelled (reactor callbackData)
    (M2k_RestoreLayer)
    (princ)
)

;;;***************************************************************************;;;
;;;     React to failed command                                               ;;;
;;;***************************************************************************;;;

(defun vlex-CommandFailed   (reactor callbackData)
    (M2k_RestoreLayer)
    (princ)
)

;;; Visual LISP Custom Functions. . . -->>

;;;***************************************************************************;;;
;;; MODULE:    ex:2DPoint (pt)                                                ;;;
;;; Purpose:   Converts an AutoLISP point into a 2D ActiveX point             ;;;
;;; Arguments: A point list (2D or 3D)                                        ;;;
;;; Example:   (ex:2DPoint (getpoint))                                        ;;;
;;;***************************************************************************;;;

(defun ex:2DPoint   (pt)
    (vl-load-com)
    (vlax-make-variant
        (vlax-safearray-fill
            (vlax-make-safearray vlax-vbdouble '(0 . 1))
            (list (car pt) (cadr pt))
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE:    ex:ActivateLastLayout ()                                       ;;;
;;; Purpose:   Activates the rightmost layout tab                             ;;;
;;; Arguments: None                                                   ;;;
;;; Example:                                      ;;;
;;; Notes:     None                               ;;;
;;; Debug:     nil                                ;;;
;;;***************************************************************************;;;

(defun ex:ActivateLastLayout (/ i layouts cnt layout)
    (vl-load-com)
    (setq   i               -1
                layouts (vla-get-layouts
                                    (vla-get-activedocument (vlax-get-acad-object))
                                )
                cnt         (1- (vla-get-count layouts))
    )               ; setq
    (vlax-for   layout layouts
        (if (= (vla-get-taborder layout) 1)
            (vla-activate layout)
        )
    )
)

;;;***************************************************************************;;;
;;; MODULE:    selectionsetToArray (ss / c r)                                 ;;;
;;; Purpose:   Returns an Variant array of subtype Object filled with the     ;;;
;;;            contents of a selection set                                    ;;;
;;; Example:   (selectionsetToArray mySS)                     ;;;
;;; Arguments: A selection set                            ;;;
;;; Notes:     1. Use this whenever you need to pass a selection set as an    ;;;
;;;               array to an ActiveX function                    ;;;
;;;            2. If you need a different subtype, simply change the reference;;;
;;;               to vlax-vbObject                        ;;;
;;; Debug:     nil                                ;;;
;;;***************************************************************************;;;

(defun selectionsetToArray (ss / c r)
    (vl-load-com)
    (setq c -1)
    (repeat (sslength ss)
        (setq r (cons (ssname ss (setq c (1+ c))) r))
    )
    (setq r (reverse r))
    (vlax-safearray-fill
        (vlax-make-safearray
            vlax-vbObject
            (cons 0 (1- (length r)))
        )
        (mapcar 'vlax-ename->vla-object r)
    )
)

;;;***************************************************************************;;;
;;; MODULE:    ex:AddObjectsToBlock (blk ss)                                  ;;;
;;; Purpose:   Adds a selection set of objects to an existing block definition;;;
;;; Arguments: The entity name of a block insert and a selection set          ;;;
;;; Example:   (ex:AddObjectsToBlock (car (entsel)) (ssget))              ;;;
;;; Notes:     Existing block references will not show a change until you     ;;;
;;;            regen the drawing                          ;;;
;;; Debug:     T                                  ;;;
;;;***************************************************************************;;;

(defun ex:AddObjectsToBlock (blk ss / doc blkref blkdef inspt refpt)
    (vl-load-com)
    (setq   doc         (vla-get-ActiveDocument (vlax-get-acad-object))
                blkref  (vlax-ename->vla-object blk)
                blkdef  (vla-Item (vla-get-Blocks doc) (vla-get-Name blkref))
                inspt       (vlax-variant-value (vla-get-InsertionPoint blkref))
                ssarray (selectionsetToArray ss)
                refpt       (vlax-3d-point '(0 0 0))
    )
    (foreach ent (vlax-safearray->list ssarray)
        (vla-Move ent inspt refpt)
    )
    (vla-CopyObjects doc ssarray blkdef)
    (foreach ent (vlax-safearray->list ssarray)
        (vla-Delete ent)
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:MappedShare (share / fso drives drive letter)                          ;;;
;;; Purpose:   Returns the logical drive letter to which a network share is mapped       ;;;
;;; Arguments: A UNC path                                        ;;;
;;; Example:   (ex:MappedShare "\\\\MyServer\\MyShare")                              ;;;
;;; Notes:     1. Be sure to substitute two backslashes for every one in the UNC path    ;;;
;;;            2. This routine requires the use SCRRUN.DLL. Visite the                   ;;;
;;;               Microsoft scripting web site if you do not have it.                    ;;;
;;; Debug:     nil                                           ;;;
;;;**************************************************************************************;;;

(defun ex:MappedShare   (share / fso drives drive letter)
    (vl-load-com)
    (setq fso (vlax-create-object "Scripting.FileSystemObject"))
    (vlax-for   drive   (setq drives (vlax-get-property fso 'Drives))
        (if (= (strcase (vlax-get-property drive 'ShareName))
                     (strcase share)
                )
            (setq letter (vlax-get-property drive 'DriveLetter))
        )
    )
    (vlax-release-object drives)
    (vlax-release-object fso)
    letter
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:BuildFilter (filter)                                       ;;;
;;; Purpose:   Returns a list containing a pair of variants for use as           ;;;
;;;            ActiveX selection set filters                         ;;;
;;; Arguments: A UNC path                                        ;;;
;;; Example:   (ex:BuildFilter '((0 . "LWPOLYLINE") (8 . "WALLS")))                      ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:BuildFilter   (filter)
    (vl-load-com)
    (mapcar '(lambda (lst typ)
                         (vlax-make-variant
                             (vlax-safearray-fill
                                 (vlax-make-safearray
                                     typ
                                     (cons 0
                                                 (1- (length lst))
                                     )
                                 )
                                 lst
                             )
                         )
                     )
                    (list (mapcar 'car filter) (mapcar 'cdr filter))
                    (list vlax-vbInteger vlax-vbVariant)
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:Centroid (poly / pl ms va reg cen)                                 ;;;
;;; Purpose:   Returns the centroid of a closed polyline                     ;;;
;;; Arguments: The entity name of a closed, planar polyline                      ;;;
;;; Example:   (ex:Centroid (car (entsel)))                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:Centroid (poly / pl ms va reg cen)
    (vl-load-com)
    (setq   pl (vlax-ename->vla-object poly)
                ms (vla-get-modelspace
                         (vla-get-activedocument (vlax-get-acad-object))
                     )
                va (vlax-make-safearray vlax-vbObject '(0 . 0))
    )
    (vlax-safearray-put-element va 0 pl)
    (setq   reg (car (vlax-safearray->list
                                     (vlax-variant-value (vla-addregion ms va))
                                 )
                        )
                cen (vla-get-centroid reg)
    )
    (vla-delete reg)
    (vlax-safearray->list (vlax-variant-value cen))
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ChangeAttributes (lst / item atts)                                 ;;;
;;; Purpose:   Modifies the specified attribute in the specified block reference         ;;;
;;; Arguments: A list containing one atom and one or more dotted pairs.              ;;;
;;;            The atom is the entity name of the block to change.           ;;;
;;;            The dotted pairs consist of the attribute tag and the new value for that attribute.
;;; Example:   (ex:ChangeAttributes (list ename '("myAttribute" . "NewValue")))          ;;;
;;; Notes:     1. Thanks to Chuck Balmer for spotting the bug in this routine.       ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ChangeAttributes (lst / item atts)
    (vl-load-com)
    (if (safearray-value
                (setq   atts
                             (vlax-variant-value
                                 (vla-getattributes (vlax-ename->vla-object (car lst)))
                             )
                )
            )
        (progn
            (foreach item   (cdr lst)
                (mapcar
                    '(lambda (x)
                         (if
                             (= (strcase (car item)) (strcase (vla-get-tagstring x)))
                                (vla-put-textstring x (cdr item))
                         )
                     )
                    (vlax-safearray->list atts)
                )
            )
            (vla-update (vlax-ename->vla-object (car lst)))
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ChangeBitmap (mnuGroup tbrName btnName bitmap)                         ;;;
;;; Purpose:   Changes the button top for the specified toobar button            ;;;
;;; Arguments: The name of the menu group, the name of the toolbar,              ;;;
;;;            the name of the toolbar button and the bitmap to use          ;;;
;;; Example:   (ex:ChangeBitmap "acad" "dimension" "linear dimension" "test.bmp")        ;;;
;;; Notes:     1. If the bitmap is not in the AutoCAD search path, you must specify      ;;;
;;;               the full path to file                          ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ChangeBitmap (mnuGroup tbrName btnName bitmap)
    (vl-load-com)
    (vla-setbitmaps
        (vla-item
            (vla-item
                (vla-get-toolbars
                    (vla-item   (vla-get-menugroups (vlax-get-acad-object))
                                        mnuGroup
                    )
                )
                tbrName
            )
            btnName
        )
        bitmap
        bitmap
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:CloseAll ()                                                ;;;
;;; Purpose:   Closes all open documents without saving                  ;;;
;;; Arguments: None                                  ;;;
;;; Example:                                             ;;;
;;; Notes:                                               ;;;
;;; Author:    Frank Whaley                              ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:CloseAll (/ item cur)
    (vl-load-com)
    (vlax-for   item (vla-get-documents (vlax-get-acad-object))
        (if (= (vla-get-active item) :vlax-false)
            (vla-close item :vlax-false)
            (setq cur item)
        )
    )
    (vla-sendcommand cur "_.CLOSE")
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:DeleteObjectFromBlock (ent)                                    ;;;
;;; Purpose:   Deletes the specified subentity from a block definition and returns the   ;;;
;;;            remaining of items in that block definition               ;;;
;;; Arguments: The entity name of the subentity to delete                ;;;
;;; Example:   (ex:DeleteObjectFromBlock (car (nentsel)))                    ;;;
;;; Notes:     1. As shown, you can use the NENTSEL function to obtain the name of an entity within a block.
;;;            2. Existing block reference will not show a change until you regen the drawing.
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:DeleteObjectFromBlock (ent / doc blk)
    (vl-load-com)
    (setq   doc (vla-get-ActiveDocument (vlax-get-acad-object))
                ent (vlax-ename->vla-object ent)
                blk (vla-ObjectIdToObject doc (vla-get-OwnerID ent))
    )
    (vla-Delete ent)
    (vla-get-Count blk)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:DrawVpBorder (vp / ll ur coords pl)                                ;;;
;;; Purpose:   Draws a rectangle representing the area displayed by a paper space viewport ;;;
;;; Arguments: The entity name of a paper space view port                ;;;
;;; Example:   (ex:DrawVpBorder (car (entsel)))                          ;;;
;;; Notes:     1. The return value is the entity name of the newly created lwpolyline    ;;;
;;;            2. The layout containing the viewport to be drawn must be active      ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:DrawVpBorder (vp / ll ur coords pl)
    (vl-load-com)
    (setq vp (vlax-ename->vla-object vp))
    (vla-GetBoundingBox vp 'll 'ur)
    (setq   ll       (trans (vlax-safearray->list ll) 3 2)
                ur       (trans (vlax-safearray->list ur) 3 2)
                coords (vlax-safearray-fill
                                 (vlax-make-safearray vlax-vbDouble (cons 0 7))
                                 (list (nth 0 ll)
                                             (nth 1 ll)
                                             (nth 0 ur)
                                             (nth 1 ll)
                                             (nth 0 ur)
                                             (nth 1 ur)
                                             (nth 0 ll)
                                             (nth 1 ur)
                                 )
                             )
    )
    (vla-put-closed
        (setq   pl (vla-AddLightWeightPolyline
                             (vla-get-ModelSpace (vla-get-Document vp))
                             coords
                         )
        )
        :vlax-true
    )
    (vlax-vla-object->ename pl)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:DriveType (drv)                                        ;;;
;;; Purpose:   Returns a string identifying the type of drive specified                  ;;;
;;; Arguments: A drive letter                                ;;;
;;; Example:   (mapcar 'ex:DriveType (ex:ListDrives))                        ;;;
;;; Notes:     1. This routine requires the use SCRRUN.DLL.                  ;;;
;;;               Visit the Microsoft scripting web site if you do not have it.      ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:DriveType (drv / fso drives drive typ)
    (vl-load-com)
    (setq fso (vlax-create-object "Scripting.FileSystemObject"))
    (if (vlax-invoke-method fso 'DriveExists drv)
        (progn
            (setq   drives (vlax-get-property fso 'Drives)
                        drive    (vlax-get-property drives 'Item drv)
                        typ      (vlax-get-property drive 'DriveType)
            )
            (vlax-release-object drive)
            (vlax-release-object drives)
            (vlax-release-object fso)
            (nth typ
                     '("UNKNOWN" "REMOVABLE" "FIXED" "REMODTE" "CDROM" "RAMDISK")
            )
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ListDrives (drv)                                           ;;;
;;; Purpose:   Returns a list containing all logical drives currently defined            ;;;
;;; Arguments: None                                  ;;;
;;; Example:                                             ;;;
;;; Notes:     1. This routine requires the use SCRRUN.DLL.                  ;;;
;;;               Visit the Microsoft scripting web site if you do not have it.      ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ListDrives (/ fso drive drives lst)
    (vl-load-com)
    (setq fso (vlax-create-object "Scripting.FileSystemObject"))
    (vlax-for   drive   (setq drives (vlax-get-property fso 'Drives))
        (setq lst (cons (vlax-get-property drive 'DriveLetter) lst))
    )
    (vlax-release-object drives)
    (vlax-release-object fso)
    (reverse lst)
)


;;;**************************************************************************************;;;
;;; MODULE:    ex:Parse (str delim / lst pos)                                ;;;
;;; Purpose:   Returns a list containing all tokens in a delimited string                ;;;
;;; Arguments: A delimited string and the delimiter character.               ;;;
;;; Example:   (ex:Parse (getenv "ACAD") ";")                            ;;;
;;; Notes:     1. AutoLISP does not correctly interpret any character code outside the   ;;;
;;;               range of 1 to 255, so you cannot parse a null delimited string.        ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:Parse (str delim / lst pos token)
    (setq pos (vl-string-search delim str))
    (while pos
        (setq   lst (cons
                                (if (= (setq token (substr str 1 pos)) delim)
                                    nil
                                    token
                                )
                                lst
                            )
                    str (substr str (+ pos 2))
                    pos (vl-string-search delim str)
        )
    )
    (if (> (strlen str) 0)
        (setq lst (cons str lst))
    )
    (reverse lst)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ExportProject (pName fName)                                ;;;
;;; Purpose:   Exports the specified project to disk                             ;;;
;;; Arguments: The name of a project and the full path to a file             ;;;
;;; Example:   (ex:ExportProject "Johnson" "c:\\temp\\project.txt")              ;;;
;;; Notes:     1. If the specified file exists, it will be overwritten           ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ExportProject (pName fName / fh prj)
    (vl-load-com)
    (setq fh (open fName "w"))
    (if (setq   prj (vl-registry-read
                                    (strcat "HKEY_CURRENT_USER\\"
                                                    (vlax-product-key)
                                                    "\\Profiles\\"
                                                    (getvar "CPROFILE")
                                                    "\\Project Settings\\"
                                                    pName
                                    )
                                    "RefSearchPath"
                                )
            )
        (progn
            (write-line (strcat "[" pName "]") fh)
            (foreach folder
                                            (ex:Parse prj ";")
                (write-line folder fh)
            )
        )
        (princ "\nThe specified windows registry key is not exists."
        )
    )
    (close fh)
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ImporttProject (fName)                                 ;;;
;;; Purpose:   Imports a project exported by ex:ExportProject                        ;;;
;;; Arguments: The full path to a file containing an exported project            ;;;
;;; Example:   (ex:ImportProject "c:\\temp\\project.txt")                    ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ImportProject (fName / pName fh l lst)
    (vl-load-com)
    (if (setq fh (open fName "r"))
        (progn
            (setq   pName   (read-line fh)
                        pName   (substr pName 2 (- (strlen pName) 2))
                        lst     ""
            )
            (while (setq l (read-line fh))
                (setq lst (strcat lst l ";"))
            )
            (vl-registry-write
                (strcat "HKEY_CURRENT_USER\\"
                                (vlax-product-key)
                                "\\Profiles\\"
                                (getvar "CPROFILE")
                                "\\Project Settings\\"
                                pName
                )
                "RefSearchPath"
                (substr lst 1 (1- (strlen lst)))
            )
            (close fh)
        )
    )
    (princ)
)




;;;**************************************************************************************;;;
;;; MODULE:    ex:GetAttributes (ent)                                    ;;;
;;; Purpose:   Returns a list of attribute tags, associated values and entity names      ;;;
;;; Arguments: The entity name os an attributed block                    ;;;
;;; Example:   (ex:GetAttributes (car (entsel))                          ;;;
;;; Notes:     1. You can use the entity name in each sublist to update a given attribute;;;
;;;            2. If there are no editable attributes in the given block, this function returns nil.
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetAttributes (ent / lst)
    (vl-load-com)
    (if (safearray-value
                (setq   lst
                             (vlax-variant-value
                                 (vla-getattributes
                                     (vlax-ename->vla-object ent)
                                 )
                             )
                )
            )
        (mapcar
            '(lambda (x)
                 (list
                     (vla-get-tagstring x)
                     (vla-get-textstring x)
                     (vlax-vla-object->ename x)
                 )
             )
            (vlax-safearray->list lst)
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetBoundingBox (ent)                                   ;;;
;;; Purpose:   Returns the extents of an individual entity               ;;;
;;; Arguments: An entity name                                ;;;
;;; Example:   (ex:GetBoundingBox (car (entsel)))                        ;;;
;;; Notes:     1. Do not use this routine wity XLINES or RAYS                ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetBoundingBox (ent / ll ur)
    (vl-load-com)
    (vla-getboundingbox (vlax-ename->vla-object ent) 'll 'ur)
    (mapcar 'vlax-safearray->list (list ll ur))
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetConstantAttributes (ent)                                ;;;
;;; Purpose:   Returns a list of constant attributes tags and their values       ;;;
;;; Arguments: The entity name of a block with constant attributes           ;;;
;;; Example:   (ex:GetConstantAttributes (car (entsel)))                     ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetConstantAttributes (ent / atts)
    (vl-load-com)
    (cond
        ((and   (safearray-value
                        (setq   atts
                                     (vlax-variant-value
                                         (vla-getconstantattributes
                                             (vlax-ename->vla-object ent)
                                         )
                                     )
                        )
                    )
         )
         (mapcar
             '(lambda   (x)
                    (cons (vla-get-tagstring x) (vla-get-textstring x))
                )
             (vlax-safearray->list atts)
         )
        )           ;
        (T
         (princ
             (strcat
                 "\nThe block reference \""
                 (vla-get-Name (vlax-ename->vla-object ent))
                 "\" doesn't include constant attributes tags and their values"
             )
         )
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetCurveLength (ent)                                   ;;;
;;; Purpose:   Returns the length of a curve                         ;;;
;;; Arguments: The entity name of a line, arc, circle, polyline (heavy or lightweight).  ;;;
;;; Example:   (ex:GetCurveLength (car (entsel)))                        ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetCurveLength (curve /)
    (vl-load-com)
    (setq curve (vlax-ename->vla-object curve))
    (vlax-curve-getDistAtParam
        curve
        (vlax-curve-getEndParam curve)
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetFileSize (fileName)                                     ;;;
;;; Purpose:   Returns the size of the specified file in bytes                   ;;;
;;; Arguments: A string specifying the full path to a file               ;;;
;;; Example:   (ex:GetFileSize "c:\\autoexec.bat")                       ;;;
;;; Notes:     1. There are reports of VL-FILE-SIZE and ACET-FILE-SIZE malfunction on    ;;;
;;;               Win2K systems. Use this as a substitute. It requires SCRRUN.DLL.   ;;;
;;;       Visit the Microsoft scripting web site if you do not have it.      ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetFileSize   (fileName / fso file size)
    (vl-load-com)
    (if (findfile fileName)
        (progn
            (setq   fso  (vlax-create-object "Scripting.FileSystemObject")
                        file (vlax-invoke-method fso 'GetFile fileName)
                        size (vlax-variant-value (vlax-get-property file 'Size))
            )
            (vlax-release-object file)
            (vlax-release-object fso)
        )
    )
    size
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetLastHeight (style)                                      ;;;
;;; Purpose:   Returns the last height used for a given text style               ;;;
;;; Arguments: The name of a text style                          ;;;
;;; Example:   (ex:SetLastHeight "standard" (* (ex:GetLastHeight "standard") 2.0))   ;;;
;;; Notes:     1. The example sets the Standard text style height to twice whatever it was before ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetLastHeight (style)
    (vl-load-com)
    (vla-get-LastHeight
        (vla-Item
            (vla-get-TextStyles
                (vla-get-ActiveDocument (vlax-get-acad-object))
            )
            style
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:SetLastHeight (style height)                               ;;;
;;; Purpose:   Sets the default height for a variable-height text style              ;;;
;;; Arguments: The name of a text style whose height is 0 and a double indicating the    ;;;
;;;            default height to be used the next time a text command is invoke      ;;;
;;; Example:   (ex:SetLastHeight "standard" 2.5)                     ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:SetLastHeight (style height)
    (vl-load-com)
    (vla-put-LastHeight
        (vla-Item
            (vla-get-TextStyles
                (vla-get-ActiveDocument (vlax-get-acad-object))
            )
            style
        )
        height
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetParentBlocks (blkName / doc)                            ;;;
;;; Purpose:   Returns a list conaining the entity names of any block definitions that   ;;;
;;;            reference the specified block                         ;;;
;;; Arguments: A string identifying the block to search for              ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetParentBlocks   (blkName / doc)
    (vl-load-com)
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (apply
        'append
        (mapcar
            '(lambda (x)
                 (if (= :vlax-false
                                (vla-get-IsLayout
                                    (vla-ObjectIdToObject
                                        doc
                                        (vla-get-OwnerId (vlax-ename->vla-object x))
                                    )
                                )
                         )
                     (list x)
                 )
             )
            (ex:ListBLockRefs blkName)
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ListBlockRefs (blkName / lst)                              ;;;
;;; Purpose:   Returns a list conaining the entity names of every reference to the specified block   ;;;
;;; Arguments: A string identifying the block to search for              ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ListBLockRefs (blkName / lst)
    (setq   lst (entget
                            (cdr
                                (assoc 330 (entget (tblobjname "block" blkName)))
                            )
                        )
    )
    (apply
        'append
        (mapcar '(lambda (x)
                             (if (entget (cdr x))
                                 (list (cdr x))
                             )
                         )
                        (repeat 2
                            (setq lst (reverse (cdr (member (assoc 102 lst) lst))))
                        )
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetPlotDevices ()                                  ;;;
;;; Purpose:   Returns a list containing all available plot devices              ;;;
;;; Arguments: None                                  ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetPlotDevices ()
    (vl-load-com)
    (vlax-safearray->list
        (vlax-variant-value
            (vla-getplotdevicenames
                (vla-item   (vla-get-layouts
                                        (vla-get-activedocument (vlax-get-acad-object))
                                    )
                                    "Model"
                )
            )
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:PetXData (vlaObj XData)                                ;;;
;;; Purpose:   Attach Extended Entity Data to an AutoCAD object.                 ;;;
;;; Arguments: An ActiveX object and an Extended Entity Data list in the same format as  ;;;
;;;            returned by GetXData.                             ;;;
;;; Example:   (ex:PutXData myVlaObj '((1001 . "ACADX") (1000 . "myStringData")))    ;;;
;;; Notes:     The Extended Entity Data application names as noted in the 1001 group     ;;;
;;;            code must be registered with the AutoLISP function REGAPP prior to        ;;;
;;;            attaching data to an object. See the AutoCAD help files for valid Extended;;;
;;;            Entity Data codes and values.                             ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:PutXData (vlaObj XData)
    (setq   XData
                 (ex:BuildFilter
                     (mapcar
                         '(lambda   (item / key)
                                (setq key (car item))
                                (if (<= 1010 key 1033)
                                    (cons   key
                                                (vlax-variant-value
                                                    (vlax-3d-point
                                                        (cdr item)
                                                    )
                                                )
                                    )
                                    item
                                )
                            )
                         XData
                     )
                 )
    )
    (vla-setXData vlaObj (car XData) (cadr XData))
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:lisp-value (v)                                 ;;;
;;; Purpose:   Returns the LISP value of an ActiveX variant.                     ;;;
;;; Arguments: An ActiveX variant or safearray.                          ;;;
;;; Example:   (ex:lisp-value myVariant)                         ;;;
;;; Notes:     This function will recursively dig into a safearray and convert all   ;;;
;;;            values, including nested safearray's, into a LISP value.          ;;;
;;; Author:    Vladimir Nesterovsky 2002                         ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:lisp-value (v)
    (cond
        ((= (type v) 'variant)
         (ex:lisp-value (variant-value v))
        )
        ((= (type v) 'safearray)
         (mapcar 'ex:lisp-value (safearray-value v))
        )
        (T v)
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:GetXData (vlaObj AppID)                            ;;;
;;; Purpose:   Get Extended Entity Data attached to an AutoCAD object.               ;;;
;;; Arguments: An ActiveX object and an application name that has been registed with     ;;;
;;;            the AutoLISP function REGAPP.                         ;;;
;;; Example:   (ex:GetXData myVlaObj "ACADX")                        ;;;
;;; Notes:     Returns a list of Extended Entity Data attached to the object.        ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:GetXData (vlaObj AppID / xType XData)
    (vla-getxdata vlaObj AppID 'xType 'xData)
    (mapcar '(lambda (key val) (cons key (ex:lisp-value val)))
                    (vlax-safearray->list xType)
                    (vlax-safearray->list xData)
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:LabelArea (ent)                                ;;;
;;; Purpose:   Creates a text entity that reports the area of a given entity.            ;;;
;;; Arguments: The entity name of any object that supports the Area property         ;;;
;;;            (Arc, Circle, Ellipse, LWPolyline, Polyline, Region or Spline)        ;;;
;;; Example:   (ex:LabelArea (car (entsel)))                         ;;;
;;; Notes:     1. The first time an entity is labeled, the text will appear at the       ;;;
;;;               entity's start point or center point                   ;;;
;;;            2. Call ex:LabelArea again to update a label. The label will update   ;;;
;;;       regardless of its current position                     ;;;
;;;            3. The are is formatted in the current units              ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:LabelArea (ent / elist xdata text start area)
    (vl-load-com)
    (regapp "LABELAREA")
    (setq   elist   (entget ent '("LABELAREA"))
                xdata   (assoc -3 elist)
                text    (if xdata
                                (entget (handent (cdr (cadadr xdata))))
                            )
                start   (if (not text)
                                (cdr (assoc 10 elist))
                            )
                area    (vla-get-area (setq ent (vlax-ename->vla-object ent)))
    )
    (if (not text)
        (progn
            (setq
                text (vla-addtext
                             (vla-get-block
                                 (vla-item
                                     (vla-get-layouts
                                         (vla-get-activedocument (vlax-get-acad-object))
                                     )
                                     (cdr (assoc 410 elist))
                                 )
                             )
                             (rtos area)
                             (vlax-3d-point start)
                             0.25
                         )
            )
        )
        (vla-put-textstring
            (setq text (vlax-ename->vla-object (cdr (assoc -1 text))))
            (rtos area)
        )
    )
    (vla-setxdata
        ent
        (vlax-make-variant
            (vlax-safearray-fill
                (vlax-make-safearray vlax-vbInteger '(0 . 1))
                '(1001 1005)
            )
        )
        (vlax-make-variant
            (vlax-safearray-fill
                (vlax-make-safearray vlax-vbVariant '(0 . 1))
                (list "LABELAREA" (vla-get-handle text))
            )
        )
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:LabelOrdinate (ss attname ordinate)                        ;;;
;;; Purpose:   Cycles through a selection set filling a specified attribute with a       ;;;
;;;        block's position (X, Y or Z)                      ;;;
;;; Arguments: A selection set containing blocks to label, the name of the attribute to  ;;;
;;;            change and an integer indicating which ordinate value to use      ;;;
;;;     (0=X, 1=Y, 2=Z)                              ;;;
;;; Example:   (ex:LabelOrdinate ss "POS" 0)                         ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:LabelOrdinate (ss attname ordinate / c block atts val att)
    (vl-load-com)
    (setq c -1)
    (repeat (sslength ss)
        (setq   block   (vlax-ename->vla-object
                                    (ssname ss (setq c (1+ c)))
                                )
                    atts    (vlax-safearray->list
                                    (vlax-variant-value
                                        (vla-getattributes block)
                                    )
                                )
                    val     (rtos
                                    (nth ordinate
                                             (vlax-safearray->list
                                                 (vlax-variant-value
                                                     (vla-get-insertionpoint block)
                                                 )
                                             )
                                    )
                                    2
                                    0
                                )
        )
        (foreach att atts
            (if (= (strcase attname) (strcase (vla-get-tagstring att)))
                (vla-put-textstring att val)
            )
        )
        (vla-update block)
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ListLayouts ()                                 ;;;
;;; Purpose:   Returns a list containing all layouts in the current document             ;;;
;;; Arguments: None                                  ;;;
;;; Example:                                         ;;;
;;; Notes:     1. ex:ListLayouts returns a list of layout names sorted by tab order,     ;;;
;;;               not name like LAYOUTLIST                           ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ListDocuments (/ fname lst)
    (vl-load-com)
    (vlax-for   doc (vla-get-documents (vlax-get-acad-object))
        (setq
            lst (cons   (if (/= (setq fname (vla-get-fullname doc)) "")
                                    fname
                                    (vla-get-name doc)
                                )
                                lst
                    )
        )
    )
    (reverse lst)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ListDocuments ()                                   ;;;
;;; Purpose:   Returns a list containing the name or full path of every open document.   ;;;
;;; Arguments: None                                  ;;;
;;; Example:                                         ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ListLayouts   (/ layouts c lst lay)
    (vl-load-com)
    (setq   layouts (vla-get-layouts
                                    (vla-get-activedocument (vlax-get-acad-object))
                                )
                c               -1
    )
    (repeat (vla-get-count layouts)
        (setq lst (cons (setq c (1+ c)) lst))
    )
    (vlax-for   lay layouts
        (setq   lst
                     (subst
                         (vla-get-name lay)
                         (vla-get-taborder lay)
                         lst
                     )
        )
    )
    (reverse lst)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ListToolbars (groupName)                               ;;;
;;; Purpose:   Returns a list containing the name of every toolbar in the secified menu group  ;;;
;;; Arguments: A string containing the name of a currently loaded menu group         ;;;
;;; Example:   (ex:ListToolbars "acad")                          ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ListToolbars (groupName / mGroups mGroup lst)
    (vl-load-com)
    (if (not
                (vl-catch-all-error-p
                    (setq
                        mGroup (vl-catch-all-apply
                                         'vla-item
                                         (list (vla-get-menugroups (vlax-get-acad-object))
                                                     groupName
                                         )
                                     )
                    )
                )
            )
        (vlax-for   tBar (vla-get-toolbars mGroup)
            (setq lst (cons (vla-get-name tBar) lst))
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:MakeLayer (lName)                                      ;;;
;;; Purpose:   Create a new layer.                           ;;;
;;; Arguments: The new layer name                                ;;;
;;; Example:   (ex:MakeLayer "A-Wall")                           ;;;
;;; Notes:     Returns the new layer object on successful creation, an existing layer    ;;;
;;;            object if the layer already exists, or NIL if the layer name cannot be created.
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:MakeLayer (lName / oLayer)
    (vl-load-com)
    (if
        (vl-catch-all-error-p
            (setq   oLayer
                         (vl-catch-all-apply
                             'vla-add
                             (list
                                 (vla-get-layers
                                     (vla-get-activedocument
                                         (vlax-get-acad-object)
                                     )
                                 )
                                 lName
                             )
                         )
            )
        )
         nil
         oLayer
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:RenameLayout (oldName newName)                         ;;;
;;; Purpose:   Rename an existing layout                         ;;;
;;; Arguments: A string containing the name of the layout to renam and a string      ;;;
;;;            containing the new name for it                        ;;;
;;; Example:   (ex:RenameLayout "Layout1" "myLayout")                    ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:RenameLayout (oldName newName)
    (vl-load-com)
    (vla-put-name
        (vla-item   (vla-get-layouts
                                (vla-get-activedocument (vlax-get-acad-object))
                            )
                            oldName
        )
        newName
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:SelectAttributedBlocks (lst)                           ;;;
;;; Purpose:   Returns a selection set containing blocks whose attribute values match    ;;;
;;;            the specified criteria                            ;;;
;;; Arguments: A block name, the attribute tag which to search and the value being sought;;;
;;; Example:   (ex:SelectAttributedBlocks '("Window" "KeyNote" "57"))            ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:SelectAttributedBlocks (lst / ss ss2 c ent att)
    (vl-load-com)
    (if (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 (car lst)))))
        (progn
            (setq c 0)
            (repeat (sslength ss)
                (setq ent (vlax-ename->vla-object (ssname ss c)))
                (if (vla-get-hasattributes ent)
                    (foreach att (vlax-safearray->list
                                                 (vlax-variant-value (vla-getattributes ent))
                                             )
                        (if
                            (= (strcase (vla-get-tagstring att)) (strcase (cadr lst)))
                             (if (= (strcase (vla-get-textstring att))
                                            (strcase (caddr lst))
                                     )
                                 (progn
                                     (vla-highlight ent :vlax-true)
                                     (if (not ss2)
                                         (setq ss2 (ssadd (ssname ss c)))
                                         (ssadd (ssname ss c) ss2)
                                     )
                                 )
                             )
                        )
                    )
                )
                (setq c (1+ c))
            )
        )
    )
    ss2
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:SetProfile (pname)                                 ;;;
;;; Purpose:   Sets a profile active                             ;;;
;;; Arguments: The name of an existing profile                       ;;;
;;; Example:   (ex:SetProfile "MyProfile")                       ;;;
;;; Notes:     1. This cannot be used to initialize a "vertical" product from AutoCAD.   ;;;
;;;       In other words, you cannot start AutoCAD and switch to something like  ;;;
;;;       Mechanical Desktop.                            ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:SetProfile (pname)
    (vl-load-com)
    (vla-put-ActiveProfile
        (vla-get-Profiles
            (vla-get-Preferences
                (vlax-get-acad-object)
            )
        )
        pname
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ToggleLayouts ()                               ;;;
;;; Purpose:   Toggles the display of layout tabs                    ;;;
;;; Arguments: None                                  ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ToggleLayouts (/ prefDisplay)
    (vl-load-com)
    (setq   prefDisplay
                 (vla-get-Display
                     (vla-get-Preferences
                         (vlax-get-acad-object)
                     )
                 )
    )
    (vla-put-DisplayLayoutTabs
        prefDisplay
        (if (= (vla-get-DisplayLayoutTabs prefDisplay) :vlax-true)
            :vlax-false
            :vlax-true
        )
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:ToggleMSBackground ()                              ;;;
;;; Purpose:   Toggles the ModelSpace background color between black and white       ;;;
;;; Arguments: None                                  ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:ToggleMSBackground (/ prefDisplay)
    (vl-load-com)
    (setq   prefDisplay (vla-get-Display
                                            (vla-get-Preferences (vlax-get-acad-object))
                                        )
                color               (vlax-variant-value
                                            (vlax-variant-change-type
                                                (vla-get-GraphicsWinModelBackgrndColor prefDisplay)
                                                vlax-vbLong
                                            )
                                        )
    )
    (vla-put-GraphicsWinModelBackgrndColor
        prefDisplay
        (vlax-make-variant
            (if (= color 0)
                16777215
                0
            )
            vlax-vbLong
        )
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:TogglePSBackground ()                              ;;;
;;; Purpose:   Toggles the PaperSpace background color between black and white       ;;;
;;; Arguments: None                                  ;;;
;;; Example:   None                                  ;;;
;;; Notes:     None                                  ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:TogglePSBackground (/ prefDisplay)
    (vl-load-com)
    (setq   prefDisplay (vla-get-Display
                                            (vla-get-Preferences (vlax-get-acad-object))
                                        )
                color               (vlax-variant-value
                                            (vlax-variant-change-type
                                                (vla-get-GraphicsWinLayoutBackgrndColor prefDisplay)
                                                vlax-vbLong
                                            )
                                        )
    )
    (vla-put-GraphicsWinLayoutBackgrndColor
        prefDisplay
        (vlax-make-variant
            (if (= color 0)
                16777215
                0
            )
            vlax-vbLong
        )
    )
    (princ)
)

;;;**************************************************************************************;;;
;;; MODULE:    C:LayerFiltersDelete ()                               ;;;
;;; Purpose:   Delete all layer filters in the current drawing.              ;;;
;;; Arguments: None                                  ;;;
;;; Example:   Command: LayerFiltersDelete                       ;;;
;;;            --or--                                    ;;;
;;;            Command: LFD                              ;;;
;;; Notes:     I could not see doing this as anything other than a user command.     ;;;
;;;            But the original command names is too long to type in, hence the aliased version.
;;; Author:    R. Robert Bell                                ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun C:LayerFiltersDelete ()
    (vl-Load-Com)
    (vl-Catch-All-Apply
        '(lambda ()
             (vla-Remove
                 (vla-GetExtensionDictionary
                     (vla-Get-Layers
                         (vla-Get-ActiveDocument (vlax-Get-Acad-Object))
                     )
                 )
                 "ACAD_LAYERFILTERS"
             )
         )
    )
    (princ "\nAll layer filter have been deleted.")
    (princ)
)
(defun C:LFD () (C:LayerFiltersDelete))

;;;**************************************************************************************;;;
;;; MODULE:    ex:listToVariantArray (lst varType)                       ;;;
;;; Purpose:   Converts a list to an ActiveX variant array               ;;;
;;; Arguments: A list. The list can be nested up to one level deep.              ;;;
;;;                 e.g.: (list "1" 2 (list 1.0 2.0 3.0))                ;;;
;;; Example:   (listToVariantArray (list (list 2.0 3.0 0.0) 1 2.0 "String"))         ;;;
;;; Notes:     1. If your list includes various data types, pass vlax-vbVariant for the  ;;;
;;;               varType argument                           ;;;
;;;        2. Entity names are converted to ObjectIDs                ;;;
;;;        3. To convert a point list to ActiveX coordinates:            ;;;
;;;       (list->VariantArray (apply 'append ptlist) vlax-vbDouble)              ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:listToVariantArray (lst varType)
    (vl-load-com)
    (vlax-make-variant
        (vlax-safearray-fill
            (vlax-make-safearray varType (cons 0 (1- (length lst))))
            (mapcar
                '(lambda (x)
                     (cond
                         ((= (type x) 'list)
                            (vlax-safearray-fill
                                (vlax-make-safearray
                                    (if (apply '= (mapcar 'type x))
                                        (cond
                                            ((= (type (car x)) 'REAL) vlax-vbDouble)
                                            ((= (type (car x)) 'INT) vlax-vbInteger)
                                            ((= (type (car x)) 'STR) vlax-vbString)
                                        )
                                        vlax-vbVariant
                                    )
                                    (cons 0 (1- (length x)))
                                )
                                x
                            )
                         )
                         ((= (type x) 'ename)
                            (vla-get-objectid (vlax-ename->vla-object x))
                         )
                         (t x)
                     )
                 )
                lst
            )       ; mapcar
        )
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    ex:selectionsetToArray (ss)                           ;;;
;;; Purpose:   Returns an Variant array of subtype Object filled with the contents of    ;;;
;;;        a selection set.                              ;;;
;;; Arguments: A selection set                               ;;;
;;; Example:   (selectonsetToArray mySS)                         ;;;
;;; Notes:     1. Use this whenever you need to pass a selecton set as an array to an    ;;;
;;;               ActiveX function                           ;;;
;;;        2. If you need a different subtype, simply change the reference to    ;;;
;;;       vlax-vbObject.                             ;;;
;;; Debug:     T                                             ;;;
;;;**************************************************************************************;;;

(defun ex:selectionsetToArray   (ss / c r)
    (vl-load-com)
    (setq c -1)
    (repeat (sslength ss)
        (setq r (cons (ssname ss (setq c (1+ c))) r))
    )
    (setq r (reverse r))
    (vlax-safearray-fill
        (vlax-make-safearray
            vlax-vbObject
            (cons 0 (1- (length r)))
        )
        (mapcar 'vlax-ename->vla-object r)
    )
)

;;; Utilities... -->>

;;;**************************************************************************************;;;
;;; MODULE:    xlList->ListOfPoints (coordList / ptlist)                     ;;;
;;; Purpose:   Convert a list of X, Y values from a single list into a list of paired    ;;;
;;;        lists From (x y x y x y ...) into ((x y)(x y)(x y)...)            ;;;
;;; Notes:     This is necessary to convert the results of using (vla-Get-Coordinates)   ;;;
;;;        on a LwPolyline object into a list of vertext points          ;;;
;;; Source:    Taken from Garden Path tutorial                               ;;;
;;; Debug:     T                                     ;;;
;;;**************************************************************************************;;;

(defun xyList->ListOfPoints (coordList / ptlist)
    (while coordList
        (setq   ptlist      (append ptlist
                                                        (list
                                                            (list   (car coordList)
                                                                        (cadr coordList)
                                                            )
                                                        )
                                        )
                    coordList   (cddr coordList)
        )
    )
    ptlist
)

;;;**************************************************************************************;;;
;;; MODULE:    Is-Vla-Object (obj)                               ;;;
;;; Purpose:   Boolean test if data type is VLA-OBJECT                   ;;;
;;;**************************************************************************************;;;

(defun Is-Vla-Object (obj) (equal (type obj) 'vla-object))

;;;**************************************************************************************;;;
;;; MODULE:    Is-String (arg)                                   ;;;
;;; Purpose:   Boolean test if data type is STRing                   ;;;
;;;**************************************************************************************;;;

(defun Is-String (arg) (equal (type arg) 'str))

;;;**************************************************************************************;;;
;;; MODULE:    Is-Real (arg)                                     ;;;
;;; Purpose:   Boolean test if data type is REAL number(double, float, etc.)         ;;;
;;;**************************************************************************************;;;

(defun Is-Real (arg) (equal (type arg) 'real))

;;;**************************************************************************************;;;
;;; MODULE:    Is-Ename (arg)                                    ;;;
;;; Purpose:   Boolean test if data type is AutoCAD ENAME (entity name)          ;;;
;;;**************************************************************************************;;;

(defun Is-Ename (arg) (equal (type arg) 'ename))

;;;**************************************************************************************;;;
;;; MODULE:    Is-Variant (arg)                                  ;;;
;;; Purpose:   Boolean test if data type is VARIANT                  ;;;
;;;**************************************************************************************;;;

(defun Is-Variant (arg) (equal (type arg) 'variant))

;;;**************************************************************************************;;;
;;; MODULE:    vlex-MakeEname (object)                               ;;;
;;; Purpose:   Convert VLA-OBJECT into ENAME data type                   ;;;
;;;**************************************************************************************;;;

(defun vlex-MakeEname   (object)
    (if (Is-Vla-Object object)
        (vlax-vla-object->Ename object)
        object
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-MakeObject (object)                              ;;;
;;; Purpose:   Convert ENAME into VLA-OBJECT data type                   ;;;
;;;**************************************************************************************;;;

(defun vlex-MakeObject (object)
    (if (Is-Ename object)
        (vlax-Ename->vla-Object object)
        object
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    IntList->VarArray (aList)                             ;;;
;;; Purpose:   Convert a LIST of INTeger values into a VARIANT SAFE-ARRAY        ;;;
;;;**************************************************************************************;;;

(defun IntList->VarArray (aList)
    (vlax-SafeArray-Fill
        (vlax-Make-SafeArray
            vlax-vbInteger ; (2) Integer
            (cons 0 (- (length aList) 1))
        )
        aList
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    VarList->VarArray (aList)                             ;;;
;;; Purpose:   Convert a LIST of VARIANT values into a VARIANT SAFE-ARRAY        ;;;
;;;**************************************************************************************;;;

(defun VarList->VarArray (aList)
    (vlax-SafeArray-Fill
        (vlax-Make-Safearray
            vlax-vbVariant ; (12) Variant
            (cons 0 (- (length aList) 1))
        )
        aList
    )
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-ApplyLtypeGen (object)                           ;;;
;;; Purpose:   Apply Linetype Generation to LwPolyline Object                ;;;
;;;**************************************************************************************;;;

(defun vlex-ApplyLtypeGen   (object / obj)
    (setq object (vlex-MakeObject object))
    ;; make sure not Ename first!
    (vla-put-LinetypeGeneration object :vlax-True)
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-Put-ByLayer (obj)                                ;;;
;;; Purpose:   Put Object Color=ByLayer, Linetype=ByLayer                ;;;
;;;**************************************************************************************;;;

(defun vlex-Put-ByLayer (obj)
    (if (vlax-write-enabled-p obj)
        (progn
            (vla-put-Color obj 255)
                    ;(vla-put-Linetype obj ...);; <-- I need to figure this out!!!
        )
    )               ; endif
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-ActiveLayout ()                              ;;;
;;; Purpose:   Returns object to active layout                       ;;;
;;;**************************************************************************************;;;

(defun vlex-ActiveLayout ()
    (vla-get-ActiveLayout (vlex-ActiveDocument))
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-ActiveLayoutName ()                              ;;;
;;; Purpose:   Returns object name to active layout (string value)           ;;;
;;;**************************************************************************************;;;

(defun vlex-ActiveLayoutName ()
    (vla-get-Name (vlex-ActiveLayout))
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-ActivePlotConfig ()                              ;;;
;;; Purpose:   Returns object to active plot configuration               ;;;
;;; Debug:     nil                                   ;;;
;;;**************************************************************************************;;;

(defun vlex-PlotConfigs (/ pc out)
    (vlax-for   each (vlax-get-property
                                     (vlex-ActiveDocument)
                                     'PlotConfigurations
                                 )
        (if (vlax-property-available-p each 'GetPlotDeviceNames)
            (setq out (cons (vlax-get-property each 'GetPlotDeviceNames) out))
        )
        (setq   itemname (vlex-Name each)
                    out          (cons itemname out)
        )
    )
    out
)

;;;**************************************************************************************;;;
;;; MODULE:    vlex-SnapOff ()                                   ;;;
;;; Purpose:   Turns OFF Osnaps                              ;;;
;;; Debug:     T                                     ;;;
;;;**************************************************************************************;;;
(defun vlex-SnapOff ()
    (vla-put-ObjectSnapMode (vlex-ActiveDocument) :vlax-false)
)
;;;**************************************************************************************;;;
;;; MODULE:    vlex-SnapOn ()                                    ;;;
;;; Purpose:   Turns ON Osnaps                               ;;;
;;; Debug:     T                                     ;;;
;;;**************************************************************************************;;;
(defun vlex-SnapOn ()
    (vla-put-ObjectSnapMode (vlex-ActiveDocument) :vlax-true)
)
;;;**************************************************************************************;;;
;;; MODULE:    vlex-OpenDwg (fullname)                               ;;;
;;; Purpose:   Open named drawing file(no error trapping is done!            ;;;
;;; Debug:     T                                     ;;;
;;;**************************************************************************************;;;
(defun vlex-OpenDwg (fullname)
    (command "vbastmt"
                     (strcat "AcadApplication.Documents.Open "
                                     (chr 34)
                                     fullname
                                     (chr 34)
                     )
    )
)
;;;**************************************************************************************;;;
;;; MODULE:    vlex-DwgNamed-p ()                                ;;;
;;; Purpose:   Returns T if drawing has been saved with a name, otherwise returns 'nil'. ;;;
;;; Debug:     T                                     ;;;
;;;**************************************************************************************;;;
(defun vlex-DwgNamed-p ()
    (if (= 1 (getvar "dwgtitled"))
        T
        nil
    )
)


;;;**************************************************************************************;;;
;;; MODULE:    vlex-ActivePlotConfig ()                              ;;;
;;; Purpose:   Returns object to active plot configuration               ;;;
;;;**************************************************************************************;;;

;;; Zooming Functions... --->

(defun vlex-ZoomExtents ()
    (vla-ZoomExtents (vlex-AcadObject))
)
(defun vlex-ZoomAll () (vla-ZoomAll (vlex-AcadObject)))
(defun vlex-ZoomCenter (pt)
    (vla-ZoomCenter (vlex-AcadObject) (vlax-3d-point pt) 1.0)
)
(defun vlex-ZoomPrevious ()
    (vla-ZoomPrevious (vlex-AcadObject))
)
(defun vlex-ZoomWindow (p1 p2)
    (vla-ZoomWindow
        (vlex-AcadObject)
        (vlax-3d-point p1)
        (vlax-3d-point p2)
    )
)
(defun vlex-ZoomOut ()
    (vla-ZoomOut (vlex-AcadObject) 0.5 1)
)
(defun vlex-ZoomIn ()
    (vla-ZoomScaled (vlex-AcadObject) 2.0 1)
)



;;;***************************************************************************;;;
;;; MODULE: vlex-Help                                                         ;;;
;;; DESCRIPTION:                                                              ;;;
;;; ARGS:                                                                     ;;;
;;; EXAMPLE:                                                                  ;;;
;;;***************************************************************************;;;

(defun vlex-Help (/ cmd)
    (setq separator "///////////////////////////////////////")
    (foreach cmd
                             (list
                                 separator                                  "VLEX Constant Globals:"     "Acad-Object"
                                 "vlex-ActiveDocument"          "vlex-PaperSpace"                    "vlex-ActiveSpace"
                                 "vlex-ModelSpace"                  "vlex-AcadPrefs"                     separator
                                 "VLEX Exposed Functions:"
;;; Environment PROFILES 
                                 "vlex-GetPrefKey"                  "vlex-SetPrefKey"                    "vlex-ProfileImport"
                                 "vlex-ProfileExport"               "vlex-ProfileExists-p"       "vlex-ProfileDelete"
                                 "vlex-ProfileList"                 "vlex-Profiles"                      "vlex-ProfileReLoad"
                                 "vlex-ProfileExportX"          "vlex-ProfileCopy"               "vlex-ProfileRename"
                                 "vlex-ProfileReset"                separator
;;; Documents
                                 "vlex-CloseAllDocs"                "vlex-SaveAllDocs"               "vlex-SaveAs2000"
                                 "vlex-SaveAsR14"                       "vlex-Saved-p"                       "vlex-PurgeAllDocs"
                                 "vlex-GetDocsCollection"       "vlex-DocCollection"             "vlex-DocsCount"
                                 "vlex-DocsList"                        separator
;;; Properties
                                 "vlex-CopyProp"                        "vlex-MapPropertyList"       "vlex-ChangeAttributes"
                                 "vlex-GetAttributes"               separator
;;; String
                                 "vlex-ParseString"                 "vlex-Massoc"                            separator
;;; Undo
                                 "vlex-UndoBegin"                       "vlex-UndoEnd"                       separator
;;; Return List
                                 "vlex-Extents"                         "vlex-RectCenter"                    "vlex-Mid"
                                 "vlex-PolyCentroid"                "vlex-GetPolySegment"            "vlex-GetEllipseArcPoints"
                                 separator
;;; Object
                                 "vlex-AcadProp"                        "vlex-ObjectType"                    "vlex-MakeObject"
                                 "vlex-DeleteObject"                "vlex-DumpIt"                            "vlex-Name"
                                 "vlex-MxRelease"                       separator
;;; Collection
                                 "vlex-CollectionCount"         "vlex-CollectionList"            "vlex-AcadCollection"
                                 "vlex-MapCollection"               "vlex-DumpCollection"            separator
;;; Modify1
                                 "vlex-IsClosed"                        "vlex-CloseArc"                      separator
;;; Sort
                                 "vlex-SortPoints"                  separator
;;; LineType
                                 "vlex-CountLtypes"                 "vlex-Ltype-Exists-p"            separator
;;; vlex-GetXXX
                                 "vlex-GetLayers"                       "vlex-GetLtypes"                     "vlex-GetTextStyles"
                                 "vlex-GetDimStyles"                "vlex-GetLayouts"                    "vlex-GetDictionaries"
                                 "vlex-GetBlocks"                       "vlex-GetPlotConfigs"            "vlex-GetViews"
                                 "vlex-GetViewPorts"                "vlex-GetGroups"                     "vlex-GetRegApps"
                                 separator
;;; vlex-ListXXX
                                 "vlex-ListLayers"                  "vlex-ListLtypes"                    "vlex-ListTextStyles"
                                 "vlex-ListDimStyles"               "vlex-ListLayouts"               "vlex-ListDictionaries"
                                 "vlex-ListBlocks"                  "vlex-ListPlotConfigs"       "vlex-ListViews"
                                 "vlex-ListViewPorts"               "vlex-ListGroups"                    "vlex-ListRegApps"
                                 separator
;;; Create Entity
                                 "vlex-AddArc"                          "vlex-AddCircle"                     "vlex-AddLine"
                                 "vlex-AddLineC"                        "vlex-AddPline"                      "vlex-AddEllipse"
                                 "vlex-AddEllipseArc1"          "vlex-AddEllipseArc2"            "vlex-AddRectangle"
                                 "vlex-AddPolygon"                  "vlex-Apply-Ltype"               "vlex-Apply-LtScale"
                                 "vlex-AddPolygon"                  "vlex-AddRectangle"              "vlex-AddSolid"
                                 separator
;;; Transition
                                 "vlex-DTR"                                 "vlex-RTD"                               "3dpoint->2dpoint"
                                 "3dpoint-list->2dpoint-list"                                                    "vlex-Roll-Ratio"
                                 "vlex-DblList->VariantArray"                                                    "vlex-IntList->VarArray"
                                 "vlxx-VarList->VarArray"       separator
;;; Prompt
                                 "vlex-DPR"                                 separator
;;; SysVars
                                 "vlex-VarSave"                         "vlex-VarRestore"                    separator
;;; Layers
                                 "vlex-LayerTable"                  "vlex-LayZero"                       "vlex-LayActive"
                                 "vlex-LayActive"                       "vlex-LayerOn"                       "vlex-LayerOff"
                                 "vlex-LayerFreeze"                 "vlex-LayerThaw"                     "vlex-LayerNoPlot"
                                 "vlex-LayerLock"                       "vlex-LayerUnLock"               "vlex-ListLayers-Locked"
                                 "vlex-ListLayers-Frozen"       "vlex-ListLayers-Off"            "vlex-ListLayers-Plottable"
                                 "vlex-ListLayers-Plottalbe-Not"                                             "vlex-Layer-Frozen-p"
                                 "vlex-SetLweight"                  separator
;;; Selection Sets
                                 "vlex-SSetExists-p"                "vlex-SelectByType"              "vlex-SelectOnScreen-Filter"
                                 "vlex-PICKCIRCLES"                 "C:GETCIRCLES"                       separator
;;; APPLICATION STATE . . .
                                 "vlex-GetWindowState"          "vlex-SetWindowState"            "vlex-HideAutoCAD"
                                 "vlex-ShowAutoCAD"                 "vlex-HideShowTest"              "vlex-DocPrefs"
                                 "vlex-LWdisplayON"                 "vlex-LWdisplayOFF"              "vlex-ObjectSortBySnapON"
                                 "vlex-ObjectSortBySnapOFF" "vlex-XrefEditON"                    "vlex-XrefEditOFF"
                                 separator
;;; Menus & Toolbars. . .
                                 "vlex-MenuGroups"                  "vlex-MenuGroups-ListAll"    "vlex-MenuGroup-Exists-p"
                                 "vlex-Toolbars"                        "vlex-Toolbars-ListAll"      "vlex-Toolbar-Exists-p"
                                 "vlex-Toolbar"                         "vlex-Toolbar-Show"              "vlex-Toolbar-Hide"
                                 "vlex-Toolbar-Dock"                "vlex-Toolbar-Folat"             separator
;;; Visual LISP Custom Functions. . .
                                 "ex:2DPoint"                               "ex:ActivateLastLayout"      "ex:AddObjectsToBlock"
                                 "ex:MappedShare"                       "ex:BuildFilter"                     "ex:Centroid"
                                 "ex:ChangeAttributes"          "ex:ChangeBitmap"                    "ex:CloseAll"
                                 "ex:DeleteObjectFromBlock" "ex:DrawVpBorder"                    "ex:DriveType"
                                 "ex:ListDrives"                        "ex:Parse"                               "ex:ExportProject"
                                 "ex:ImportProject"                 "ex:GetAttributes"               "ex:GetBoundingBox"
                                 "ex:GetConstantAttributes" "ex:GetCurveLength"              "ex:GetFileSize"
                                 "ex:GetLastHeight"                 "ex:SetLastHeight"               "ex:GetParentBlocks"
                                 "ex:ListBLockRefs"                 "ex:GetPlotDevices"              "ex:PutXData"
                                 "ex:lisp-value"                        "ex:GetXData"                            "ex:LabelArea"
                                 "ex:LabelOrdinate"                 "ex:ListDocuments"               "ex:ListLayouts"
                                 "ex:ListToolbars"                  "ex:MakeLayer"                       "ex:RenameLayout"
                                 "ex:SelectAttributedBlocks"                                                     "ex:SetProfile"
                                 "ex:ToggleLayouts"                 "ex:ToggleMSBackground"      "ex:TogglePSBackground"
                                 "C:LayerFiltersDelete"         "ex:listToVariantArray"      "ex:selectionsetToArray"
                                 separator
;;; utilities...
                                 "vlex-ActiveSpace-Name"        "xyList->ListOfPoints"       "Is-Vla-Object"
                                 "Is-String"                                "Is-Real"                                    "Is-Ename"
                                 "Is-Variant"                               "vlex-MakeEname"                     "vlex-MakeObject"
                                 "IntList->VarArray"                "VarList->VarArray"              "vlex-ApplyLtypeGen"
                                 "vlex-Put-ByLayer"                 "vlex-ActiveLayout"              "vlex-ActiveLayoutName"
                                 "vlex-PlotConfigs"                 "vlex-OpenDwg"                       "vlex-DwgNamed-p"
                                 separator
;;; Zooming Functions...
                                 "vlex-ZoomExtents"                 "vlex-ZoomAll"                       "vlex-ZoomCenter"
                                 "vlex-ZoomPrevious"                "vlex-ZoomWindow"                    "vlex-ZoomOut"
                                 "vlex-ZoomIn"
                                )
        (princ cmd)
        (terpri)
    )
    (princ)
)


;;;*************************************************************************;;;
;;; MODULE:                                                                 ;;;
;;; DESCRIPTION:                                                            ;;;
;;; ARGS:                                                                   ;;;
;;; EXAMPLE:                                                                ;;;
;;;*************************************************************************;;;

(defun vlex-Version ()
    (princ "\nVlex-Lisp 2004 ver. 1.00")
    (princ
        "\nCopyright (C) 2004 Kama Whaley, All rights reserved."
    )
    (terpri)
    (princ)
)

                    ;(vlex-Version)

(princ)
