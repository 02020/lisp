;;;(setq ADOConstant-adOpenKeyset 1
;;;      ADOConstant-adLockOptimistic 3
;;;      ADOConstant-adCmdText
;;;       ADOConstant-adStateClosed 
;;;	   
;;;	   
;;;	   ADOConstant-adGetRowsRest
;;;
;;;       )

;; | ----------------------------------------------------------------------------
;; | ADO_Init
;; | ----------------------------------------------------------------------------
;; | Function : ADO Initialization function
;; | Arguments: None
;; | Updated  : March 14, 2007
;; | (C) 1999-2007, Four Dimension Technologies, Bangalore
;; | e-mail   : rakesh.rao@4d-technologies.com 
;; | Web      : www.4d-technologies.com
;; | ----------------------------------------------------------------------------


(defun ADO_Init ( / ADOLISP_ADODLLPath )


;;; Set up some variables that must be global (within
;;; this file)

;;; Define a VB data type that Visual LISP forgot
(if (not vlax-vbDecimal)
  (setq vlax-vbDecimal 14)
)

;;; Set a flag if we are running in AutoCAD 2000 (not 2000i,
;;; 2002, ...)
(if (< (atof (getvar "ACADVER")) 15.05)
  (setq ADOLISP_IsAutoCAD2000 T)
)

;; Import the ADO type library if it hasn't already been
;; loaded.

(if (null ADOMethod-Append)
(cond
	((and (setq ADOLISP_ADODLLPath
                 (vl-registry-read
                   "HKEY_CLASSES_ROOT\\ADODB.Command\\CLSID"
                 )
          )
          (setq ADOLISP_ADODLLPath
                 (vl-registry-read
                   (strcat "HKEY_CLASSES_ROOT\\CLSID\\"
                           ADOLISP_ADODLLPath
                           "\\InProcServer32"
                   )
                 )
          )
          (progn
            ;; Workaround for bug in AutoCAD 2008 under Vista, returning
            ;; a dotted pair list containing the string instead of the
            ;; string
            (if (listp ADOLISP_ADODLLPath)
              (setq ADOLISP_ADODLLPath (cdr ADOLISP_ADODLLPath))
            )
            (findfile ADOLISP_ADODLLPath)
          )
     )
	)
    ;; Or if we can find it where we expect to find it ...
    ((setq ADOLISP_ADODLLPath
            (findfile
              (if (getenv "systemdrive")
                (strcat
                  (getenv "systemdrive")
                  "\\program files\\common files\\system\\ado\\msado15.dll"
                )
                "c:\\program files\\common files\\system\\ado\\msado15.dll"
              )
            )
     )
     ;; Import it
     (vlax-import-type-library
       :tlb-filename ADOLISP_ADODLLPath :methods-prefix
       "ADOMethod-" :properties-prefix "ADOProperty-"
       :constants-prefix "ADOConstant-"
     )
    )
    ;; Can't find the library, tell the user
    (T
     (alert
       (strcat "Cannot find\n\""
               (if ADOLISP_ADODLLPath
                 ADOLISP_ADODLLPath
                 "msado15.dll"
               )
               "\""
       )
     )
    )
))
)



;; | ----------------------------------------------------------------------------
;; | ADO_DisconnectFromDB
;; | ----------------------------------------------------------------------------
;; | Function : Disconnects the specified database connection and release the
;; |            connection object
;; | Arguments: An ADO Connection Object
;; | Action   : Returns T if the connection was disconncted, else nil
;; | Updated  : November 14, 2001
;; | Comment  : A function to close a connection and release the connection
;; |            object.
;; | (C) 1999-2004, Four Dimension Technologies, Bangalore
;; | e-mail   : rakesh.rao@4d-technologies.com 
;; | Web      : www.4d-technologies.com
;; | ----------------------------------------------------------------------------


(defun ADO_DisconnectFromDB (ConnectionObject / Ret)

  (setq
    #ADOLISP_ErrorList	      nil
    #ADOLISP_LastSQLStatement nil
  )

  (if (and ConnectionObject (boundp ConnectionObject))
    (progn
      (vlax-invoke-method ConnectionObject "Close")
      (vlax-release-object ConnectionObject)
      (setq Ret T)
    )
    (setq Ret nil)
  )
  Ret
)







;; | ---------------------------------------------------------------------
;; | ADO_DoSQL
;; | ---------------------------------------------------------------------
;; | Description: A function to execute an arbitrary SQL statement
;; |              (replacable parameters are not supported).
;; |
;; | Arguments:
;; |              ConnectionObject: An ADO Connection Object.
;; |              SQLString       : the SQL statement to execute.
;; |
;; | Return value:
;; |
;; |  If anything fails, NIL.  Call (ADO_ErrorPrinter) to
;; |  print error messages to the command line.  Otherwise:
;; |
;; | If the SQL statement is a "select ..." statement that
;; |  could return rows, returns a list of lists.  The first
;; |  is a list of the column names.  If any rows were
;; |  returned, the subsequent sub-lists contain the
;; |  returned rows in the same order as the column names
;; |  in the first sub-list.
;; |
;; |  If the SQL statement is a "delete ...", "update ...", or
;; |  "insert ..." that cannot return any rows:
;; |    Returns T
;; | ---------------------------------------------------------------------

(defun ADO_DoSQL (ConnectionObject  SQLStatement      /
		  RecordSetObject   FieldsObject      FieldNumber
		  FieldCount	    FieldList	      RecordsAffected
		  TempObject	    ReturnValue	      CommandObject
		  IsError	    FieldItem
		  FieldPropertiesList		      FieldName
		 )
  ;; Assume no error
  (setq	ADOLISP_ErrorList
	 nil
	;; Initialize global variables
	ADOLISP_LastSQLStatement
	 SQLStatement
	ADOLISP_FieldsPropertiesList
	 nil
  )

  ;; Create an ADO Recordset and set the cursor and lock
  ;; types
  (setq	RecordSetObject
	 (vlax-create-object "ADODB.RecordSet")
  )
  (vlax-put-property
    RecordSetObject
    "cursorType"
    ADOConstant-adOpenKeyset
  )
  (vlax-put-property
    RecordSetObject
    "LockType"
    ADOConstant-adLockOptimistic
  )
  ;; Open the recordset.  If there is an error ...
  (if (vl-catch-all-error-p
	(setq TempObject
	       (vl-catch-all-apply
		 'vlax-invoke-method
		 (list RecordSetObject	  "Open"
		       SQLStatement	  ConnectionObject
		       nil		  nil
		       ADOConstant-adCmdText
		      )
	       )
	)
      )
    ;; Save the error information
    (progn
      (setq ADOLISP_ErrorList
	     (ADO_ErrorProcessor TempObject ConnectionObject)
      )
      (setq IsError T)
      (vlax-release-object RecordSetObject)
    )
    ;; Otherwise, set an indicator that it worked
    (setq RecordsAffected T)
  )
  ;; If there were no errors ...
  (if (not IsError)
    ;; If the recordset is closed ...
    (if	(= ADOConstant-adStateClosed
	   (vlax-get-property RecordsetObject "State")
	)
      ;; Then the SQL statement was a "delete ..." or an
      ;; "insert ..." or an "update ..." which doesn't
      ;; return any rows.
      (progn
	(setq ReturnValue RecordsAffected)
	;; And release the recordset and command; we're done.
	(vlax-release-object RecordSetObject)
      )
      ;; The recordset is open, the SQL statement
      ;; was a "select ...".
      (progn
	;; Get the Fields collection, which
	;; contains the names and properties of the
	;; selected columns
	(setq FieldsObject (vlax-get-property
			     RecordSetObject
			     "Fields"
			   )
	      ;; Get the number of columns
	      FieldCount   (vlax-get-property FieldsObject "Count")
	      FieldNumber  -1
	)
	;; For all the fields ...
	(while
	  (> FieldCount (setq FieldNumber (1+ FieldNumber)))
	   (setq FieldItem	     (vlax-get-property
				       FieldsObject
				       "Item"
				       FieldNumber
				     )
		 ;; Get the names of all the columns in a list to
		 ;; be the first part of the return value
		 FieldName	     (vlax-get-property FieldItem "Name")
		 FieldList	     (cons FieldName FieldList)
		 FieldPropertiesList nil
	   )
	   (foreach FieldProperty '("Type"	      "Precision"
				    "NumericScale"    "DefinedSize"
				    "Attributes"
				   )
	     (setq FieldPropertiesList
		    (cons (cons	FieldProperty
				(vlax-get-property
				  FieldItem
				  FieldProperty
				)
			  )
			  FieldPropertiesList
		    )
	     )
	   )
	   ;; Save the list in the global list
	   (setq ADOLISP_FieldsPropertiesList
		  (cons
		    (cons FieldName
			  FieldPropertiesList
		    )
		    ADOLISP_FieldsPropertiesList
		  )
	   )
	)
	;; Get the FieldsPropertiesList in the right order
	(setq ADOLISP_FieldsPropertiesList
	       (reverse
		 ADOLISP_FieldsPropertiesList
	       )
	)

	;; Initialize the return value
	(setq ReturnValue (list (reverse FieldList)))
	;; If there are any rows in the recordset ...
	(if
	  (not (and (= :vlax-true
		       (vlax-get-property RecordSetObject "BOF")
		    )
		    (= :vlax-true
		       (vlax-get-property RecordSetObject "EOF")
		    )
	       )
	  )
	   ;; We're about to get tricky, hang on!  Create the
	   ;; final results list ...
	   (setq
	     ReturnValue
	      ;; By appending the list of rows to the list of
	      ;; fields.
	      (append
		(list (reverse FieldList))
		;; Uses Douglas Wilson's elegant
		;; list-transposing code from
		;; http://xarch.tu-graz.ac.at/autocad/lisp/
		;; to create the list of rows, because
		;; GetRows returns items in column order
		(apply
		  'mapcar
		  (cons
		    'list
		    ;; Set up to convert a list of lists
		    ;; of variants to a list of lists of
		    ;; items that AutoLISP understands
		    (mapcar
		      '(lambda (InputList)
			 (mapcar '(lambda (Item)
				    (ADO_variant-value Item)  ;; ±»ÐÞ¸Ä
				  )
				 InputList
			 )
		       )
		      ;; Get the rows, converting them from
		      ;; a variant to a safearray to a list
		      (vlax-safearray->list
			(vlax-variant-value
			  (vlax-invoke-method
			    RecordSetObject
			    "GetRows"
			    ADOConstant-adGetRowsRest
			  )
			)
		      )
		    )
		  )
		)
	      )
	   )
	)
	;; Close the recordset and release it and the
	;; command
	(vlax-invoke-method RecordSetObject "Close")
	(vlax-release-object RecordSetObject)
      )
    )
  )
  ;; And return the results
  ReturnValue
)


;;ÅÐ¶Ï
(defun ADO_variant-value (value)
  (setq _type (vlax-variant-type value))
  (cond
    ((= _type 14) (vlax-variant-value (vlax-variant-change-type value vlax-vbstring)))
    (t (vlax-variant-value value))
  )
)





;; ! ****************************************************************************
;; ! ADO_ErrorProcessor
;; ! ****************************************************************************
;; ! Function : A function to assemble all errors into a list of lists of dotted
;; !            pairs of strings ("name" . "value")
;; ! Arguments: 'VLErrorObject'    - ADO Error Object
;; !            'ConnectionObject' - ADO Connection Object
;; ! Returns  : A list of all error messages
;; ! Comments :
;; ! Updated  : May 25, 2004
;; ! (C) 1999-2004, Four Dimension Technologies, Bangalore
;; ! e-mail   : rakesh.rao@4d-technologies.com 
;; ! Web      : www.4d-technologies.com
;; ! **************************************************************************


(defun ADO_ErrorProcessor (VLErrorObject ConnectionObject /
                               ErrorsObject ErrorObject ErrorCount
                               ErrorNumber ErrorList ErrorValue
								ReturnList
                              )


;; First get Visual LISP's error message
(setq
	ReturnList (list (list (cons "Visual LISP message" (vl-catch-all-error-message VLErrorObject))))

	;; Get the ADO errors object and quantity
	ErrorObject  (vlax-create-object "ADODB.Error")
	ErrorsObject (vlax-get-property ConnectionObject "Errors")
	ErrorCount   (vlax-get-property ErrorsObject "Count")
	ErrorNumber  -1
)

;; Loop over all the ADO errors ...
(while (< (setq ErrorNumber (1+ ErrorNumber)) ErrorCount)
	;; Get the error object of the current error
	(setq
		ErrorObject (vlax-get-property ErrorsObject "Item" ErrorNumber)
		;; Clear the list of items for this error
		ErrorList   nil
	)
	;; Loop over all possible error items of this error
	(foreach ErrorProperty '("Description" "HelpContext" "HelpFile" "NativeError" "Number" "SQLState" "Source")
		;;  Get the value of the current item.  If it's a number ...
		(if (numberp (setq ErrorValue (vlax-get-property ErrorObject ErrorProperty)))
			;; Convert it to a string for consistency
			(setq ErrorValue (itoa ErrorValue))
		)
		;; And store it
		(setq ErrorList (cons (cons ErrorProperty ErrorValue) ErrorList))
	)
	;; Add the list for the current error to the return value
	(setq ReturnList (cons (reverse ErrorList) ReturnList))
)

;; Set up the return value in the correct order
(reverse ReturnList)
)



;; ! ****************************************************************************
;; ! ADO_GetFieldNames
;; ! ****************************************************************************
;; ! Function : Label the segments of a polyline with their distances
;; ! Arguments: 'ConnectionObject' - The ADO connection object
;; !            'Table'            - The table to retrieve fields from
;; ! Action   : Returns a list of all field names and their types from the table
;; !            as a nested list
;; ! Updated  : June 24, 2003
;; ! (C) 1999-2004, Four Dimension Technologies, Bangalore
;; ! e-mail   : rakesh.rao@4d-technologies.com 
;; ! Web      : www.4d-technologies.com
;; ! **************************************************************************


(defun ADO_GetFieldNames ( ConnectionObject Table / RecordSet flds fldsCnt fldsIdx fldsLst thisFld thisFldName thisFldType
 fldsLst catchObj ErrorFound cmd )


(setq
	cmd (vlax-create-object "ADODB.Command")
	fldsLst nil
	ErrorFound nil
)
(vlax-put-property cmd "ActiveConnection" ConnectionObject)
(vlax-put-property cmd "CommandTimeout"  30)

(setq catchObj (vl-catch-all-apply 'vlax-put-property (list cmd "CommandText" (strcat "SELECT * from " Table))))

(if (= (type catchObj) 'VL-CATCH-ALL-APPLY-ERROR)
(progn
	(alert (vl-catch-all-error-message catchObj))
	(setq ErrorFound T)
))
	  
(if (not ErrorFound)
(progn
	(setq
		RecordSet (vlax-create-object "ADODB.Recordset")
		catchObj  (vl-catch-all-apply 'vlax-invoke-method (list RecordSet "Open" cmd nil adoConstant-adOpenDynamic adoConstant-adLockBatchOptimistic adoConstant-adCmdUnknown))
	)

	(if (= (type catchObj) 'VL-CATCH-ALL-APPLY-ERROR)
	(progn
		(alert (vl-catch-all-error-message catchObj))
		(setq ErrorFound T)
	))
	
	(if (not ErrorFound)
	(progn
		(setq
			flds    (vlax-get-property RecordSet "Fields")
			fldsCnt (1- (vlax-get-property flds "Count"))		; Zero-based index
			fldsIdx 0
			fldsLst '()
		)

		(while (<= fldsIdx fldsCnt)
			(setq
				thisFld (vlax-get-property flds "Item" fldsIdx)		; Item is a property not a method for recordSet fields...
				thisFldName (vlax-get-property thisFld "Name")
				thisFldType (vlax-get-property thisFld "Type")
			)
			(cond
				((member thisFldType (list	adoConstant-adBigInt           adoConstant-adInteger
											adoConstant-adSmallInt         adoConstant-adTinyInt
											adoConstant-adUnsignedBigInt   adoConstant-adUnsignedInt
											adoConstant-adUnsignedSmallInt adoConstant-adUnsignedTinyInt
										))									; Integer
					(setq thisFldType "Integer")
				)
				((member thisFldType (list	adoConstant-adBinary    adoConstant-adLongVarBinary
											adoConstant-adVarBinary
										))									; Binary
					(setq thisFldType "Binary")
				)
				((= thisFldType adoConstant-adBoolean)								; Boolean
					(setq thisFldType "Boolean")
				)
				((member thisFldType (list	adoConstant-adBSTR        adoConstant-adChar
											adoConstant-adDBDate      adoConstant-adDBTime
											adoConstant-adDBTimeStamp adoConstant-adEmpty
											adoConstant-adError       adoConstant-adGUID
											adoConstant-adLongVarChar adoConstant-adLongVarWChar
											adoConstant-adVarChar     adoConstant-adVarWChar
											adoConstant-adWChar
										))									; String
					(setq thisFldType "Character")
				)
				((member thisFldType (list	adoConstant-adCurrency adoConstant-adDate
											adoConstant-adDecimal  adoConstant-adDouble
											adoConstant-adNumeric  adoConstant-adSingle
											))								; Double
					(setq thisFldType "Real")
				)
			)

			(setq
				fldsLst (cons (list (strcase thisFldName) thisFldType) fldsLst)
				fldsIdx (1+ fldsIdx)
			)
		)

		(setq fldsLst (reverse fldsLst))

		(if (/= (vlax-get-property RecordSet "State") adoConstant-adStateClosed )
		(progn
			(vlax-invoke-method RecordSet "Close")
			(vlax-release-object RecordSet)
			(setq RecordSet nil)
		))
	))

	;; reelase the command object
	(vlax-release-object cmd)
	(setq cmd nil)
))
fldsLst
)




;; | ----------------------------------------------------------------------------
;; | ADO_ConnectToDB
;; | ----------------------------------------------------------------------------
;; | Function : Connects to the specified database defined by 'ConnectionString'
;; | Arguments: 'ConnectString' - Connect String to connect to database
;; |                              Either the name of a .UDL file,
;; |                              including the ".UDL", or an
;; |                              OLEDB connection string.
;; |                              If this argument is the name of
;; |                              a UDL file without a full path,
;; |                              it is searched for in the
;; |                              current directory, the
;; |                              AutoCAD search path, and the
;; |                              AutoCAD Data Source Location.
;; |            'UserName'      - The user name to use when connecting.
;; |                              May be a null string if the user name is
;; |                              specified in the first argument or the
;; |                              first argument is a UDL file name.
;; |            'PassWord'      - The password to use when connecting.
;; |                              May be a null string if the password is
;; |                              supplied in the first argument or the
;; |                              first argument is a UDL file name.
;; | Action   : Returns the connection object, if successfully connected
;; | Updated  : March 13, 2007
;; |
;; | Return value:
;; |  If anything fails, NIL.  Call (ADO_ErrorPrinter) to
;; |  print error messages to the command line.
;; |  Otherwise, an ADO Connection Object.
;; | ---------------------------------------------------------------------------


(defun ADO_ConnectToDB	(ConnectString UserName Password / IsUDL
                            FullUDLFileName ConnectionObject TempObject
                            ReturnValue ConnectionPropertiesObject
                            ConnectionParsingPropertyObject
                        )
  ;; Assume no error
  (setq ADOLISP_ErrorList        nil
        ADOLISP_LastSQLStatement nil
  )

  ;; If the connect string is a UDL file name ...
  (if (= ".UDL"
         (strcase
           (substr ConnectString (- (strlen ConnectString) 3))
         )
      )
    (progn
      ;; Set a flag that it's a UDL file
      (setq IsUDL T)
      ;; Try to find it
      (cond
        ((setq FullUDLFileName (findfile ConnectString)))
        ;; Didn't find it in the current directory or
        ;; the AutoCAD search path, try the AutoCAD
        ;; Data Source location
        ((setq FullUDLFileName
                (findfile (strcat (vlax-get-property
                                    (vlax-get-property
                                      (vlax-get-property
                                        (vlax-get-acad-object)
                                        "Preferences"
                                      )
                                      "Files"
                                    )
                                    "WorkspacePath"
                                  )
                                  "\\"
                                  ConnectString
                          )
                )
         )
        )
        ;; Didn't find it, store an error message
        (t
         (setq ADOLISP_ErrorList
                (list (list (cons "ADOLISP connection error"
                                  (strcat "Can't find \""
                                          ConnectString
                                          "\""
                                  )
                            )
                      )
                )
         )
        )
      )
    )
  )

  ;; If the first argument is a UDL file name... ...
  (if IsUDL
    ;; If we found it ...
    (if FullUDLFileName
      (progn
        ;; Create an ADO connection object
        (setq ConnectionObject
               (vlax-create-object
                 "ADODB.Connection"
               )
        )
        ;; Try to open the connection.  If there is an error
        ;; ...
        (if (vl-catch-all-error-p
              (setq TempObject
                     (vl-catch-all-apply
                       'vlax-invoke-method
                       (list ConnectionObject
                             "Open"
                             (strcat "File Name=" FullUDLFileName)
                             UserName
                             Password
                             ADOConstant-adConnectUnspecified
                       )
                     )
              )
            )
          (progn
            ;; Save the error information
            (setq ADOLISP_ErrorList
                   (ADO_ErrorProcessor TempObject ConnectionObject)
            )
            ;; Release the connection object
            (vlax-release-object ConnectionObject)
          )
          ;; It worked, store the connection object in our
          ;; return value
          (setq ReturnValue ConnectionObject)
        )
      )
    )
    ;; The connect string is not a UDL file name.
    (progn
      ;; Create an ADO connection object
      (setq ConnectionObject
             (vlax-create-object "ADODB.Connection")
      )
      ;; Try to open the connection.  If there is an error ...
      (if (vl-catch-all-error-p
            (setq TempObject
                   (vl-catch-all-apply
                     'vlax-invoke-method
                     (list
                       ConnectionObject "Open" ConnectString UserName
                       Password ADOConstant-adConnectUnspecified
                      )
                   )
            )
          )
        (progn
          ;; Save the error information
          (setq ADOLISP_ErrorList
                 (ADO_ErrorProcessor TempObject ConnectionObject)
          )
          ;; Release the connection object
          (vlax-release-object ConnectionObject)
        )
        ;; It worked, store the connection object in our
        ;; return value
        (setq ReturnValue ConnectionObject)
      )
    )
  )
  ;; If we made a connection ...
  (if ReturnValue
    (progn
      ;; If we want to set ODBC Parsing to true ...
      (if (not ADOLISP_DoNotForceJetODBCParsing)
        (progn
          ;; Get the properties collection
          (setq ConnectionPropertiesObject
                 (vlax-get-property
                   ReturnValue
                   "Properties"
                 )
          )
          ;; If the properties collection has a "Jet OLEDB:ODBC
          ;; Parsing" item ...
          (if (not (vl-catch-all-error-p
                     (setq ConnectionParsingPropertyObject
                            (vl-catch-all-apply
                              'vlax-get-property
                              (list
                                ConnectionPropertiesObject
                                "ITEM"
                                "Jet OLEDB:ODBC Parsing"
                              )
                            )
                     )
                   )
              )
            ;; Set the "Jet OLEDB:ODBC Parsing" item to
            ;; "true" so the Jet engine accepts double-quotes
            ;; around delimited identifiers
            (vlax-put-property
              ConnectionParsingPropertyObject
              "VALUE"
              :vlax-true
            )
          )
        )
      )
      ;; And release our objects
      (if (= 'VLA-OBJECT (type ConnectionParsingPropertyObject))
        (vlax-release-object ConnectionParsingPropertyObject)
      )
      (if (= 'VLA-OBJECT (type ConnectionPropertiesObject))
        (vlax-release-object ConnectionPropertiesObject)
      )
    )
  )
  ReturnValue
)
