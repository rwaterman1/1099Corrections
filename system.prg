* Program to hold the various functions and procedures needed.


***********************************************************
*
* Set any necessary system settings.
*
***********************************************************
PROCEDURE SetSettings
	lnArea = SELECT()
	IF NOT USED('RptSettings')
		IF FILE('RptSettings.dbf')
			USE RptSettings IN 0 SHARED ORDER SetName
		ENDIF
	ENDIF
	IF USED('RptSettings')
		IF INDEXSEEK('ALLOW_ANYONE', .T., 'RptSettings', 'SetName')
			plAllowAnyone = IIF(UPPER(ALLTRIM(RptSettings.SetValue)) $ 'T Y', .T., .F.)
		ENDIF
	ENDIF
	SELECT(lnArea)
ENDPROC


***********************************************************
*
* Determine if user can access any reports.
*
***********************************************************
PROCEDURE AccessGranted
	LPARAMETERS tcUserName

	llAllowAccess = .F.

	SELECT RptUsers
	LOCATE FOR UPPER(ALLTRIM(tcUserName)) = UPPER(ALLTRIM(UserName))
	IF FOUND()
		llAllowAccess = .T.
	ENDIF

	RETURN llAllowAccess

ENDPROC


***********************************************************
*
* Convert any type of value to a string.
*	tlIncludeQuotes determines if double quotes are put around the value returned or not.
*
***********************************************************
PROCEDURE Convert2String
	LPARAMETERS txValue, tlIncludeQuotes
	
	lcValueString = ""
	lcVarType = VARTYPE(txValue)
	DO CASE
		CASE lcVarType = "C"				&& No conversion needed since this is already character.
			* Get rid of double quotes in the string so it doesn't mess up the result returned.
			IF tlIncludeQuotes
				lcValueString = '"' + STRTRAN(ALLTRIM(txValue), '"') + '"'
			ELSE
				lcValueString = ALLTRIM(txValue)
			ENDIF
		CASE lcVarType = "L"				&& Logical, Binary (T/F)
			lcValueString = IIF(txValue, '.T.', '.F.')
		CASE lcVarType = "D"				&& Date
			lcValueString = "{" + DTOC(txValue) + "}"
		CASE lcVarType = "T"				&& DateTime
			lcValueString = TTOC(txValue)
		CASE lcVarType = "N"				&& Integer, Numeric
			lcValueString = ALLTRIM(STR(txValue))
		CASE lcVarType = "Y"				&& Currency
			lcValueString = ALLTRIM(STR(MTON(txValue)))
		CASE lcVarType = "U"				&& Unknown
			* This is unknown, or undefined, so that is what we will return.
			IF tlIncludeQuotes
				lcValueString = '"UNKNOWN"'
			ELSE
				lcValueString = "UNKNOWN"
			ENDIF
	ENDCASE

	RETURN lcValueString
ENDPROC


***********************************************************
*
* Save errors to table for reference.
*
***********************************************************
PROCEDURE ErrorHandler
	LPARAMETERS tcUserName, tcProgram, tcCode, tnLineNo, tcMessage, tcTable, tnRecNo, tlShowError

	IF NOT USED('Errors') AND FILE('Errors.dbf')
		USE Errors IN 0 SHAR
	ENDIF

	IF USED('Errors')
		INSERT INTO Errors (Created_dt, Username, Program, Code, LineNo, Message, Table, RecNo) ;
		 VALUES (DATETIME(), tcUserName, tcProgram, tcCode, tnLineNo, tcMessage, tcTable, tnRecNo)
	ENDIF

	* Show error to user.  Should not show if not told to.
	IF tlShowError
		lcErrorMessage = 'There was an error:' + CHR(13) + CHR(13) + tcMessage + CHR(13) + CHR(13) + ;
		 'If you continue, the program may not function as expected.'
		MESSAGEBOX(lcErrorMessage,48,'Error')
	ENDIF

	* Putting things back.
	IF NOT EMPTY(tcTable)
		SELECT (tcTable)
	ENDIF

ENDPROC
