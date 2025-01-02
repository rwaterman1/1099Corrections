* This project allows someone to create a file of corrections made for 1099 data that is then
*  sent to the IRS.  This program will only work correctly if the ALL1099DATA table has continued
*  to be updated with the prior tax years data.  Also, the PriorFiles table needs to be updated with
*  the prior tax years final file.

* Initial setup
_SCREEN.Width = 1000
_SCREEN.Height = 800
SET EXCLUSIVE OFF
SET DELETED ON
SET EXACT OFF
SET CENTURY ON

SET PROCEDURE TO System.prg ADDITIVE

* Getting username.
PUBLIC  llDisplayErrors, lpUserIDBuffer, nBufferSize, RetVal, pcHomeDir, Corrections, pnTaxYear, pcCompany

llDisplayErrors = .T.
pcHomeDir = SYS(5) + SYS(2003)		&& Saving current directory so we know where home is.
RetVal         = 0
lpUserIDBuffer = SPACE(25) && Return buffer for user ID string
nBufferSize    = 25        && Size of user ID return buffer

DECLARE INTEGER GetUserName IN Win32API AS GetName ;
        STRING  @lpUserIDBuffer, ;
        INTEGER @nBufferSize

RetVal = GetName(@lpUserIDBuffer, @nBufferSize)
lpUserIDBuffer = UPPER(ALLTRIM(lpUserIDBuffer))

* Code for trapping errors.
ON ERROR DO ErrorHandler WITH IIF(VARTYPE(lpUserIDBuffer)<>'C','',lpUserIDBuffer), PROGRAM(), MESSAGE(1), LINENO(), MESSAGE(), ALIAS(), RECNO(), llDisplayErrors

USE \\jkfradb\fr_acct\fracctrpts\users\kevin\data_loc in 0 SHARED order region
USE \\jkfradb\fradb\regions IN 0 SHARED
USE Settings IN 0 SHARED ORDER Label
USE PriorFiles IN 0 SHAR
USE Correction IN 0 SHAR
USE CorrectH IN 0 SHARED
SET FILTER TO EMPTY(SentDate) IN CorrectH
USE CorrectD IN 0 SHARED
SET FILTER TO CorrectD.HID = CorrectH.HID IN CorrectD
USE ErrorType IN 0 SHARED ORDER ErrCode
SET FILTER TO Enable IN ErrorType
USE \\jkntdev\mis\legacy_apps\fixes\1099mag\new1099submissionscripts\all1099data.dbf IN 0 SHARED
* Limit the starting year to 2024 since that's when electronic corrections first started.
lnFirstYear = VAL(GetSetting('FirstYear'))
IF YEAR(DATE())-4 > lnFirstYear		&& If it's been more than 4 years after 2024, use the newer year.
	lnFirstYear = YEAR(DATE())-4
ENDIF

* Allow the correction of records before this year and up to 3 year ago.
SELECT DISTINCT RptYear FROM All1099Data WHERE RptYear <> YEAR(DATE()) AND RptYear >= lnFirstYear ORDER BY 1 DESCENDING INTO CURSOR Years
SELECT DISTINCT Company_No FROM All1099Data ORDER BY 1 INTO CURSOR Company

IF RECCOUNT('Years') < 1
	MESSAGEBOX('No corrections can be made at this time.')
	Cleanup()
	RETURN
ENDIF

GO TOP IN Years
pnTaxYear = Years.RptYear
GO TOP IN Company
pcCompany = ALLTRIM(Company.Company_No)
SELECT All1099Data
SET FILTER TO RptYear = pnTaxYear AND Company_no = pcCompany

DO FORM Corrections NAME Corrections LINKED
RELEASE Corrections

Cleanup()
