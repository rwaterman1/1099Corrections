* This program is used to create the correction file to be sent to the IRS for any records that have already been submmited.

clos all
clea all
clea
set dele on

*--> Had to set up NAT000 and NAT002 in my data_loc in order to find the NAT2 data folder
*--> May have to update the Reporting App to account for this (when printing any checks).
*--> Look into what issues may arise with multiple 1099s for same franchisee from same vendor (Same Fran paid from AFR and NAT2?)
*--> *** <-- Only have to note pymt types 3 and C in A record if they will be used in that region (by that payee)...

*--> ! ! ! Be sure to SKIP (don't report) records that have an amount of 0 ! ! ! <--* 

*--> Remove when TESTING completed!
*set path to i:\frrpts
*-->

*--> Last year we will file any 1099's for Boston is with this submission (2015).
*--> Last year we will file any 1099's for Denver is with this submission (2016).
*--> Last year we will file any 1099's for St Louis is with this submission (2017).
*--> Last year we will file any 1099's for Las Vegas and Reno is with this submission (2018).
*LastFiling="RNO951 LAS551" && Set to company_no of any region where this will be the last year they file - otherwise set to "      ".
LastFiling="      " && Set to company_no of any region where this will be the last year they file - otherwise set to "      ".
*-->

GatherData=.T.  && Set to .F. once data has been collected.
xTestInd="T"  && If Test File, set to "T", otherwise leave blank " " or "F".
createSubmission=.T.  && Set to .T. to create submission file.
xRptYear=2024
TotA_Recs=0
TotB_Recs=0
recSeqNo=0
xCompNo=""
exclRegions = "BOS291 DEN191 HOU051 STL091 NMX841 RNO951 LAS551"

if xTestInd="T" 
	fileName="test2_"+alltrim(str(xRptYear))
else
	fileName="final"+alltrim(str(xRptYear))
endif

*--> KMC 12/02/2021 
*use i:\FrRpts\data_loc in 0 order region 
*-->
use \\jkfradb\fr_acct\fracctrpts\users\kevin\data_loc in 0 order region

*--> Query all 1099data and copy to a central location (cursor tmp1099 which is then stored permanently in table All1099Data).
if GatherData=.T.
	Do CreateTmpCursor
	Do GetCorpAcct1099Data
	*sele tmp1099
	*brow last
	Do GetFranAcct1099Data

	*--> KMC 12/18/2020 - Have to change Vendor 7's from Traverse to NE's on my end.
	sele tmp1099
	repla all for ven_fran = "Vendor" and trim(AmtCode) $ "7" AmtCode with "NE"
	*-->
	
	Do CopyDataToStorage	
	wait clear
endif
*return

*--> Create file to submit to IRS based on data from All1099Data.
if CreateSubmission=.T.
	Do Query1099Data

	*--> KMC 12/16/2020 - Modified to accomodate 1099-NEC (Non-employee Compensation which would be used for Franchisee's).
	*sele tmp1099
	*if !eof()
	*	Do SetWriteFile 
	*	Do WriteT_Record 
	*	AllDone=.F.	
	*	do while !AllDone
	*		Do WriteA_Record
	*		Do WriteBC_Record
	*		*Write State Totals "K" record
	*	enddo	
	*	Do WriteF_Record 
	*	SET TEXTMERGE TO
	*	SET CONSOLE ON
	*endif
	sele tmp1099
	if !eof()
		brow last
		Do SetWriteFile 
		Do WriteT_Record 
		
		sele tmp1099
		set filt to AmtCode <> "NE"
		go top
		if !eof() && KMC 12/02/2021
			AllDone=.F.	
			do while !AllDone
				Do WriteA_Record
				Do WriteBC_Record
			enddo
		endif && KMC 12/02/2021
		
		sele tmp1099
		set filt to AmtCode = "NE"
		go top
		if !eof() && KMC 12/02/2021
			AllDone=.F.	
			do while !AllDone
				Do WriteA_Record
				Do WriteBC_Record
			enddo
		endif && KMC 12/02/2021
			
		sele tmp1099
		set filt to
		*-->
		
		Do WriteF_Record 
		SET TEXTMERGE TO
		SET CONSOLE ON
	endif

endif

messagebox("Verify in the file submisison process that all AmtCodes in use are populated when the file to transmit is created.")

sele AmtCode, count(AmtCode) as count from All1099Data where rptYear = xRptYear into cursor tmpCodes group by AmtCode order by AmtCode
sele tmpCodes
brow last

CLOS DATA ALL

Proc CreateTmpCursor
	create cursor tmp1099 ;
		(rptYear N(4), ;
		company_no C(7), ;
		ven_fran C(10), ;
		dlr_code C(6), ;
		dlr_id C(3), ;
		name_1099 C(40), ;
		dlr_addr C(40), ;
		dlr_city C(20), ;
		dlr_state C(2), ;
		dlr_zip C(10), ;
		ssn_fid C (11), ;
		amount N(12,2), ;
		AmtCode C(2))
return

Proc getCorpAcct1099Data
	sele data_loc
	set filt to !empty(traverse_d) and !company_no $ exclRegions
	go top
	do while !eof()
		wait window "Now Processing "+alltrim(data_loc.region)+" - Vendors..." nowait
	
		xServer=alltrim(data_loc.traverse_s)
		SqlUsr=alltrim(data_loc.traverse_p)
		curRegion=alltrim(data_loc.traverse_d)

		VenID1=""
		VenID2="ZZZZZZZZZZ"
		IndFrom=""
		IndThru="Z"
		RptYear=alltrim(str(xRptYear))
		RptPeriods="12"

		xSqlStr="Exec qryApTen99FormsMMTmpTable '"+VenID1+"','"+VenID2+"','"+IndFrom+"','"+IndThru+"',"+rptYear+","+rptPeriods
		*?xSqlStr
		
		*--> Create Connection to SQL Database
		oConn=sqlStringConnect("Driver={SQL Server};Server="+xServer+";"+SqlUsr+";Database="+curRegion)

		*--> Check SQL Connection
		if oConn=-1
			wait window "SQL Conection Error!"
			sele data_loc
			brow last
			return
		endif	

*--> KMC 01/31/2014.  SP qryApTen99FormsMmTmpTable no longer works.
*!*			*--> Execute SQL Stored Procedure 
*!*			sqlExec(oConn,xSqlStr)

*!*			* Get 1099 data from table tmpAPTen99Edit - delete all records in table when done.
*!*			xSqlStr="select * from tmpAPTen99Edit"
*!*			x=sqlExec(oConn,xSqlStr,'myCursor')
*!*		
*!*			if x=-1
*!*				wait window "SQL Connection Error "+xSqlStr
*!*				return	
*!*			endif
	
		* Pull 1099 data from table tblApTen99Edit.
		* There is no year field, so not sure if this data is overwritten when nexts years 1099's are run or not.
		*xSqlStr="select * from tblAPTen99Edit"
		xSqlStr="select * from tblAPTen99Edit"
		x=sqlExec(oConn,xSqlStr,'myCursor')
		
		*--> KMC 02/02/15 - Ten99FieldIndicator is only populated here now.
		xSqlStr="select * from tblAPVendor"
		x=sqlExec(oConn,xSqlStr,'myCursor2')
		*-->
		
		*sele myCursor
		*brow last
		*sele myCursor2
		*brow last
	
*-->
		sele xRptyear as RptYear, ;
			iif(data_loc.company_no="ADVXX1","_ADVXX1",iif(data_loc.company_no="INTXX1","_INTXX1",data_loc.company_no)) as company_no, ;
			"Vendor" as ven_fran, "      " as dlr_code, "   " as dlr_id, ;
			myCursor.name as name_1099, ;
			iif(!isnull(myCursor.addr1), myCursor.addr1, "")as dlr_addr, ;
			iif(!isnull(myCursor.addr2), myCursor.addr2, "")as dlr_addr2, ;
			iif(!isnull(myCursor.city), myCursor.city, "") as dlr_city, ;
			iif(!isnull(myCursor.region), myCursor.region, "") as dlr_state, ;
			iif(!isnull(myCursor.postalcode), myCursor.postalcode, "") as dlr_zip, ;
			myCursor.Ten99RecipientID as ssn_fid, myCursor2.Ten99FieldIndicator as AmtCode, myCursor.amount ;
		from myCursor ;
		inner join myCursor2 on myCursor.vendorID = myCursor2.vendorId ;
		order by company_no, dlr_code, dlr_id ;
		into cursor tmpOut

		sele tmpOut
		if !eof()
			scan
				scatter memvar
				if isnull(m.AmtCode)
					m.AmtCode=""
				endif	
				insert into tmp1099 from memvar
			endscan			
			sele myCursor
			use
		endif

*--> KMC 01/31/2014 - Nothing to delete now.
*!*			* Delete all data from table tmpApten99Edit and tmpAPTen99MMHeader when done.
*!*			xSqlStr="delete from tmpAPTen99Edit"
*!*			x=sqlExec(oConn,xSqlStr,'myCursor')
*!*			if x=-1
*!*				wait window "SQL Connection Error "+xSqlStr
*!*				suspend
*!*				return	
*!*			endif
*!*			xSqlStr="delete from tmpAPTen99MMHeader"
*!*			x=sqlExec(oConn,xSqlStr,'myCursor')
*!*			if x=-1
*!*				wait window "SQL Connection Error "+xSqlStr
*!*				suspend
*!*				return	
*!*			endif
*-->

		sqldisconnect(oConn)
		release oConn
		sele data_loc
		skip
	enddo
	
*!*		sele tmp1099
*!*		brow last

return

Proc GetFranAcct1099Data
	sele data_loc
	set filt to (! "_" $ region and !company_no="NAT000") &&Only NAT2 data is reported. 
	go top
	do while !eof()
		wait window "Now Processing "+alltrim(data_loc.region)+" - Franchisees..." nowait
		xFile=alltrim(data_loc.fran_acct)+"\data1099.dbf"
		if file(xFile)
			use alltrim(data_loc.fran_acct)+"\data1099" in 0
			*--> KMC 01/19/2024 - Updated so that CA frans are reported as 1099-MISC - per Dan Moore via Paul Meister.
			*8-> KMC 01/23/2024 - For CA Frans, 1099-MISC, Other Income, Amt Type 3.
			sele xRptyear as RptYear, ;
				iif(data_loc.company_no="AFRZZ1","_AFRZZ1",iif(data_loc.company_no="NAT00","_NAT000",iif(data_loc.company_no="INTXX1","_INTXX1",company_no))) as company_no, ;
				ven_fran, dlr_code, dlr_id, name_1099, dlr_addr, dlr_city, dlr_state, dlr_zip, ssn_fid, ;
				iif(left(data_loc.company_no,3) $ "LAX OAK SAC SDO",'3','NE') as AmtCode, ;
				sum(amount) as amount ;
			from alltrim(data_loc.fran_acct)+"\data1099" ;
			where data1099.rptYear=xRptYear ;
			and print1099="Y" ;
			group by company_no, dlr_code, dlr_id ;
			order by company_no, dlr_code, dlr_id ;
			into cursor tmpOut
			sele tmpOut
			scan
				scatter memvar
				insert into tmp1099 from memvar
			endscan			
			sele data1099
			use
		endif
		sele data_loc
		skip
	enddo	
return

Proc CopyDataToStorage
	wait window "Storing Data..." nowait
	sele tmp1099
	go top
	if eof()
		messagebox("No 1099 Data to Report.",0," System Alert")
		return
	else
		if !file('All1099Data.dbf')
			sele tmp1099
			copy stru to All1099Data
		endif
		if used('All1099Data')
			sele All1099Data
			use
		endif
		use All1099Data in 0 excl
		sele All1099Data
		dele all for rptYear=xRptyear
		pack
		sele tmp1099
		if !eof()
			scan
				if amount>0
					scatter memvar
					insert into All1099Data from memvar
				endif
			endscan	
		endif
		sele All1099Data
		use
		sele tmp1099	
	endif	
	wait clear
	sele tmp1099
	use
return

Proc Query1099Data
	sele xRptyear as RptYear, ;
	iif(left(company_no,1)="_","NAT002",company_no) as company_no, ;
		ven_fran, dlr_code, dlr_id, name_1099, dlr_addr, dlr_city, dlr_state, dlr_zip, ;
		ssn_fid, amount, AmtCode ;
	from All1099Data where rptyear=xRptyear ;
	order by company_no ;
	into cursor tmp1099
	sele tmp1099
	go bott
	TotB_Recs=recno()
	go top
return

Proc SetWriteFile
	SET CONSOLE OFF
	SET TEXTMERGE ON

	*xyPath=sys(5)+sys(2003)+"\"
	xFileOut=fileName
	*-->
	SET TEXTMERGE TO (xFileOut)
return

Proc WriteT_Record
	* \\ Stay on same line, \ new line
	*--> Data for Transmitter "T" Record
	recSeqNo=recSeqNo+1
	cTxt="T"  && (1) Record Indicator
	\\<<cTxt>>
	cTxt=PADR(alltrim(str(xRptYear)),4," ") && (2-5) Payment Year
	\\<<cTxt>>
	cTxt=" " && (6) Enter "P" if reporting a prior year.
	\\<<cTxt>>
	cTxt="752074877" && (7-15) Transmitter TIN
	\\<<cTxt>>
	cTxt="34572" && (16-20) Transmitter Control Code
	\\<<cTxt>>
	cTxt=REPLICATE(" ",7) && (21-27) Blank
	\\<<cTxt>>
	cTxt=xTestInd && (28) Enter a "T" if test file, otherwise leave blank
	\\<<cTxt>>
	cTxt=" "  && (29) Foreign Entity Indicator
	\\<<cTxt>>
	cTxt=PADR("JANI-KING INTERNATIONAL, INC.",40," ") && (30-69) Transmitter Name
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (70-109) Transmitter Name - Cont.
	\\<<cTxt>>
	cTxt=PADR("JANI-KING INTERNATIONAL, INC.",40," ") && (110-149) Company Name
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (150-189) Company Name - Cont.
	\\<<cTxt>>
	cTxt=PADR("16885 DALLAS PARKWAY",40," ") && (190-229) Company Mailing Address
	\\<<cTxt>>
	cTxt=PADR("ADDISON",40," ") && (230-269) Company City
	\\<<cTxt>>
	cTxt="TX" && (270-271) Company State
	\\<<cTxt>>
	cTxt=PADR("75001",9," ") && (272-280) Company Zip
	\\<<cTxt>>
	cTxt=REPLICATE(" ",15) && (281-295) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(str(TotB_Recs)),8,"0") && (296-303) Total Number of Payee "B" records	
	\\<<cTxt>>
	cTxt=PADR("KEVIN CAVANAUGH",40," ") && (304-343) Contact Name
	\\<<cTxt>>
	cTxt=PADR("9729910900185",15," ") && (344-358) Contact Phone
	\\<<cTxt>>
	cTxt=PADR("KCAVANAUGH@JANIKING.COM",50," ") && (359-408) Contact Email
	\\<<cTxt>>
	cTxt=REPLICATE(" ",91) && (409-499) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(str(recSeqNo)),8,"0") && (500-507) Record Sequence Number
	\\<<cTxt>>
	cTxt=REPLICATE(" ",10) && (508-517) Blank
	\\<<cTxt>>
	cTxt="I" && (518) I = In-House Software created submission, V = Software from Vendor or other source created submission.
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (519-558) Vendor Name
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (559-598) Vendor Mailing Address
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (599-638) Vendor City
	\\<<cTxt>>
	cTxt=REPLICATE(" ",2) && (639-640) Vendor State
	\\<<cTxt>>
	cTxt=REPLICATE(" ",9) && (641-649) Vendor Zip
	\\<<cTxt>>
	cTxt=REPLICATE(" ",40) && (650-689) Vendor Contact Name
	\\<<cTxt>>
	cTxt=REPLICATE(" ",15) && (690-704) Vendor Phone & Extension
	\\<<cTxt>>
	cTxt=REPLICATE(" ",35) && (705-739) Vendor Contact Email Address
	\\<<cTxt>>
	cTxt=" " && (740) Enter a 1 (one) if Foreign Entity
	\\<<cTxt>>
	cTxt=REPLICATE(" ",8) && (741-748) Blank
	\\<<cTxt>>
	cTxt=REPLICATE(" ",2) && (749-750) Blank
	\\<<cTxt>>
return	

Proced WriteA_Record
	sele tmp1099
	xCompNo=alltrim(tmp1099.company_no)

	*--> Set what AmtCodes are being reported for this region.
	curRec=RecNo()
	AmtCodeType1=""
	AmtCodeType3=""
	*--> 01/19/2023 - Added Type 7 back in (1099-MISC) for CA per Dan Moore via Paul Meister.
	AmtCodeType7=""  && KMC 12/16/2020 - AmtCode7 no longer used, is now AmtCode 1 for Return Type 1099-NEC (Non-Employee Compensation).
	*-->
	AmtCodeTypeC=""
	do while !eof() and company_no=xCompNo
		*--> KMC 12/16/2020
		*if tmp1099.AmtCode="1" Now reported as AmtCode1 for 1099-NEC (Return Type NE)
		if tmp1099.AmtCode="1" or tmp1099.AmtCode = "NE"
		*-->
			AmtCodeType1="1"
		endif				
		if tmp1099.AmtCode="3" && KMC 01/23/2024 - CA Frans are now 1099-MISC with Amt Type of 3 (Other Income).
			AmtCodeType3="3"
		endif				
		*--> KMC 01/24/2023 - Amt type 7 is no longer valid for 1099-MISC (Amt Type 7 for 1099-MISC was for Non-Empl Comp), 1099-MISC for CA Frans is now Amt Type 3.
		*--> KMC 01/19/2024 - Added type 7 back in for CA per Dan Moore via Paul Meister.
		*--> KMC 12/16/2020 - Now reported as AmtCode1 for 1099-NEC (Return Type NE)
		*--> Waiting to hear from Paul M. as to what AmtCode 7's for Vendors should be changed to.
		if tmp1099.AmtCode="7"
			AmtCodeType7="7"
		endif				
		*-->
		*-->
		if tmp1099.AmtCode="C"
			AmtCodeTypeC="C"
		endif				
		skip
	enddo
	go curRec	
	*-->
	
	sele Data_Loc 
	loca for data_loc.company_no=xCompNo   
	use alltrim(data_loc.Fran_Acct)+"\jkcmpfil" in 0
	sele jkcmpfil
	if data_loc.company_no="NAT002"
		loca for company_no="NAT99"  && Only used to get JK INT FedID and Address Info - All 1099's from JK INT companties (AFR, NAT, INT, ADV) all use same info.
	else
		loca for company_no=data_loc.company_no
	endif
	if found()
		recSeqNo=recSeqNo+1
		TotA_Recs=TotA_Recs+1
		cTxt="A"  && (1) Record Type
		\<<cTxt>>
		cTxt=alltrim(str(xRptYear))  && (2-5) Payment Year
		\\<<cTxt>>
		cTxt=" "  && (6) Combined Federal/State Filing Program - enter a "1" (one) if participating.
		\\<<cTxt>>
		cTxt=REPLICATE(" ",5)  && (7-11) Blank
		\\<<cTxt>>
		cTxt=PADR(alltrim(strtran(jkcmpfil.fed_id,"-","")),9," ") && 12-20)
		\\<<cTxt>>
		cTxt=REPLICATE(" ",4)  && (21-24) Payer Name Control (Can be left blank)
		\\<<cTxt>>
		if xCompNo $ LastFiling
			cTxt="1" && (25) Last Filing Indicator enter a "1" (one) if this is the last year this payer name and TIN will file.
		else
			cTxt=" " && (25) Leave blank except when this is the last year this payer name and TIN will file.
		endif
		\\<<cTxt>>
	
		*--> KMC 12/16/2020 - To accomodate new method for reporting non-employee compensation - 1099-NEC
		*cTxt="A " && (26,27) Type of Return - "A " = 1099 MISC
		if tmp1099.AmtCode <> "NE"
			cTxt="A " && (26,27) Type of Return - "A " = 1099-MISC
		else
			cTxt="NE" && (26,27) Type of Return - "NE" = 1099-NEC
		endif
		*-->
	
		\\<<cTxt>>
		
		*--> KMC 01/22/2024 - Added type 7 back in for CA per Dan Moore via Paul Meister.
		AmtCodeStr=AmtCodeType1+AmtCodeType3+AmtCodeType7+AmtCodeTypeC
		*AmtCodeStr=AmtCodeType1+AmtCodeType3+AmtCodeTypeC
		*-->
		
		cTxt=PADR(AmtCodeStr,18," ") && (28-45) Amount Codes - 1=Rents (1099-MISC) & Non-Emp Comp (1099-NEC), 3=Other Income, C=Gross proceeds paid to an attorney.
		\\<<cTxt>>
		cTxt=REPLICATE(" ",6)  && (46-51) Blank
		\\<<cTxt>>
		cTxt=" " && (52) Foreign Entity Indicator 
		\\<<cTxt>>
		if xCompNo = "NAT002"  && Data from all of the International Companies (Acct Fee Rebate, Nat1 or Nat2)
			cTxt=PADR("JANI-KING INTERNATIONAL, INC.",40," ") && (53-92) First Payer Name Line
		else
			cTxt=PADR(alltrim(jkcmpfil.dsp_name),40," ") && (53-92) First Payer Name Line
		endif
		\\<<cTxt>>
		cTxt=REPLICATE(" ",40) && (93-132) Second Payer Name Line
		\\<<cTxt>>
		cTxt="0" && (133) Transfer Agent Indicator
		\\<<cTxt>>
		cTxt=PADR(alltrim(jkcmpfil.address),40," ") && (134-173) Payer Shipping Address
		\\<<cTxt>>
		cTxt=PADR(alltrim(jkcmpfil.city),40," ") && (174-213) Payer City
		\\<<cTxt>>
		cTxt=PADR(alltrim(jkcmpfil.state),2," ") && (214-215) Payer State
		\\<<cTxt>>
		cTxt=PADR(alltrim(strtran(jkcmpfil.zip,"-","")),9," ") && (216-224) Payer Zip Code
		\\<<cTxt>>
		cTxt=PADR(alltrim(str(jkcmpfil.phone)),15," ") && (225-239) Payer's Phone & Extension
		\\<<cTxt>>
		cTxt=REPLICATE(" ",260) && (240-499) Blank
		\\<<cTxt>>
		cTxt=PADL(alltrim(str(recSeqNo)),8,"0")  && (500-507) Record Sequence Number
		\\<<cTxt>>
		cTxt=REPLICATE(" ",241) && (508-748) Blank
		\\<<cTxt>>
		cTxt=REPLICATE(" ",2) && (749-750) Blank
		\\<<cTxt>>
		use && Close jkcmpfil
	else
		messagebox("Unable to Locate Company Info - Program will Exit!",0," System Alert")
		suspend
	endif
return

Proc WriteBC_Record
	sele tmp1099
	SubCnt=0
	AmtCode1Tot=0
	AmtCode3Tot=0
	*--> KMC 12/16/2020 - AmtCode7 no longer used - see changes to WriteA_Record.
	AmtCode7Tot=0  
	*-->
	AmtCodeCTot=0
	do while tmp1099.company_no=xCompNo and !eof()
		recSeqNo=recSeqNo+1
		SubCnt=SubCnt+1
		do case
		
			*--> KMC 12/16/2020
			*case AmtCode="1"
			case AmtCode="1" or AmtCode = "NE"
			*-->
		
				AmtCode1Tot=AmtCode1Tot+Amount
			case AmtCode="3"
				AmtCode3Tot=AmtCode3Tot+Amount
			*--> KMC 01/19/2023 - Per Dan Moore via Paul Meister, type 7 back in use for CA.
			*--> KMC 12/16/2020 - AmtCode7 no longer used - see changes to WriteA_Record.
			case AmtCode="7"
				AmtCode7Tot=AmtCode7Tot+Amount
			*-->
			*-->
			case AmtCode="C"
				AmtCodeCTot=AmtCodeCTot+Amount
		endcase
		cTxt="B"  && (1) Record Type
		\<<cTxt>>
		cTxt=alltrim(str(xRptYear))  && (2-5) Payment Year
		\\<<cTxt>>
		cTxt=" " && (6) Corrected Return Indicator		
		\\<<cTxt>>
		cTxt=REPLICATE(" ",4) && (7-10) Name Control
		\\<<cTxt>>
		if substr(tmp1099.ssn_fid,3,1)="-"
			xTIN="1" && EIN
		else
			xTIN="2" && SSN
		endif	
		cTxt=xTIN && (11) Type of TIN
		\\<<cTxt>>
		cTxt=PADL(alltrim(strtran(tmp1099.ssn_fid,"-","")),9," ") && (12-20) Payer's TIN				
		\\<<cTxt>>
		cTxt=REPLICATE(" ",20) && (21-40) Payer's Account Number for Payee
		\\<<cTxt>>
		cTxt=REPLICATE(" ",4) && (41-44) Payer's Office Code
		\\<<cTxt>>
		cTxt=REPLICATE(" ",10) && (45-54) Blank
		\\<<cTxt>>
		*--> KMC 02/02/2015 - We have some records with amount code 1 now.
		*cTxt=REPLICATE("0",12) && (55-66) Payment Amount 1
		if AmtCode="1" or AmtCode = "NE"  && KMC 01/06/2021 Added 'or AmtCode - "NE"'.
			cTxt=PADL(alltrim(strtran(str(Amount,12,2),".","")),12,"0") && (55-66) Payment Amount 1 (Rents)
		else
			cTxt=REPLICATE("0",12) && (55-66) 
		endif	
		*-->
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (67-78) Payment Amount 2
		\\<<cTxt>>
		if AmtCode="3"
			cTxt=PADL(alltrim(strtran(str(Amount,12,2),".","")),12,"0") && (79-90) Payment Amount 3 (Other Income)
		else
			cTxt=REPLICATE("0",12) && (79-90) 
		endif
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (91-102) Payment Amount 4
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (103-114) Payment Amount 5
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (115-126) Payment Amount 6
		\\<<cTxt>>
		
		*--> KMC 01/19/2023 - Per Dan Moore via Paul Meister, type 7 back in use for CA.
		*--> KMC 12/16/2020 - AmtCode7 no longer used - see changes to WriteA_Record.
		if AmtCode="7"
			cTxt=PADL(alltrim(strtran(str(Amount,12,2),".","")),12,"0") && (127-138) Payment Amount 7 (Nonemployee compensation) <-- KMC 01/19/2024 Amt Code 7 is 1099-MISC
		else
			cTxt=REPLICATE("0",12)
		endif
		*cTxt=REPLICATE("0",12)
		*-->
		*-->
		
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (139-150) Payment Amount 8
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (151-162) Payment Amount 9
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (163-174) Payment Amount A
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (175-186) Payment Amount B
		\\<<cTxt>>
		if AmtCode="C"
			cTxt=PADL(alltrim(strtran(str(Amount,12,2),".","")),12,"0") && (187-198) Payment Amount C (Payments to Attorney for Legal Fees).
		else
			cTxt=REPLICATE("0",12)
		endif
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (199-210) Payment Amount D
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (211-222) Payment Amount E
		\\<<cTxt>>

		*--> KMC 11/12/2009
		*cTxt=REPLICATE(" ",24) && (223-246) Reserved
		*\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (223-234) Payment Amount F
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (235-246) Payment Amount G
		\\<<cTxt>>
		*-->

		*--> KMC 10/27/2021
		cTxt=REPLICATE("0",12) && (247-258) Payment Amount H
		\\<<cTxt>>
		cTxt=REPLICATE("0",12) && (259-270) Payment Amount J
		\\<<cTxt>>
		cTxt=REPLICATE(" ",16) && (271--286) Blank
		\\<<cTxt>>
		*-->

		cTxt=" "  && (287) Foreign Currency Indicator (1=Foreign otherwise leave blank)
		\\<<cTxt>>
		cTxt=PADR(alltrim(name_1099),40," ") && (288-327) First Payee Name Line
		\\<<cTxt>>
		cTxt=REPLICATE(" ",40) && (328-367) Second Payee Name Line
		\\<<cTxt>>
		cTxt=PADR(alltrim(dlr_addr),40," ") && (368-407) Payee Mailing Address
		\\<<cTxt>>
		cTxt=REPLICATE(" ",40) && (408-447) Blank
		\\<<cTxt>>
		cTxt=PADR(alltrim(dlr_city),40," ") && (448-487) Payee City
		\\<<cTxt>>
		cTxt=PADR(alltrim(dlr_state),2," ") && (488-489) Payee State
		\\<<cTxt>>
		cTxt=PADR(alltrim(strtran(dlr_zip,"-","")),9," ") && (490-498) Payee Zip
		\\<<cTxt>>
		cTxt=" " && (499) Blank
		\\<<cTxt>>
		cTxt=PADL(alltrim(str(recSeqNo)),8,"0")  && (500-507) Record Sequence Number
		\\<<cTxt>>
		cTxt=REPLICATE(" ",36) && (508-543) Blank
		\\<<cTxt>>
		cTxt=REPLICATE(" ",1) && (544) Sewcond TIN Indicator
		\\<<cTxt>>
		cTxt=REPLICATE(" ",2) && (545-546) Blank
		\\<<cTxt>>
		cTxt=" " && (547) Direct Sales Indicator
		\\<<cTxt>>
		cTxt=REPLICATE(" ",173) && (548-722) Blank
		\\<<cTxt>>
		cTxt=REPLICATE(" ",12) && (723-734) State Income Tax Withheld
		\\<<cTxt>>
		cTxt=REPLICATE(" ",12) && (735-746) Local Income Tax Withheld
		\\<<cTxt>>
		cTxt=REPLICATE(" ",2) && (747-748) Blank
		\\<<cTxt>>
		cTxt=REPLICATE(" ",2) && (749-750) Blank
		\\<<cTxt>>
		skip
	enddo	

	*--> Write "C" Record - Totals for Payer
	recSeqNo=recSeqNo+1
	cTxt="C"  && (1) Record Type
	\<<cTxt>>
	cTxt=PADL(alltrim(str(SubCnt)),8,"0")  && (2-9) Number of Payees - Total "B" records covered by preceding "A" record
	\\<<cTxt>>
	cTxt=REPLICATE(" ",6) && (10-15) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(strtran(str(AmtCode1Tot,18,2),".","")),18,"0") && (16-33) Totals for Payment Amount 1
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (34-51) Totals for Payment Amount 2
	\\<<cTxt>>
	cTxt=PADL(alltrim(strtran(str(AmtCode3Tot,18,2),".","")),18,"0") && (52-69) Totals for Payment Amount 3
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (70-87) Totals for Payment Amount 4
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (88-105) Totals for Payment Amount 5
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (106-123) Totals for Payment Amount 6
	\\<<cTxt>>
	
	*--> KMC 01/19/2024 - Per Dan Moore via Paul Meister, Amt Code 7 back in play for CA.
	*--> KMC 12/16/2020
	cTxt=PADL(alltrim(strtran(str(AmtCode7Tot,18,2),".","")),18,"0") && (124-141) Totals for Payment Amount 7
	*cTxt=REPLICATE("0",18) && (124-142) Totals for Payment Amount 7
	*-->
	*-->
	
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (142-159) Totals for Payment Amount 8
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (160-177) Totals for Payment Amount 9
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (178-195) Totals for Payment Amount A
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (196-213) Totals for Payment Amount B
	\\<<cTxt>>
	cTxt=PADL(alltrim(strtran(str(AmtCodeCTot,18,2),".","")),18,"0") && (214-231) Total for Payment Amount C (Payments to Attorney for Legal Fees.
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (232-249) Totals for Payment Amount D
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (250-267) Totals for Payment Amount E
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (268-285) Totals for Payment Amount F
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (286-303) Totals for Payment Amount G
	\\<<cTxt>>

	*--> KMC 10/27/2021
	cTxt=REPLICATE("0",18) && (304-321) Totals for Payment Amount H
	\\<<cTxt>>
	cTxt=REPLICATE("0",18) && (322-339) Totals for Payment Amount J
	\\<<cTxt>>
	*-->
	
	cTxt=REPLICATE(" ",160) && (340-499) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(str(recSeqNo)),8,"0")  && (500-507) Record Sequence Number
	\\<<cTxt>>
	cTxt=REPLICATE(" ",241) && (508-748) Blank
	\\<<cTxt>>
	cTxt=REPLICATE(" ",2) && (749-750) Blank
	\\<<cTxt>>
	if eof()
		AllDone=.T.
	endif	
Return

Proced WriteF_Record
	recSeqNo=recSeqNo+1
	cTxt="F"  && (1) Record Type
	\<<cTxt>>
	cTxt=PADL(alltrim(str(TotA_Recs)),8,"0")  && (2-9) Total Payer "A" Records
	\\<<cTxt>>
	cTxt=REPLICATE("0",21) && (10-30) Zeros
	\\<<cTxt>>
	cTxt=REPLICATE(" ",19) && (31-49) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(str(TotB_Recs)),8,"0")  && (50-57) Total Payee "B" Records - can be left blank if count is reported in T record (which it is)
	\\<<cTxt>>
	cTxt=REPLICATE(" ",442) && (58-499) Blank
	\\<<cTxt>>
	cTxt=PADL(alltrim(str(recSeqNo)),8,"0")  && (500-507) Record Sequence Number
	\\<<cTxt>>
	cTxt=REPLICATE(" ",241) && (508-748) Blank
	\\<<cTxt>>
	cTxt=REPLICATE(" ",2) && (749-750) Blank
	\\<<cTxt>>
Return
