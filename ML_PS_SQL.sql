

/* 1.) --------------------------------------
		pt with ICD or CRT-D
*/
DROP TABLE IF EXISTS #pt_ICD;
SELECT DISTINCT a.*,
	   CAST(b.DeathDateTime as date) as DeathDate
INTO #pt_ICD
FROM Dflt.Demographics as a left JOIN 
		(SELECT ScrSSN, MAX(DeathDateTime) as DeathDateTime from Src.SPatient_SPatient 
			WHERE DeathDateTime <= CAST('2023/12/31' as date) OR DeathDateTime IS NULL
			group by ScrSSN) AS b on a.ScrSSN = b.ScrSSN
WHERE GeneratorType = 'ICD'


/* 2.) --------------------------------------
		 HF date 
*/
DROP TABLE IF EXISTS #pt_HF;
with ot1 AS
	 (SELECT a.scrssn, id.VisitSID, id.sta3n, VisitDateTime
		FROM Src.Outpat_VDiagnosis id inner join CDWWork.Dim.ICD9 as zz on id.ICD9SID = zz.ICD9SID
									  inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN from #pt_ICD) as a on a.ScrSSN = x.ScrSSN
			where (zz.ICD9Code like '402._1%' or zz.ICD9Code like '404._1%' or zz.ICD9Code like '404._3%' or zz.ICD9Code like '428%')
			  AND VisitDateTime is not null
			  AND WorkloadLogicFlag = 'Y'
			  AND VisitDateTime <= CAST('2020-09-30' as date)
	UNION
	  SELECT a.scrssn, id.VisitSID, id.sta3n, VisitDateTime
		FROM Src.Outpat_VDiagnosis id inner join CDWWork.Dim.ICD10 as zz on id.ICD10SID = zz.ICD10SID
									  inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN from #pt_ICD) as a on a.ScrSSN = x.ScrSSN
			where (zz.ICD10Code like 'I11.0%' OR zz.ICD10Code like 'I13.0%' OR zz.ICD10Code like '13.2%' OR zz.ICD10Code like 'I50%')
			  AND VisitDateTime is not null
			  AND WorkloadLogicFlag = 'Y'
			  AND VisitDateTime <= CAST('2020-09-30' as date)
	 ), 
	in1 as (
	SELECT a.scrssn, id.sta3n, id.InpatientSID, dischargedatetime, ICD9Code as ICDCode, OrdinalNumber
			FROM Src.Inpat_InpatientDiagnosis id inner join CDWWork.Dim.ICD9 as zz on id.ICD9SID = zz.ICD9SID 
												 inner join dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
												 inner join (SELECT DISTINCT ScrSSN from #pt_ICD) as a on a.scrssn = x.scrssn
		   WHERE dischargeDateTime <= cast('2020-09-30' as date) 
			 AND dischargeDateTime is not null
			 AND (zz.ICD9Code like '402._1%' or zz.ICD9Code like '404._1%' or zz.ICD9Code like '404._3%' or zz.ICD9Code like '428%')
		UNION
		 SELECT a.scrssn, id.sta3n, id.InpatientSID, dischargedatetime, ICD10Code as ICDCode, OrdinalNumber
			FROM Src.Inpat_InpatientDiagnosis id inner join CDWWork.Dim.ICD10 as zz on id.ICD10SID = zz.ICD10SID
												 inner join dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
												 inner join (SELECT DISTINCT ScrSSN from #pt_ICD) as a on a.scrssn = x.scrssn
		   WHERE dischargeDateTime <= cast('2020-09-30' as date) 
			 AND dischargeDateTime is not null
			 AND (zz.ICD10Code like 'I11.0%' OR zz.ICD10Code like 'I13.0%' OR zz.ICD10Code like '13.2%' OR zz.ICD10Code like 'I50%')),
	comb as (
	SELECT scrssn, CAST(VisitDateTime as date) as VisitDateTime from ot1
	union
	SELECT scrssn, dischargedatetime as VisitDateTime from in1),
	fst as (
		SELECT ScrSSN, cast(min(VisitDateTime) as date) as HF_date 
		FROM comb
		group by ScrSSN)
SELECT a.ScrSSN, CAST(ImplantDate as DATE) as ImplantDate,  b.HF_date,
		(SELECT max(v)
				FROM (VALUES (ImplantDate), (HF_date), (CAST('2015-07-01' as DATE))) AS value(v)) AS Eligible_date
INTO #pt_HF 
from #pt_ICD as a left join fst as b on a.ScrSSN = b.ScrSSN


/* 3.) --------------------------------------
		Baseline LVEF
*/
DROP TABLE IF EXISTS #DR_LVEF;
with ef0 as (
select distinct a.ScrSSN, CAST(ef.ValueDateTime as DATE) as ValueDateTime,
	CASE WHEN ef.High_Value IS NULL THEN Low_Value
	ELSE ef.High_Value
	END AS LVEF,
	ef.Low_Value, ef.High_Value, ef.valuestring
		FROM Src.VINCI_TIU_NLP_LVEF as ef inner join Dflt.CohortCrossWalk as x on ef.PatientSID = x.PatientSID
										  inner join #pt_HF as a on x.ScrSSN = a.ScrSSN
	WHERE ValueDateTime <= CAST('2023-12-31' AS DATE)
	   AND ValueDateTime <= Eligible_date),
	ef1  as (
		SELECT ScrSSN, ValueDateTime, AVG(LVEF) as LVEF
		FROM ef0
		GROUP BY ScrSSN, ValueDateTime),
	ef2 as (
	SELECT ScrSSN, ValueDateTime, LVEF,
			ROW_NUMBER() OVER(partition by ScrSSN order by ScrSSN, ValueDateTime desc) as rn
		from ef1
	),
	ef3 as (
	SELECT * from ef2 where rn = 1
	)
SELECT a.ScrSSN, ImplantDate, HF_date, CAST(Eligible_date as date) as Eligible_date, 
		b.LVEF as LVEF_BL, b.ValueDateTime as LVEF_BLDT
INTO #DR_LVEF
	from #pt_HF as a left join ef3 as b on a.ScrSSN = b.ScrSSN
GO


/* 4.) --------------------------------------
		Exposure
		ACEIs, ARBs, ARNI
*/

-- outpatient rx
DROP TABLE IF EXISTS #ACEIARB;
select a.ScrSSN, ImplantDate, HF_date, Eligible_date,
		CASE WHEN y.DrugNameWithoutDose = 'SACUBITRIL/VALSARTAN' THEN 'ARNI'
				ELSE 'ACEI/ARB' END AS Arm,
		y.DrugNameWithoutDose, y.DrugNameWithDose, z.StrengthNumeric, b.QtyNumeric, b.DaysSupply, 
		b.ReleaseDateTime,
		cast(b.ReleaseDateTime as date) as startdate,
		cast(DATEADD(day, DaysSupply, ReleaseDateTime) as date) as enddate
into #ACEIARB
	FROM #DR_LVEF  as a inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
						inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
						inner join Dflt.dim_rx as y on y.NationalDrugSID = b.NationalDrugSID
						inner join CDWWork.Dim.NationalDrug as z on y.NationalDrugSID = z.NationalDrugSID
		WHERE y.RX_Cat in ('RX_ACEIs_ARBs')
		AND ReleaseDateTime IS NOT NULL
		AND ReleaseDateTime <= CAST('2020-09-30' AS DATE)
		AND DaysSupply IS NOT NULL
GO


/*** first prescription of ARNI ***/
DROP TABLE IF EXISTS #ARNI_all;
with ARNI0 as (
	SELECT ScrSSN, Eligible_date, Arm, min(startdate) as ARNI_1stDate
		FROM #ACEIARB
		WHERE Arm = 'ARNI'
		GROUP BY ScrSSN, Eligible_date, Arm
	),
	ARNI1 as (
	SELECT * FROM ARNI0 WHERE ARNI_1stDate >= Eligible_date
	)
SELECT a.*
INTO #ARNI_all
	FROM #ACEIARB as a inner join ARNI1 as b on a.ScrSSN = b.ScrSSN and a.Arm = b.Arm
		WHERE ReleaseDateTime >= ARNI_1stDate

/*** 1st ACEI/ARB after eligible ***/
SELECT *
INTO #ACEI_all
	FROM #ACEIARB 
	WHERE ReleaseDateTime >= Eligible_date
	  AND Arm = 'ACEI/ARB'


/*** CURSOR ***/
DROP TABLE IF EXISTS #Prescriptions;
  CREATE TABLE #Prescriptions
(
  prescriptionid INT  NOT NULL IDENTITY,
  ScrSSN      varchar(9)  NOT NULL,
  Arm     varchar(100)  NOT NULL,
  startdate      DATE NOT NULL,
  DaysSupply    INT  NOT NULL,
  enddate AS DATEADD(day, DaysSupply, startdate),
  CONSTRAINT CHK_Prescriptions_ed_sd CHECK(DaysSupply > 0)
);

CREATE UNIQUE CLUSTERED INDEX idx_start
  ON #Prescriptions
   (ScrSSN, Arm, startdate, prescriptionid);

ALTER TABLE #Prescriptions
  ADD CONSTRAINT PK_Prescriptions PRIMARY KEY
    NONCLUSTERED(prescriptionid);

INSERT INTO #Prescriptions
  (ScrSSN, Arm, startdate, DaysSupply) 
select scrssn, Arm, startdate, DaysSupply
from (SELECT * from #ARNI_all
		UNION
	  SELECT * from #ACEI_all) a
go


-- create treatment episode with gap < 30 days
DROP TABLE IF EXISTS #exposure;
SET NOCOUNT ON;
DECLARE @Result AS TABLE
(
  ScrSSN varchar(9)  NOT NULL,
  Arm    varchar(100)  NOT NULL,
  startdate DATE NOT NULL,
  enddate   DATE NOT NULL,
  daystotal INT NOT NULL
);

DECLARE
  @ScrSSN      AS varchar(9),
  @Arm     AS varchar(100),
  @startdate      AS DATE,
  @DaysSupply    AS INT,
  @sumnumdays     AS INT,
  @prevScrSSN  AS varchar(9),
  @prevArm     AS varchar(100),
  @prevDaysSupply AS INT,
  @daystot as INT,
  @firststartdate AS DATE;

DECLARE C CURSOR FAST_FORWARD FOR
  SELECT ScrSSN, Arm, startdate, DaysSupply
  FROM #Prescriptions
  ORDER BY ScrSSN, Arm, startdate, prescriptionid;

OPEN C;

FETCH NEXT FROM C INTO
  @ScrSSN, @Arm, @startdate, @DaysSupply;

SELECT
  @prevScrSSN  = @ScrSSN,
  @prevArm     = @Arm,
  @prevDaysSupply    = @DaysSupply,
  @firststartdate = @startdate,
  @daystot        = 0,
  @sumnumdays     = 0;

WHILE @@fetch_status = 0
BEGIN

  IF    @prevScrSSN <> @ScrSSN
     OR @prevArm    <> @Arm
     OR DATEADD(day, @sumnumdays+30, @firststartdate)
      <   @startdate
 BEGIN --begin loop to end this run
   INSERT INTO @Result(ScrSSN, Arm, startdate, enddate, daystotal)
   VALUES(@prevScrSSN, @prevArm, @firststartdate,
      DATEADD(day, @sumnumdays, @firststartdate), @daystot);

  SELECT  --create new startdate
    @firststartdate = @startdate,
    @sumnumdays     = 0,
	@daystot        = 0;
 END --end loop to end this run


 --
 IF @prevScrSSN = @prevScrSSN
   AND @prevArm = @Arm
   AND DATEADD(day, @sumnumdays, @firststartdate)
      < @startdate
	AND DATEADD(day, @sumnumdays + 30, @firststartdate)
	  >= @startdate

   BEGIN
      SELECT 
	    @sumnumdays += DATEDIFF(day, DATEADD(day, @sumnumdays, @firststartdate), @startdate)
   END


  SELECT
    @sumnumdays    += @DaysSupply,
	@daystot       += @DaysSupply,
    @prevScrSSN  = @ScrSSN,
    @prevArm     = @Arm,
	@prevDaysSupply    = @DaysSupply;

  FETCH NEXT FROM C INTO
    @ScrSSN, @Arm, @startdate, @DaysSupply;
END

IF @sumnumdays > 0
  INSERT INTO @Result(ScrSSN, Arm, startdate, enddate, daystotal)
    VALUES(@prevArm, @prevArm, @firststartdate,
  DATEADD(day, @sumnumdays, @firststartdate), @daystot);

CLOSE C;

DEALLOCATE C;
SET NOCOUNT OFF;

SELECT ScrSSN, Arm, startdate, enddate, daystotal 
into #exposure
FROM @Result;


DROP TABLE IF EXISTS #Exposure;
with a0 as (
	SELECT *,
			row_number() over (partition by ScrSSN, Arm order by ScrSSN, Arm, startdate) as rn
	FROM #exposure
	)
select b.ScrSSN, b.ImplantDate, b.HF_date, b.Eligible_date,  
		a.Arm, a.startdate, a.enddate, a.daystotal,
		b.LVEF_BL, b.LVEF_BLDT
INTO #Exposure
FROM a0 as a inner join #DR_LVEF as b on a.ScrSSN = b.ScrSSN
WHERE rn = 1
GO


/* 5.) --------------------------------------------------------------
		User type:
		  a. prevalent user: had ACEI/ARB  in priori 180 days.
		  b. new user: without ACEI/ARB in the past 180 days.
*/
DROP TABLE IF EXISTS #usertype;
with ACEI0 as (
	SELECT ScrSSN, Arm, DrugNameWithoutDose, startdate as prior_startdate, enddate as prior_enddate
		FROM #ACEIARB
		WHERE Arm = 'ACEI/ARB'
	),
	a1 as (
	SELECT a.ScrSSN, a.Arm, a.startdate, b.prior_startdate, b.prior_enddate,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, a.Arm, startdate ORDER BY a.ScrSSN, a.Arm, startdate, prior_startdate desc) as rn
		FROM #Exposure as a left join ACEI0 as b on a.ScrSSN = b.ScrSSN
			WHERE b.prior_startdate < a.startdate
	),
	a2 as (
	SELECT *,
			DATEDIFF(DAY, prior_enddate, startdate) as diff
		FROM a1
		where rn = 1
	)
	SELECT a.*, b.diff,
		CASE WHEN diff <= 180 THEN 'Prevalent'
		ELSE 'New' END AS UserType
INTO #usertype
		FROM #Exposure as a left join a2 as b on a.ScrSSN = b.ScrSSN and a.Arm = b.Arm and a.startdate = b.startdate


/* 6.) --------------------------------------------------------------
		Death date from DAF
*/
DROP TABLE IF EXISTS #DeathDates_all;

DECLARE @lastDAFdate AS datetime2(0);
SET @lastDAFdate = (SELECT MAX(MPI_DOD) FROM CDWWork.SDAF.DAFMaster);

WITH DEATH AS (
	SELECT COH.ScrSSN,
		   CAST(
				COALESCE(
						DAF.MPI_DOD,
						DAF.Missing_DOD,
						MVI.DeathDateTime,
						PAT.DeathDateTime) AS DATE) AS DeathDate,
			CASE WHEN DAF.MPI_DOD IS NOT NULL THEN 'DAF.MPI_DOD'
				 WHEN DAF.Missing_DOD IS NOT NULL THEN 'DAF.Missing_DOD'
				 WHEN MVI.DeathDateTime IS NOT NULL THEN 'MVI.DeathDateTime'
				 WHEN PAT.DeathDateTime IS NOT NULL THEN 'PAT.DeathDateTime'
			END AS DeathDateSource
		FROM dflt.CohortCrosswalk as COH LEFT JOIN Src.SDAF_DAFMaster AS DAF ON COH.PatientICN = DAF.MPI_ICN
										LEFT JOIN Src.Veteran_MVIPerson as MVI on COH.PatientICN = MVI.MVIPersonICN AND MVI.DeathDateTime > @lastDAFdate
										LEFT JOIN Src.Patient_Patient AS PAT on COH.PatientICN = PAT.PatientICN AND PAT.DeathDateTime > @lastDAFdate
										)
SELECT DISTINCT *
INTO #DeathDates_all
FROM DEATH
WHERE DeathDate IS NOT NULL

DROP TABLE IF EXISTS #Exposure2;
SELECT DISTINCT a.*,
		b.UserType,
		CASE WHEN DeathDate > CAST('2020-09-30' as date) THEN NULL
			ELSE DeathDate END AS DeathDate
INTO #Exposure2
	FROM #Exposure as a left join #usertype as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate and a.arm = b.arm
						left join #DeathDates_all as c on a.ScrSSN = c.ScrSSN
	WHERE a.HF_date is not null



/* 7.) --------------------------------------------------------------
		Demographics 
*/

--- SMK
DROP TABLE IF EXISTS #smk;
with  smk1 as (
	SELECT a.ScrSSN, a.StartDate,
			hf.HealthFactorDateTime, hf.HealthFactorDateSID,
			dim.HealthFactorType, s.SmokingFactor
		FROM #Exposure2 as a inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
							 inner join CDWWork.HF.HealthFactor as hf on x.PatientSID = hf.PatientSID
							 inner join CDWWork.Dim.HealthFactorType as dim on hf.HealthFactorTypeSID = dim.HealthFactorTypeSID
							 inner join Dflt.HF_SMOKING_LOOKUP as s on dim.HealthFactorType = s.HEALTHFACTORTYPE
			WHERE HealthFactorDateTime <= StartDate
	),
	Smoking_ever as (
 --count each instance regardless of timing
SELECT count(*) as cnt, scrssn, StartDate, smokingFactor
  FROM smk1
  group by scrssn, StartDate, smokingFactor
  ), 
  smoking_wide as (
 --create one record per person file
	select pts.scrssn, pts.StartDate,
	   case when SMKcurrent > 0 then SMKcurrent else 0 end as SMKcurrent, 
	   case when SMKpast > 0 then SMKpast else 0 end as SMKpast, 
	   case when SMKnever > 0 then SMKnever else 0 end as SMKnever
	from
	  (select distinct scrssn, StartDate from smoking_ever) pts  --all patients in file
	  left join
	  (select scrssn, StartDate, cnt as SMKcurrent
	  FROM Smoking_ever
	  where smokingfactor = 'CURRENT SMOKER') a
	  on pts.scrssn = a.scrssn and pts.StartDate = a.StartDate
	  left join
	  (select scrssn, StartDate, cnt as SMKpast
	  from Smoking_ever
	  where smokingfactor = 'FORMER SMOKER') b
	  on pts.scrssn = b.scrssn and pts.StartDate = b.StartDate
	  left join
	  (select scrssn, StartDate, cnt as SMKnever
	  from Smoking_ever
	  where smokingfactor = 'NEVER SMOKER') C
	  ON pts.SCRSSN = C.SCRSSN and pts.StartDate = c.StartDate
  ),
  Smoking_smkhfcom as (
  --select most frequent response into smkhfcom variable
  --0=Never smoked; 1=current smoker; 2=past smoker
  select a.*, b.MostcommonNum
  ,case when b.mostcommonnum = 0 then null
		when a.SMKcurrent = b.MostcommonNum then 1
		when a.SMKpast = b.MostcommonNum then 2
		when a.smknever = b.mostcommonnum then 0 end as SMKHFCOM
  from smoking_wide a
  inner join
  (
  select scrssn, StartDate, max(counts) as MostcommonNum
  from
  (select scrssn, StartDate, smkcurrent as counts
  from Smoking_wide
  union
  select scrssn, StartDate, smkpast as counts
  from Smoking_wide
  union
  select scrssn, StartDate, smknever as counts
  from Smoking_wide) sub
  group by scrssn, StartDate) b
  on a.scrssn = b.scrssn and a.StartDate = b.StartDate
  ), 
  mostcomm as (SELECT ScrSSN, StartDate, SMKHFCOM from Smoking_smkhfcom
  	where mostcommonnum > 0                                                                      
	)
  --create permanent dataset
  select a.ScrSSN, a.StartDate, b.SMKHFCOM
  into #smk
  from #Exposure2 as a left join mostcomm as b on a.ScrSSN = b.ScrSSN and a.StartDate = b.StartDate
 GO


-- GISURH
DROP TABLE IF EXISTS #GIS;
with g0 as (
SELECT DISTINCT a.ScrSSN, a.startdate, GISPatientAddressLongitude, GISPatientAddressLatitude, b.URH, b.zip, GISAddressUpdatedDate,
	ABS(DATEDIFF(DAY, GISAddressUpdatedDate, a.startdate)) as diff
FROM #Exposure2 as a left join dflt.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
					 left join SRC.SPatient_SPatientGISAddress as b on x.PatientSID = b.PatientSID),
	g1 as (
	SELECT *, 
			row_number() over (partition by scrssn, startdate order by scrssn, startdate, diff) as RN
		FROM g0)
SELECT *
INTO #GIS
FROM g1
WHERE rn = 1
GO


/* 8.) --------------------------------------------------------------
		 weight, height
*/

DROP TABLE IF EXISTS Dflt.DR_Weights;
SELECT a.ScrSSN,
	   b.VitalResultNumeric as WEIGHT, b.VitalSignTakenDateTime
INTO Dflt.DR_Weights
FROM  (SELECT DISTINCT ScrSSN FROM #Exposure2) as a INNER JOIN Dflt.CohortCrossWalk as x on a.scrssn = x.ScrSSN
													INNER JOIN Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
													INNER JOIN CDWWork.Dim.VitalType as c on b.VitalTypeSID = c.VitalTypeSID
WHERE VitalSignTakenDateTime IS NOT NULL
		AND VitalResultNumeric IS NOT NULL
		AND VitalResultNumeric > 75
		AND VitalResultNumeric <700
		AND VitalType = 'WEIGHT'
		AND VitalSignTakenDateTime <= CAST('2020-09-30' as date)

-- baseline weight
DROP TABLE IF EXISTS #weight;
WITH w0 as (
	SELECT a.ScrSSN, a.startdate,
			b.WEIGHT, b.VitalSignTakenDateTime,
			row_number() over (partition by a.scrssn, a.startdate order by a.scrssn, a.startdate, VitalSignTakenDateTime DESC) as RN
	FROM #Exposure2 as a left join Dflt.DR_Weights as b on a.ScrSSN = b.ScrSSN
		WHERE VitalSignTakenDateTime <= startdate
	)
SELECT ScrSSN, startdate, WEIGHT as Weight_BL, VitalSignTakenDateTime as WeightDT
INTO #weight 
from w0
where rn = 1

-- baseline height
DROP TABLE IF EXISTS #height;
with h0 as (
		SELECT a.ScrSSN, a.startdate,
			b.VitalResultNumeric as HEIGHT, b.VitalSignTakenDateTime
	FROM #Exposure2 as a INNER JOIN Dflt.CohortCrosswalk as x on a.scrssn = x.ScrSSN
						 INNER JOIN Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
						 INNER JOIN CDWWork.Dim.VitalType as c on b.VitalTypeSID = c.VitalTypeSID
		WHERE VitalSignTakenDateTime <= startdate
			AND VitalSignTakenDateTime IS NOT NULL
			AND VitalResultNumeric IS NOT NULL
			AND VitalResultNumeric > 48
			AND VitalResultNumeric < 84
			AND VitalType = 'HEIGHT'
			AND VitalSignTakenDateTime <= CAST('2020-09-30' as date)
)
SELECT distinct ScrSSN, startdate,
	   PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY HEIGHT) OVER (PARTITION BY ScrSSN, startdate) as HEIGHT_BL
INTO #height
from h0
GO


DROP TABLE IF EXISTS Dflt.DR_Exposure;
SELECT distinct a.*,
				GeneratorType, CRT, cast(BirthDateTime as date) as BirthDate, 
				Gender, Race, Ethnicity, EmploymentStatus, Eligibility, MeansTestStatus,
				Weight_BL, cast(WeightDT as date) as Weight_BLDT, 
				HEIGHT_BL, e.SMKHFCOM, f.zip, f.URH as GISURH
INTO Dflt.DR_Exposure
		FROM #exposure2 as a left join Dflt.Demographics as b on a.ScrSSN = b.ScrSSN
							 left join #weight as c on a.ScrSSN = c.ScrSSN and a.StartDate = c.StartDate
						     left join #height as d on a.ScrSSN = d.ScrSSN and a.StartDate = d.StartDate
						     left join #smk as e on a.ScrSSN = e.ScrSSN and a.StartDate = e.StartDate
							 left join #GIS as f on a.ScrSSN = f.ScrSSN and a.StartDate = f.StartDate
GO


/* 9.) --------------------------------------------------------------
		 BP
*/

DROP TABLE IF EXISTS Dflt.DR_BP;
SELECT a.ScrSSN, 
	   dim.VitalType,
	   b.VitalSignTakenDateTime, b.VitalResult, b.Systolic, b.Diastolic
INTO Dflt.DR_BP
	FROM (SELECT DISTINCT ScrSSN from Dflt.DR_Exposure) as a inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
					inner join Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
					inner join CDWWork.Dim.VitalType as dim on b.VitalTypeSID = dim.VitalTypeSID
	WHERE dim.VitalType in ('BLOOD PRESSURE')
		AND VitalSignTakenDateTime IS NOT NULL
		AND Systolic is not null 
		AND Diastolic is not null
		AND Systolic != 0 
		AND Diastolic != 0
		AND Systolic > Diastolic
		AND VitalSignTakenDateTime <= CAST('2020-09-30' as date)
GO

DROP TABLE IF EXISTS #BP_BL;
with bp1 as (
	SELECT a.ScrSSN, startdate, b.VitalSignTakenDateTime, Systolic, Diastolic
		FROM Dflt.DR_Exposure as a left join Dflt.DR_BP as b on a.ScrSSN = b.ScrSSN
		 where VitalSignTakenDateTime <= startdate
	),
	bp2 as (
		SELECT *,
				ROW_NUMBER() OVER (PARTITION BY ScrSSN, startdate ORDER BY ScrSSN, startdate, VitalSignTakenDateTime DESC) AS RN
		FROM bp1
	)
SELECT  * 
	into #BP_BL
	from bp2
	where RN = 1
GO


/* 10.) --------------------------------------------------------------
		 Labs
*/

DROP TABLE IF EXISTS Dflt.dim_labs;
with BNP as (
	SELECT LabChemTestSID, 'BNP' as Phenotype, labChemTestName
		  FROM	[CDWWork].[Dim].[LabChemTest] 
		  WHERE 
		   ((labchemtestname like '%BNP%' or  
			labchemtestname  like '%brain natriuretic peptide%' or 
			labchemtestname  like '%natriu%pept%') and
			LabChemTestName not like '%ratio%' AND
			LabChemTestName not like '%pro%' AND
			LabChemTestName not like '%TELOPEPTIDE%') OR
			LabChemTestName = 'CAPPROF-BNP5'
		),
	potassium as (
	  SELECT LabChemTestSID, 'potassium' as Phenotype, labChemTestName
		  FROM	[CDWWork].[Dim].[LabChemTest] 
		  WHERE ([LabChemTestname] like '%[^a-z]K[^a-z]%'  OR 
				[LabChemTestname] like '%Potas%') AND 
				[LabChemTestname] not like '%micro%' AND
				[LabChemTestname] not like '%bacteri%' AND
				[LabChemTestname] not like '%C%TRACH%' AND 
				[LabChemTestname] not like '%Coli%' AND
				[LabChemTestname] not like '%Latex%' AND
				[LabChemTestname] not like '%N.%Men%' AND
				[LabChemTestname] not like '%C%K%M%B%' AND
				[LabChemTestname] not like '%C%P%K%' AND
				[LabChemTestname] not like '%Virus%' AND
				[LabChemTestname] not like '%HCV%' AND 
				[LabChemTestname] not like '%HGB%' AND
				[LabChemTestname] not like '%Immuno%' AND
				[LabChemTestname] not like '%Vitamin%' AND 
				[LabChemTestname] not like '%X%' AND
				[LabChemTestname] not like '%B-cell%' AND
				[LabChemTestname] not like '%calcium%' AND
				[LabChemTestname] not like '%chem%ligand%' AND
				[LabChemTestname] not like '%free%K+L%' AND
				[LabChemTestname] not like '%free%Kappa%' AND
				[LabChemTestname] not like '%free%K/L%' AND
				[LabChemTestname] not like '%ratio%free%' AND
				[LabChemTestname] not like '%rogers%free%' AND
				[LabChemTestname] not like '%LT%Chain%Chain%' AND
				[LabChemTestname] not like '%Kappa%' AND
				[LabChemTestname] not like '%[0-9]K%' AND
				[LabChemTestname] not like '%channel%' AND
				[LabChemTestname] not like '%neuro%' AND
				[LabChemTestname] not like '%Q%' AND
				[LabChemTestname] not like '%renal%' AND
				[LabChemTestname] not like '%Ur%' AND
				[LabChemTestname] not like '%stool%' AND
				[LabChemTestname] not like '%fec%' AND
				[LabChemTestname] not like '%ratio%' AND
				[LabChemTestname] not like '%volt%' AND
				[LabChemTestname] not like '%24H%' AND
				[LabChemTestname] not like '%24 H%' AND
				[LabChemTestname] not like '%IGE%' AND
				[LabChemTestname] not like '%k-cup%' AND
				[LabChemTestname] not like '%K1000 POC%' AND
				[LabChemTestname] not like '%K/L%' AND
				[LabChemTestname] not like '%CLOT%' AND
				[LabChemTestname] not like '%K75%' AND
				[LabChemTestname] not like '%K76%' AND
				[LabChemTestname] not like '%K77%' AND 
				[LabChemTestname] not like '%PNEUMONIAE%' AND
				[LabChemTestname] not like '%ANION GAP W/O%' AND
				[LabChemTestname] not like '%DIALYS%' AND
				[LabChemTestname] not like '%FLUID%' AND
				[LabChemTestname] not like '%K167%' AND 
				[LabChemTestname] not like '%K-1000%' AND
				[LabChemTestname] not like '%CK-7%' AND
				[LabChemTestname] not like '%ACT%' AND
				[LabChemTestname] not like '%TEG%' AND
				[LabChemTestname] not like '%CANNABI%' AND
				[LabChemTestname] not like '%TRACHOMATIS%' AND
				[LabChemTestname] not like '%U24%' AND
				[LabChemTestname] not like '%VIT.%' AND
				[LabChemTestname] not like '%STEROIDS%' AND
				[LabChemTestname] not like '%BG ANION GAP(K)%' AND
				[LabChemTestname] not like '%CODON 117 K117N%' AND
				[LabChemTestname] not like '%escherichia%' AND
				[LabChemTestname] not like '%INTERP%' AND
				[LabChemTestname] not like '%W515L/K%' AND
				[LabChemTestname] not like '%INTERP%' AND
				[LabChemTestname] not like '%INTERP%' AND
				[LabChemTestname] not like '%INTERP%' AND
				[LabChemTestname] not like '%INTERP%' AND
				[LabChemTestname] <> 'ZZSodium/Potassium (OLD 10/00)'),
	proBNP0 as (
	SELECT LabChemTestSID, LabChemTestname
		  FROM	[CDWWork].[Dim].[LabChemTest] 
		  WHERE 
		   (labchemtestname like '%BNP%' or 
			labchemtestname like '%terminal%pep%' or
			   labchemtestname  like '%proBNP%' or 
			   labchemtestname  like '%pro[- /_]BNP%' or 
			   labchemtestname  like '%pro brain natriuretic peptide%' or 
			   labchemtestname  like '%natriu%pept%') and
			   labChemTestName not like '%ratio%' AND 
			   LabChemTestName not like '%TELOPEPTIDE%'
	),
	proBNP as (
	SELECT a.LabChemTestSID, 'NT-proBNP' as Phenotype, a.labChemTestName
		FROM proBNP0 as a LEFT JOIN BNP as b on a.LabChemTestSID = b.LabChemTestSID
			WHERE b.LabChemTestSID IS NULL
	)
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
INTO Dflt.dim_labs
  FROM [CDWWork].[ORDCovid].[DimLabs]
  where  Phenotype = 'Creatinine'
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
  FROM [CDWWork].[ORDCovid].[DimLabs]
  where  Phenotype = 'eGFR'
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
	FROM BNP
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
	FROM proBNP
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
	FROM potassium
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
  FROM CDWWork.ORDCovid.DimLabs
  where  Phenotype = 'Albumin'
UNION
SELECT DISTINCT LabChemTestSID, Phenotype, labChemTestName
  FROM CDWWork.ORDCovid.DimLabs
  where  Phenotype = 'Hemoglobin'


drop table if exists #lab0;
SELECT a.ScrSSN, y.Phenotype, y.LabChemTestName, y.LabChemTestSID, b.LabChemSpecimenDateTime, b.LabChemCompleteDateTime,
		b.LabChemResultValue, b.LabChemResultNumericValue, b.Units, b.Abnormal, b.RefHigh, b.RefLow
INTO #lab0
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.DR_Exposure) as a inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
															 inner join Src.Chem_LabChem as b on x.PatientSID =b.PatientSID
															 inner join Dflt.dim_labs as y on b.LabChemTestSID = y.LabChemTestSID
	WHERE LabChemSpecimenDateTime IS NOT NULL
	  AND LabChemResultValue IS NOT NULL
	  AND LabChemResultValue not in ('canc', 'comment')
	  AND Units not like ('%g/24%')
	  AND Units not like ('%gm/24%')
	  AND Units not like ('%gms/24%')
	  AND LabChemSpecimenDateTime <= CAST('2020-09-30' as date)


DROP TABLE IF EXISTS Dflt.DR_labs;
with crea0 as (
	SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue,
		   CAST(LabChemResultNumericValue as decimal(8, 2)) as LabChemResultNumericValue,
		   Units, Abnormal, RefHigh, RefLow
	FROM #lab0
		WHERE Phenotype = 'Creatinine'
		  AND LabChemResultNumericValue is not null
		  AND LabChemTestName not like '%URINE%'
		  AND LabChemResultNumericValue <= 7
		  AND RefHigh is not null
		  AND (Phenotype = 'Creatinine' AND Units not in ('GM/VOL', 'GM/VOLUME', 'mL/mn/1.73', 'ML/Min', 'U/L', 'ng/mL', 'mg/day', 'mmol/L', 'g/L'))
	),
	BNP0 as (
		SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue,
		CASE WHEN LabChemResultNumericValue IS NOT NULL AND Units = 'pg/dL' THEN CAST(LabChemResultNumericValue/100 as decimal(8, 2))
			 WHEN LabChemResultNumericValue IS NOT NULL AND Units in ('pGm/mL', 'ng/mL', 'pg/ml') THEN CAST(LabChemResultNumericValue as decimal(8, 2))
			 WHEN LabChemResultNumericValue IS NULL AND LabChemResultValue = 'see comment' THEN NULL
			 ELSE CAST(TRIM(' <' FROM TRIM('>' from TRIM('<' from LabChemResultValue))) as decimal(8, 2)) END AS LabChemResultNumericValue,
		Units, Abnormal, RefHigh, RefLow
	FROM #lab0
		WHERE Phenotype = 'BNP'
		  AND LabChemResultValue not in ('NONREPORTABLE', 'NOT REPORTED')
	),
	proBNP0 as (
		SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue,
		CASE WHEN LabChemResultNumericValue IS NOT NULL THEN CAST(LabChemResultNumericValue as decimal(14, 2))
			ELSE CAST(TRIM(' ' FROM TRIM('H' FROM TRIM('>' from TRIM('<' from LabChemResultValue)))) as decimal(14, 2)) END AS LabChemResultNumericValue,	
		Units, Abnormal, RefHigh, RefLow
		FROM #lab0
			WHERE Phenotype = 'NT-proBNP'
			  AND LabChemResultValue not in ('')
			  AND Units not in ('ug/L', 'mcg/L')
			  ),
   potassium0 as (
		SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue, LabChemResultNumericValue,
		--CASE WHEN LabChemResultNumericValue IS NOT NULL THEN CAST(LabChemResultNumericValue as decimal(14, 2))
		--	ELSE CAST(TRIM(' ' FROM TRIM('H' FROM TRIM('>' from TRIM('<' from LabChemResultValue)))) as decimal(14, 2)) END AS LabChemResultNumericValue,	
		Units, Abnormal, RefHigh, RefLow
		FROM #lab0
			WHERE Phenotype = 'potassium'
			 AND reflow not in ('""Not Estab.""', '"NOT ESTAB."', '"Not established"', '"SEE INTERPRETATION"', '"Variable"',
			 '12', '19', '25', '25.0', '26.0', '27', '30', '35', 'NO RANGES EXIST', 'NOT ESTAB.', 'Variable')
			 AND Units not like ('%/24h')
			 AND (RefLow IS NOT NULL OR LabChemResultNumericValue <= 8.5)
			  ),
	albumin0 as (
	SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue, LabChemResultNumericValue,
		Units, Abnormal, RefHigh, RefLow
		FROM #lab0
			WHERE Phenotype = 'Albumin'     /* exclude urine/CSF albumin */
			  AND units in ('g/dL', 'mg/dL', 'gm/dl', 'GMS/DL', 'g/L', 'gr/dL')
			  AND RefHigh is not null
			  AND RefHigh not in ('5000', '50', '5410')
	),
	hb0 as (
	SELECT ScrSSN, Phenotype, LabChemTestName, LabChemTestSID, LabChemSpecimenDateTime, LabChemCompleteDateTime, LabChemResultValue, LabChemResultNumericValue,
		Units, Abnormal, RefHigh, RefLow
		FROM #lab0
			WHERE Phenotype = 'Hemoglobin'
			  AND  LabChemTestName not like '%SERUM%'    /* exclude free hemoglobin in serum for dx of hemolysis */
			  AND LabChemTestName not like '%PLASMA%'
	)
SELECT * 
INTO Dflt.DR_labs 
FROM crea0
UNION
SELECT * FROM potassium0
UNION
SELECT * FROM BNP0
UNION
SELECT * FROM proBNP0
UNION
select * from albumin0
UNION
SELECT * from hb0
GO


-- baseline labs
DROP TABLE IF EXISTS #merge_labs;
with crea0 as (
	SELECT a.ScrSSN, a.startdate, 
		  b.LabChemResultNumericValue as creatinine_BL, 
		   b.LabChemCompleteDateTime as creatinine_BLDT,
			ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
		FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
		WHERE Phenotype = 'Creatinine'
		  AND LabChemTestName not like '%URINE%'
		  AND LabChemCompleteDateTime <= startdate
		  -- AND LabChemResultNumericValue <= 7
	),
	crea1 as (
	SELECT * FROM crea0 where rn = 1
	),
	BNP0 as (
	SELECT a.ScrSSN, a.startdate, 
		b.LabChemResultNumericValue as BNP_BL,	
		b.LabChemCompleteDateTime as BNP_BLDT,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
			FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
			WHERE Phenotype = 'BNP'
			  AND LabChemCompleteDateTime <= startdate
	),
	BNP1 as (
	SELECT * FROM BNP0 where rn = 1
	),
	proBNP0 as (
	SELECT a.ScrSSN, a.startdate, 
		b.LabChemResultNumericValue AS proBNP_BL,	
		b.LabChemCompleteDateTime as proBNP_BLDT,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
			FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
			WHERE Phenotype = 'NT-proBNP'
			  AND LabChemCompleteDateTime <= startdate
	),
	proBNP1 as (
		SELECT * from proBNP0 where rn = 1
	),
	potassium0 as (
		SELECT a.ScrSSN, a.startdate, 
		b.LabChemResultNumericValue AS potassium_BL,	
		b.LabChemCompleteDateTime as potassium_BLDT,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
			FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
			WHERE Phenotype = 'potassium'
			  AND LabChemCompleteDateTime <= startdate),
	potassium1 as (
	SELECT * FROM potassium0 where rn = 1),
	albumin0 as (
		SELECT a.ScrSSN, a.startdate, 
		b.LabChemResultNumericValue AS albumin_BL,	
		b.LabChemCompleteDateTime as albumin_BLDT,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
			FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
			WHERE Phenotype = 'Albumin'
			  AND LabChemCompleteDateTime <= startdate),
	albumin1 as (
	SELECT * FROM albumin0 where rn = 1),
	hb0 as (
		SELECT a.ScrSSN, a.startdate, 
		b.LabChemResultNumericValue AS hb_BL,	
		b.LabChemCompleteDateTime as hb_BLDT,
		ROW_NUMBER() OVER (PARTITION BY a.ScrSSN, startdate ORDER BY a.ScrSSN, startdate, LabChemCompleteDateTime desc) as rn
			FROM Dflt.DR_Exposure as a inner join Dflt.DR_labs as b on a.ScrSSN = b.ScrSSN
			WHERE Phenotype = 'Hemoglobin'
			  AND LabChemCompleteDateTime <= startdate),
	hb1 as (
	SELECT * FROM hb0 where rn = 1)
SELECT a.ScrSSN, a.startdate,
		b.creatinine_BL, b.creatinine_BLDT,
		e.potassium_BL, e.potassium_BLDT,
		c.BNP_BL, c.BNP_BLDT,
		d.proBNP_BL, d.proBNP_BLDT,
		f.albumin_BL, f.albumin_BLDT,
		g.hb_BL, g.hb_BLDT
INTO #merge_labs
	FROM Dflt.DR_Exposure as a left join crea1 as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate
							   left join BNP1 as c on a.ScrSSN = c.ScrSSN and a.startdate = c.startdate
							   left join proBNP1 as d on a.ScrSSN = d.ScrSSN and a.startdate = d.startdate
							   left join potassium1 as e on a.ScrSSN = e.ScrSSN and a.startdate = e.startdate
							   left join albumin1 as f on a.ScrSSN = f.ScrSSN and a.startdate = f.startdate
							   left join hb1 as g on a.ScrSSN = g.ScrSSN and a.startdate = g.startdate


/* 11.) --------------------------------------------------------------
		 Baseline Comorbidity
*/


DROP TABLE IF EXISTS Dflt.dim_DR_ICD910;
SELECT *
INTO Dflt.dim_DR_ICD910
FROM Dflt.dim_ICD910
WHERE condition in ('Any_malignancy', 'CMB_AFibFlutter', 'CMB_ARD', 'CMB_Cardiomyopathy', 'CMB_CKD', 'CMB_COPD', 'CMB_DM', 'CMB_DVT_PE',
					'CMB_DysLipid', 'CMB_HTN', 'CMB_MDD', 'CMB_OSA', 'CMB_PAD', 'CMB_SRD', 'CMB_VHD', 'MI', 'Flu', 'Fracture', 
					'HemoStroke', 'IschemicStroke', 'SAH', 'TIA')
UNION
SELECT comnum, 
		CASE WHEN comnum = 9901 THEN 'LVAD'
			 WHEN comnum = 9907 THEN 'Long_QT'
			 WHEN comnum = 9910 THEN 'Brugada'
			 ELSE 'OTHER' END AS condition,
		ICDCodeNoDecimal, ICDCode, ICDType
FROM Dflt.ICD_COMOR_MASTER_910
    where comnum in (9901, 9907, 9910)
UNION
-- heart transplant
SELECT distinct 219 as comnum, 'HeartTx' as condition,
	REPLACE(ICD9code, '.', '') as ICDCodeNoDecimal,
	ICD9code as ICDCode, 
	9 as ICDType
FROM CDWWork.dim.ICD9
where ICD9Code like 'V42.1%'
UNION
SELECT distinct 219 as comnum, 'HeartTx' as condition,
	REPLACE(ICD10code, '.', '') as ICDCodeNoDecimal,
	ICD10code as ICDCode, 
	10 as ICDType
FROM CDWWork.dim.ICD10
where ICD10Code like 'Z94.1%'
-- Anemia
UNION
SELECT distinct 301 as comnum, 'Anemia' as condition,
	REPLACE(ICD9code, '.', '') as ICDCodeNoDecimal,
	ICD9code as ICDCode, 
	9 as ICDType
FROM CDWWork.dim.ICD9
where ICD9Code like '28[0-5]%'
UNION
SELECT distinct 301 as comnum, 'Anemia' as condition,
	REPLACE(ICD10code, '.', '') as ICDCodeNoDecimal,
	ICD10code as ICDCode, 
	10 as ICDType
FROM CDWWork.dim.ICD10
where ICD10Code like 'D5%' or ICD10Code like 'D6[0-4]%'
-- Dementia
UNION
SELECT distinct 302 as comnum, 'Dementia' as condition,
	REPLACE(ICD9code, '.', '') as ICDCodeNoDecimal,
	ICD9code as ICDCode, 
	9 as ICDType
FROM CDWWork.dim.ICD9
where ICD9Code like '290%' or ICD9Code like '331.[0-1]%' or ICD9Code like '331.8[2-3]%'
UNION
SELECT distinct 302 as comnum, 'Dementia' as condition,
	REPLACE(ICD10code, '.', '') as ICDCodeNoDecimal,
	ICD10code as ICDCode, 
	10 as ICDType
FROM CDWWork.dim.ICD10
where ICD10Code like 'F0[0-3]%' or ICD10Code like 'G30%' or ICD10Code like 'G31.0%' or ICD10Code like 'G31.8[3-4]%'
-- (20113 rows affected)


DROP TABLE IF EXISTS #Dim_ICD9;
SELECT DISTINCT ICD9SID, ICD9Code, condition
INTO #Dim_ICD9
	FROM CDWWork.Dim.ICD9 as x inner join Dflt.dim_DR_ICD910 as a on x.ICD9Code = a.ICDCode
WHERE ICDtype = 9 
GO

DROP TABLE IF EXISTS #Dim_ICD10;
SELECT DISTINCT ICD10SID, ICD10Code, condition
INTO #Dim_ICD10
	FROM CDWWork.Dim.ICD10 as x inner join Dflt.dim_DR_ICD910 as a on x.ICD10Code = a.ICDCode
WHERE ICDtype = 10
GO

DROP TABLE IF EXISTS #Comorbidites_long;
WITH in1 AS
	(SELECT a.scrssn, a.startdate, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD9 on id.ICD9SID = #Dim_ICD9.ICD9SID 
											 inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join Dflt.DR_Exposure as a on a.scrssn = x.scrssn
	   WHERE  dischargeDateTime <= startdate 
	UNION
	 SELECT a.scrssn, a.startdate, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD10 on id.ICD10SID = #Dim_ICD10.ICD10SID
											 inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join Dflt.DR_Exposure as a on a.scrssn = x.scrssn
		 where  dischargeDateTime <= startdate 
		 ), 
  firstin AS
	 (SELECT scrssn, startdate, condition, MIN(dischargedatetime) as recDate
	  FROM in1
	  GROUP BY ScrSSN, startdate, condition
	  ), 
  ot1 AS
	 (SELECT a.scrssn, a.startdate, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD9 on id.ICD9SID = #Dim_ICD9.ICD9SID
									  inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join Dflt.DR_Exposure as a on a.scrssn = x.scrssn
	   WHERE  VisitDateTime <= startdate  
	UNION
	  SELECT a.scrssn, startdate, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD10 on id.ICD10SID = #Dim_ICD10.ICD10SID
									  inner join Dflt.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join Dflt.DR_Exposure as a on a.scrssn = x.scrssn
	  WHERE  VisitDateTime <= startdate 
	 ), 
  ot2 AS
	 (SELECT scrssn, startdate, condition, vdiagnosisdatetime 
	 ,ROW_NUMBER() OVER (PARTITION BY scrssn, startdate, condition ORDER BY scrssn, startdate, condition, VDIAGNOSISDATETIME) AS RN
	 FROM ot1
	 ), 
  secot AS
	 (SELECT scrssn, startdate, condition, vdiagnosisdatetime as recdate
	 FROM ot2
	 where RN = 2
	 ), 
  comb AS
	 (SELECT scrssn, startdate, condition, recdate
	   FROM firstin
	UNION
	   SELECT scrssn, startdate, condition, recdate
	   FROM secot
	 ), 
  comb2 AS
	 (SELECT scrssn, startdate, condition, recdate
	  ,ROW_NUMBER() OVER (PARTITION BY scrssn, startdate, condition ORDER BY scrssn, startdate, condition, recdate) AS RN
	  FROM comb
	  ) 
SELECT scrssn, startdate, condition, recdate
	INTO #Comorbidites_long
		FROM comb2
		WHERE rn = 1
GO

DROP TABLE IF EXISTS #long1;
SELECT  *,
		concat(condition,'_DT') as condition_dt
INTO #long1
  FROM #Comorbidites_long

DROP TABLE IF EXISTS #Comorbidities_dates;
SELECT b.*
INTO #Comorbidities_dates
FROM #merge_labs as a left join (SELECT scrssn, startdate, recdate, condition_dt FROM #long1) as t
PIVOT
(
MAX(recdate)
FOR condition_dt in 
(CMB_AFibFlutter_DT, 
 CMB_Cardiomyopathy_DT, 
 CMB_CKD_DT, 
 CMB_COPD_DT, 
 CMB_DM_DT, 
 CMB_DysLipid_DT, 
 CMB_HTN_DT, 
 CMB_MDD_DT,
 CMB_OSA_DT,
 CMB_PAD_DT, 
 CMB_VHD_DT,
 MI_DT,
 LVAD_DT,
 HeartTx_DT,
 Long_QT_DT,
 Brugada_DT,
 Anemia_DT,
 Any_malignancy_DT,
 CMB_ARD_DT,
 CMB_DVT_PE_DT,
 CMB_SRD_DT,
 Dementia_DT,
 Flu_DT,
 Fracture_DT,
 HemoStroke_DT,
 IschemicStroke_DT,
 SAH_DT,
 TIA_DT
 )) as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate
GO


/* 12.) --------------------------------------------------------------
		 RX
*/

-- dim table 
with digoxin0 as (
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%DIGOXIN%' THEN 'RX_Digoxin'END AS RX_Cat, 
		DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	), 
	digoxin1 as (
SELECT DISTINCT a.NationalDrugSID, a.RX_Cat, b.DrugNameWithoutDose, a.DrugNameWithDose, c.DosageForm
FROM digoxin0 AS a LEFT JOIN CDWWork.Dim.DrugNameWithoutDose as b on a.DrugNameWithoutDoseSID = b.DrugNameWithoutDoseSID
				   LEFT JOIN CDWWork.Dim.DosageForm as c on a.DosageFormSID = c.DosageFormSID
		WHERE RX_Cat IS NOT NULL
		  AND DosageForm not in ('INJ,SOLN')
		  )
SELECT * into #dim_digoxin from digoxin1 
GO


-- merge drug dim table
DROP TABLE IF EXISTS #dim_rx;
SELECT * 
INTO #dim_rx
  FROM Dflt.dim_rx
  WHERE RX_cat in ('RX_betaBlocker', 'RX_Diuretic_potassium', 'RX_Diuretic_TZD', 'RX_Diuretics_loop', 'RX_GLP1', 'RX_lipid_statin', 'RX_SGLT2')
	AND DrugNameWithoutDose not like '%AMILORIDE%'
	AND DrugNameWithoutDose not like '%TRIAMTERENE%'   -- Spironolactone and eplerenone preferred by guidelines
UNION
SELECT * FROM #dim_digoxin


DROP TABLE IF EXISTS Dflt.DR_RX;
select a.ScrSSN, y.RX_Cat,
		y.DrugNameWithoutDose, y.DrugNameWithDose, z.StrengthNumeric, b.QtyNumeric, b.DaysSupply, b.ReleaseDateTime,
		cast(b.ReleaseDateTime as date) as startdate,
		cast(DATEADD(day, DaysSupply, ReleaseDateTime) as date) as enddate
into Dflt.DR_RX
	FROM (SELECT DISTINCT ScrSSN from dflt.DR_Exposure) as a inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
															 inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
															 inner join #dim_rx as y on y.NationalDrugSID = b.NationalDrugSID
															 inner join CDWWork.Dim.NationalDrug as z on y.NationalDrugSID = z.NationalDrugSID
		WHERE ReleaseDateTime IS NOT NULL
		  AND ReleaseDateTime <= CAST('2020-09-30' AS DATE)
		  AND DaysSupply IS NOT NULL
		  AND b.ReleaseDateTime >= CAST('2013-01-01' as date)	
GO


-- baseline RX
DROP TABLE IF EXISTS #RX_BL;
SELECT a.ScrSSN, a.StartDate,
		b.Rx_cat, b.startdate as rxstartdate, b.enddate as rxenddate
INTO #RX_BL
	FROM dflt.DR_Exposure as a left join Dflt.DR_RX as b on a.ScrSSN = b.ScrSSN
WHERE a.StartDate > b.startdate 
  AND a.StartDate <= b.enddate


DROP TABLE IF EXISTS #RX_dates;
with bl0 as (
SELECT  *,
		concat(RX_Cat,'_DT') as RX_Cat_dt
  FROM #RX_BL
)
SELECT DISTINCT b.*
INTO #RX_dates
FROM bl0 as a left join (SELECT scrssn, startdate, rxstartdate, RX_Cat_dt FROM bl0) as t
PIVOT
(
MAX(rxstartdate)
FOR RX_Cat_dt in 
(RX_betaBlocker_dt,
RX_Digoxin_dt,
RX_Diuretic_potassium_dt,
RX_Diuretic_TZD_dt,
RX_Diuretics_loop_dt,
RX_GLP1_dt,
RX_lipid_statin_dt,
RX_SGLT2_dt)) as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate
GO

/* 13.) --------------------------------------------------------------
		 Any and HF hospitalization
*/

-- HF hospitalization

SELECT DISTINCT ICD9SID, ICD9Code, condition
INTO #Dim_ICD9_HF
	FROM CDWWork.Dim.ICD9 as x inner join Dflt.dim_ICD910 as a on x.ICD9Code = a.ICDCode
WHERE ICDtype = 9 
  AND condition = 'CMB_HF'
GO

SELECT DISTINCT ICD10SID, ICD10Code, condition
INTO #Dim_ICD10_HF
	FROM CDWWork.Dim.ICD10 as x inner join Dflt.dim_ICD910 as a on x.ICD10Code = a.ICDCode
WHERE ICDtype = 10
  AND condition = 'CMB_HF'
GO


/*** all hospitalization ***/
DROP TABLE IF EXISTS Dflt.DR_AllHosp;
select a.scrssn, b.InpatientSID,
		b.AdmitDateTime, b.dischargedatetime
		-- c.ICD9SID, y.ICD9Code as ICDCode, c.OrdinalNumber
into Dflt.DR_AllHosp
 from (SELECT ScrSSN from Dflt.DR_Exposure) as a 
							inner join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
							inner join Src.Inpat_Inpatient as b on x.PatientSID = b.PatientSID
    where b.AdmitDateTime IS NOT NULL


/*** HF hospitalization ***/
DROP TABLE IF EXISTS Dflt.DR_HFH;
with a0 as(
	SELECT a.*,
		   y.ICD9Code as ICDCode, c.OrdinalNumber
		FROM Dflt.DR_AllHosp as a inner join Src.Inpat_InpatDischargeDiagnosis as c on a.InpatientSID = c.InpatientSID
								  inner join #Dim_ICD9_HF as y on c.ICD9SID = y.ICD9SID
	UNION
	select a.*,
			y.ICD10Code as ICDCode, c.OrdinalNumber
	 from Dflt.DR_AllHosp as a inner join Src.Inpat_InpatDischargeDiagnosis as c on a.InpatientSID = c.InpatientSID
							   inner join #Dim_ICD10_HF as y on c.ICD10SID = y.ICD10SID
	),
	hf0 as (
	SELECT *
		FROM a0
		WHERE OrdinalNumber in (1, 2, 3)
  )
SELECT DISTINCT ScrSSN, 
				AdmitDateTime as HFH_AdmitDate,
				DischargeDateTime as HFH_DischargeDate
INTO Dflt.DR_HFH
FROM hf0


/*** number of baseline HFH in the past 12 months ***/
with a0 as (
	SELECT a.ScrSSN, a.startdate,
			b.HFH_AdmitDate, b.HFH_DischargeDate
		FROM Dflt.DR_Exposure as a inner join Dflt.DR_HFH as b on a.ScrSSN = b.ScrSSN
			WHERE HFH_DischargeDate >= DATEADD(MONTH, -12, a.startdate)
			  AND HFH_DischargeDate < a.startdate)
SELECT ScrSSN, startdate, count(*) as HFH_BL_num
INTO #HFH_count
	FROM a0
	GROUP BY ScrSSN, startdate


/* 14.) --------------------------------------------------------------
		BL flu vaccine
*/

DROP TABLE IF EXISTS #FluShot;
with a0 as (
	SELECT a.ScrSSN, CAST(b.VisitDateTime as date) as FluVaccinationDate, c.CVXCode, c.ImmunizationName
		FROM (SELECT DISTINCT ScrSSN FROM Dflt.DR_Exposure) as a LEFT JOIN Dflt.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
																 LEFT JOIN CDWWork.Immun.Immunization as b on x.PatientSID = b.PatientSID
																 LEFT JOIN CDWWork.Dim.ImmunizationName as c on b.ImmunizationNameSID = c.ImmunizationNameSID
	WHERE b.VisitDateTime >= CAST('2013-01-01' as date)
	  AND b.VisitDateTime <=  CAST('2020-09-30' as date)
	  AND CVXCode in ('15', '16', '88', '111', '125', '126', '127', '128', 
					  '135', '140', '141', '144', '149', '150', '151', '153', '155', '158',
					  '160', '161', '166', '168', '171', '185', '186', '194', '197',
					  '200', '201', '202', '205', '231', '320')),     /* check for haemophilus influenzae type b vaccine */
	flu0 as (
	SELECT DISTINCT a.ScrSSN, a.startdate, b.FluVaccinationDate, b.ImmunizationName
		FROM Dflt.DR_Exposure as a inner join a0 as b on a.ScrSSN = b.ScrSSN
			WHERE FluVaccinationDate >= DATEADD(MONTH, -12, startdate)
			  AND FluVaccinationDate < startdate)
SELECT ScrSSN, startdate, max(FluVaccinationDate) as FluVaccinationDate 
INTO #FluShot
	FROM flu0
	GROUP BY ScrSSN, startdate
GO


/* 15.) --------------------------------------------------------------
		ADI
*/

DROP TABLE IF EXISTS #ADI1;
SELECT PATIENTICN, Year, quarter, FIPS_GEOID, ADI_NATRANK, ADI_STATERNK
INTO #ADI_ALL
	FROM Src.ADI_Pat_Pat_PSSG
	where Year >= 2015
	  AND Year < 2021
	  AND quarter in ('Q1', 'Q2', 'Q3', 'Q4')
GO


DROP TABLE IF EXISTS #ADI1;
with a0 as (
		SELECT ScrSSN, startdate, 
				YEAR(dateadd(mm,3,startdate)) as BLYear,
				CASE WHEN MONTH(startdate) in (10, 11, 12) THEN 'Q1'
					 WHEN MONTH(startdate) in (1, 2, 3) THEN 'Q2'
					 WHEN MONTH(startdate) in (4, 5, 6) THEN 'Q3'
					 WHEN MONTH(startdate) in (7, 8, 9) THEN 'Q4'
				ELSE 'ERROR' END AS quarter
		FROM Dflt.DR_Exposure)
SELECT a.*, FIPS_GEOID, ADI_NATRANK, ADI_STATERNK
INTO #ADI1
from a0 as a left join Dflt.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
			 left join #ADI_ALL as b on x.PatientICN = b.PATIENTICN and a.BLYear = b.year and a.quarter = b.quarter


DROP TABLE IF EXISTS #ADI2;
SELECT DISTINCT * INTO #ADI2 from #ADI1

DROP TABLE IF EXISTS #ADI3;
SELECT ScrSSN, startdate, min(ADI_NATRANK) as ADI_NATRANK, min(ADI_STATERNK) as ADI_STATERNK
INTO #ADI3
FROM #ADI2
GROUP BY ScrSSN, startdate


/* 16.) --------------------------------------------------------------
		All LVEF for time-varying
*/

DROP TABLE IF EXISTS Dflt.DR_EF;
with ef0 as (
select distinct a.ScrSSN, CAST(ef.ValueDateTime as DATE) as ValueDateTime,
	CASE WHEN ef.High_Value IS NULL THEN Low_Value
	ELSE ef.High_Value
	END AS LVEF,
	ef.Low_Value, ef.High_Value, ef.valuestring
		FROM Src.VINCI_TIU_NLP_LVEF as ef inner join Dflt.CohortCrossWalk as x on ef.PatientSID = x.PatientSID
										  inner join (SELECT DISTINCT ScrSSN from Dflt.DR_Exposure) as a on x.ScrSSN = a.ScrSSN
	WHERE ValueDateTime <= CAST('2020-09-30' AS DATE))
SELECT ScrSSN, ValueDateTime, AVG(LVEF) as LVEF
INTO Dflt.DR_EF
FROM ef0
GROUP BY ScrSSN, ValueDateTime


/* 17.) --------------------------------------------------------------
		Merge all baseline data
*/

SELECT a.*,
	   bp.Systolic as SBP_BL, bp.Diastolic as DBP_BL, CAST(bp.VitalSignTakenDateTime as date) as BP_BLDT,
	   lab.creatinine_BL, CAST(lab.creatinine_BLDT as date) as creatinine_BLDT,
	   lab.potassium_BL, CAST(lab.potassium_BLDT as date) as potassium_BLDT,
	   lab.BNP_BL, CAST(lab.BNP_BLDT as date) as BNP_BLDT,
	   lab.proBNP_BL, CAST(lab.proBNP_BLDT as date) as proBNP_BLDT,
	   lab.albumin_BL, CAST(lab.albumin_BLDT as date) as albumin_BLDT, 
	   lab.hb_BL, CAST(lab.hb_BLDT as date) as hb_BLDT,
	   CAST(CMB_AFibFlutter_DT as date) as CMB_AFibFlutter_DT, CAST(CMB_Cardiomyopathy_DT as date) as CMB_Cardiomyopathy_DT,
	   CAST(CMB_CKD_DT as date) as CMB_CKD_DT, CAST(CMB_COPD_DT as date) as CMB_COPD_DT,
	   CAST(CMB_DM_DT as date) as CMB_DM_DT, CAST(CMB_DysLipid_DT as date) as CMB_DysLipid_DT,
	   CAST(CMB_HTN_DT as date) as CMB_HTN_DT, CAST(CMB_MDD_DT as date) as CMB_MDD_DT,
	   CAST(CMB_OSA_DT as date) as CMB_OSA_DT, CAST(CMB_PAD_DT as date) as CMB_PAD_DT,
	   CAST(CMB_VHD_DT as date) as CMB_VHD_DT, CAST(MI_DT as date) as CMB_MI_DT,
	   CAST(LVAD_DT as date) as CMB_LVAD_DT, CAST(HeartTx_DT as date) as CMB_HeartTx_DT,
	   CAST(Long_QT_DT as date) as CMB_Long_QT_DT, CAST(Brugada_DT as date) as CMB_Brugada_DT,
	   CAST(Anemia_DT as date) as Anemia_DT, CAST(Any_malignancy_DT as date) as Any_malignancy_DT,
	   CAST(CMB_ARD_DT as date) as CMB_ARD_DT, CAST(CMB_DVT_PE_DT as date) as CMB_DVT_PE_DT, 
	   CAST(CMB_SRD_DT as date) as CMB_SRD_DT, CAST(Dementia_DT as date) as Dementia_DT, 
	   CAST(Flu_DT as date) as Flu_DT, CAST(Fracture_DT as date) as Fracture_DT, 
	   CAST(HemoStroke_DT as date) as HemoStroke_DT, CAST(IschemicStroke_DT as date) as IschemicStroke_DT, 
	   CAST(SAH_DT as date) as SAH_DT, CAST(TIA_DT as date) as TIA_DT,
	   RX_betaBlocker_dt, RX_Digoxin_dt, RX_Diuretic_potassium_dt as RX_MRA_dt, RX_Diuretic_TZD_dt, RX_Diuretics_loop_dt,
	   RX_GLP1_dt, RX_lipid_statin_dt, RX_SGLT2_dt,
	   HFH_BL_num,
	   FluVaccinationDate,
	   ADI_NATRANK, ADI_STATERNK
INTO Dflt.DR_BLData
	FROM Dflt.DR_Exposure as a LEFT JOIN #BP_BL as bp on a.ScrSSN = bp.ScrSSN AND a.startdate = bp.startdate
							   LEFT JOIN #merge_labs as lab on a.ScrSSN = lab.ScrSSN AND a.startdate = lab.startdate
							   LEFT JOIN #Comorbidities_dates as dx on a.ScrSSN = dx.ScrSSN AND a.startdate = dx.startdate
							   LEFT JOIN #RX_dates as rx on a.ScrSSN = rx.ScrSSN AND a.startdate = rx.startdate
							   LEFT JOIN #HFH_count as hfh on a.ScrSSN = hfh.ScrSSN and a.startdate = hfh.startdate
							   LEFT JOIN #FluShot as flu on a.ScrSSN = flu.ScrSSN and a.startdate = flu.startdate
							   LEFT JOIN #ADI3 as adi on a.ScrSSN = adi.ScrSSN and a.startdate = adi.startdate



/*

/* 18.) --------------------------------------------------------------
		wide to long format monthly
*/

/*** ITT follow-up, can inner joint for pp after censoring ***/
drop table if exists #time;
WITH a0 as(
SELECT ScrSSN, Arm, startdate, enddate, ROW_NUMBER() over(ORDER BY ScrSSN, startdate) as id,
		CASE WHEN DeathDate IS NOT NULL THEN DeathDate
			ELSE CAST('2020-09-30' as date) END AS Death_itt_date
	FROM Dflt.DR_BLData)
SELECT *, DATEDIFF(MONTH, startdate, Death_itt_date) + 1 as  Death_itt_time
INTO #time
from a0


/*** expand rows ***/
DROP TABLE IF EXISTS #long;
WITH a0 as (
	SELECT *, death_itt_time - 1 as time
		FROM #time),
CTE AS (
	SELECT * FROM a0
	UNION ALL
	SELECT ScrSSN, Arm, startdate, enddate, id, Death_itt_date, Death_itt_time, time - 1
		FROM CTE
		WHERE time - 1 >= 0
	)
SELECT *,  
	   DATEADD(MONTH, time, startdate) as itv_startdate
INTO #long
	from CTE
	ORDER BY ScrSSN, startdate, time
OPTION(MAXRECURSION 0)


/* 19.) --------------------------------------------------------------
		time-varying weights
*/

DROP TABLE IF EXISTS #weights_tv;
with w0 as (
	SELECT a.*, b.Weight, b.VitalSignTakenDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, VitalSignTakenDateTime desc) as rn
		FROM #long as a left join Dflt.DR_Weights as b on a.ScrSSN = b.ScrSSN
		WHERE VitalSignTakenDateTime <= itv_startdate
	)
SELECT *
INTO #weights_tv
FROM w0 
where rn = 1
order by scrssn, startdate, time, rn


/* 20.) --------------------------------------------------------------
		time-varying BPs
*/

DROP TABLE IF EXISTS #BP_tv;
with w0 as (
	SELECT a.*, b.Systolic, b.Diastolic, b.VitalSignTakenDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, VitalSignTakenDateTime desc) as rn
		FROM #long as a left join Dflt.DR_BP as b on a.ScrSSN = b.ScrSSN
		WHERE VitalSignTakenDateTime <= itv_startdate
	)
SELECT *
INTO #BP_tv
FROM w0 
where rn = 1
order by scrssn, startdate, time, rn


/* 21.) --------------------------------------------------------------
		time-varying EF%
*/

DROP TABLE IF EXISTS #LVEF_tv;
with w0 as (
	SELECT a.*, b.LVEF, b.ValueDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, ValueDateTime desc) as rn
		FROM #long as a left join Dflt.DR_EF as b on a.ScrSSN = b.ScrSSN
		WHERE ValueDateTime <= itv_startdate
	)
SELECT *
INTO #LVEF_tv
FROM w0 
where rn = 1
order by scrssn, startdate, time, rn

/* 22.) --------------------------------------------------------------
		time-varying labs (creatinine, K+)
*/

DROP TABLE IF EXISTS #Labs_tv;
with creatinine0 as (
	SELECT a.*, b.labchemresultnumericvalue as creatinine_tv, b.LabChemCompleteDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, LabChemCompleteDateTime desc) as rn
		FROM #long as a left join Dflt.DR_Labs as b on a.ScrSSN = b.ScrSSN
		WHERE LabChemCompleteDateTime <= itv_startdate
		  AND Phenotype = 'Creatinine'
		  AND LabChemTestName not like '%URINE%'
	),
	creatinine1 as (
	SELECT *
		FROM creatinine0
		WHERE rn = 1
	),
	potassium0 as (
	SELECT a.*, b.labchemresultnumericvalue as potassium_tv, b.LabChemCompleteDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, LabChemCompleteDateTime desc) as rn
		FROM #long as a left join Dflt.DR_Labs as b on a.ScrSSN = b.ScrSSN
		WHERE LabChemCompleteDateTime <= itv_startdate
		  AND Phenotype = 'potassium'
	),
	potassium1 as (
	SELECT *
		FROM potassium0
		WHERE rn = 1
	),
	bnp0 as (
	SELECT a.*, b.labchemresultnumericvalue as BNP_tv, b.LabChemCompleteDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, LabChemCompleteDateTime desc) as rn
		FROM #long as a left join Dflt.DR_Labs as b on a.ScrSSN = b.ScrSSN
		WHERE LabChemCompleteDateTime <= itv_startdate
		  AND Phenotype = 'BNP'
	),
	bnp1 as (
	SELECT *
		FROM bnp0
		WHERE rn = 1
	),
	probnp0 as (
	SELECT a.*, b.labchemresultnumericvalue as proBNP_tv, b.LabChemCompleteDateTime,
		ROW_NUMBER() over(PARTITION BY a.ScrSSN, startdate, time ORDER BY a.ScrSSN, startdate, time, LabChemCompleteDateTime desc) as rn
		FROM #long as a left join Dflt.DR_Labs as b on a.ScrSSN = b.ScrSSN
		WHERE LabChemCompleteDateTime <= itv_startdate
		  AND Phenotype = 'NT-proBNP'
	),
	probnp1 as (
	SELECT *
		FROM probnp0
		WHERE rn = 1
	)
SELECT a.ScrSSN, a.Arm, a.startdate, a.id, a.time, a.itv_startdate,
	   b.creatinine_tv, cast(b.LabChemCompleteDateTime as date) as creatinine_tvdt,
	   c.potassium_tv, cast(c.LabChemCompleteDateTime as date) as potassium_tvdt,
	   d.BNP_tv, cast(d.LabChemCompleteDateTime as date) as BNP_tvdt,
	   e.proBNP_tv, cast(e.LabChemCompleteDateTime as date) as proBNP_tvdt
INTO #Labs_tv
	FROM #long as a left join creatinine1 as b on a.id = b.id and a.time = b.time
					left join potassium1 as c on a.id = c.id and a.time = c.time
					left join bnp1 as d on a.id = d.id and a.time = d.time
					left join probnp1 as e on a.id = e.id and a.time = e.time
ORDER BY scrssn, startdate, time


/* 23.) --------------------------------------------------------------
		time-varying hospitalization in the past month, 
					 cumulative HF hospitalization
*/

DROP TABLE IF EXISTS #Hosp_tv;
WITH HFH0 as (
	SELECT a.ScrSSN, a.startdate, a.id, a.time, a.itv_startdate,
			CAST(HFH_AdmitDate as date) as HFH_AdmitDate,
			CAST(HFH_DischargeDate as date) as HFH_DischargeDate
		FROM #long as a left join Dflt.DR_HFH as b on a.ScrSSN = b.ScrSSN	
			WHERE (DATEADD(MONTH, -1, itv_startdate) <= HFH_AdmitDate AND HFH_AdmitDate < itv_startdate)
			   OR (DATEADD(MONTH, -1, itv_startdate) <= HFH_DischargeDate AND HFH_DischargeDate < itv_startdate)
			   OR (itv_startdate >= HFH_AdmitDate AND itv_startdate <= HFH_DischargeDate)
	),
	HFH1 as (
	SELECT id, time, count(*) as HFH_num_tv
		FROM HFH0
		GROUP BY id, time
	),
	Hosp0 as (
	SELECT a.ScrSSN, a.startdate, a.id, a.time, a.itv_startdate,
			CAST(AdmitDateTime as date) as AdmitDate,
			CAST(DischargeDateTime as date) as DischargeDate
		FROM #long as a left join Dflt.DR_AllHosp as b on a.ScrSSN = b.ScrSSN	
			WHERE (DATEADD(MONTH, -1, itv_startdate) <= AdmitDateTime AND AdmitDateTime < itv_startdate)
			   OR (DATEADD(MONTH, -1, itv_startdate) <= DischargeDateTime AND DischargeDateTime < itv_startdate)
			   OR (itv_startdate >= AdmitDateTime AND itv_startdate <= DischargeDateTime)
	),
	Hosp1 as (
	SELECT id, time, count(*) as Hosp_num_tv
		FROM Hosp0
		GROUP BY id, time
	),
	comb as (
	SELECT a.ScrSSN, a.Arm, a.startdate, a.id, a.time, a.itv_startdate,
		   CASE WHEN b.id IS NOT NULL THEN HFH_num_tv
				ELSE 0 END AS HFH_num_tv,
		   CASE WHEN c.id is not null THEN Hosp_num_tv
				ELSE 0 END AS Hosp_num_tv
		FROM #long as a left join HFH1 as b on a.id = b.id and a.time = b.time
						left join Hosp1 as c on a.id = c.id and a.time = c.time
	)
SELECT *,
		sum(HFH_num_tv) OVER (PARTITION BY id ORDER BY id, time) as HFH_cumsum_tv
INTO #Hosp_tv
	FROM comb

/* 24.) --------------------------------------------------------------
		time-varying Rx
*/

DROP TABLE IF EXISTS #RX_tv;
with rx0 as (
	SELECT a.id, a.time, a.itv_startdate,
			b.rx_cat, b.startdate as rx_startdate, b.enddate as rx_enddate
		FROM #long as a left join Dflt.DR_RX as b on a.ScrSSN = b.ScrSSN
			WHERE (DATEADD(MONTH, -1, itv_startdate) <= b.startdate AND  b.startdate < itv_startdate)
			   OR (DATEADD(MONTH, -1, itv_startdate) <= b.enddate AND  b.enddate < itv_startdate)
			   OR (itv_startdate >= b.startdate AND itv_startdate <= b.enddate)
	),
	rx1 as (
	SELECT distinct *,
		concat(RX_Cat,'_tvdt') as RX_Cat_tvdt
	FROM rx0
	)
SELECT DISTINCT b.*
INTO #RX_tv
FROM (SELECT id,  time, itv_startdate, rx_startdate, RX_Cat_tvdt FROM rx1) as t
PIVOT
(
MAX(rx_startdate)
	FOR RX_Cat_tvdt in 
	(RX_betaBlocker_tvdt,
	RX_Digoxin_tvdt,
	RX_Diuretic_potassium_tvdt,
	RX_Diuretic_TZD_tvdt,
	RX_Diuretics_loop_tvdt,
	RX_GLP1_tvdt,
	RX_lipid_statin_tvdt,
	RX_SGLT2_tvdt)) as b 
GO


/* 25.) --------------------------------------------------------------
		time-varying device measures
*/

/*** clean device data ***/
DROP TABLE IF EXISTS Dflt.DR_DeviceData;
with comb0 as (
	SELECT [Device Serial Number] as serial,
		   [TrendDate] as TrendDate,
		   CASE WHEN [RemoteSessionAnnotation] = 'true' THEN 1
				WHEN [RemoteSessionAnnotation] = 'false' THEN 0
				ELSE NULL END AS TransmissionFlag,
		   CASE WHEN [DayHeartRate (bpm)] = 'NULL' THEN NULL
				WHEN [DayHeartRate (bpm)] = '' THEN NULL
				ELSE CAST([DayHeartRate (bpm)] as numeric(10, 0))
			END AS DayHR,
		   CASE WHEN [NightHeartRate (bpm)] = 'NULL' THEN NULL
				WHEN [NightHeartRate (bpm)] = '' THEN NULL
				ELSE CAST([NightHeartRate (bpm)] as numeric(10, 0))
			END AS NightHR,
		   CASE WHEN [HeartRateVariability (ms)] = 'NULL' THEN NULL
				WHEN [HeartRateVariability (ms)] = '' THEN NULL
				ELSE CAST([HeartRateVariability (ms)] as numeric(10, 0))
			END AS HRV,
		   CASE WHEN [AccumulatedDiff (ohms)] = 'NULL' THEN NULL
				WHEN [AccumulatedDiff (ohms)] = '' THEN NULL
				ELSE CAST([AccumulatedDiff (ohms)] as numeric(10, 0))
			END AS FluidIndex,
		   CASE WHEN [ActivitiesOfDlyLiving (minutes)] = 'NULL' THEN NULL
				WHEN [ActivitiesOfDlyLiving (minutes)] = '' THEN NULL
				ELSE CAST([ActivitiesOfDlyLiving (minutes)] as numeric(10, 0))
			END AS PhysicalActivity_min,
		   CASE WHEN [TimeInATAF (ms)] = 'NULL' THEN NULL
				WHEN [TimeInATAF (ms)] = '' THEN NULL
				ELSE CAST([TimeInATAF (ms)] as numeric(20, 0))
			END AS TimeInATAF_ms,
		   CASE WHEN [SpontaneousVTVFPerDayCnt] = 'NULL' THEN NULL
				WHEN [SpontaneousVTVFPerDayCnt] = '' THEN NULL
				ELSE CAST([SpontaneousVTVFPerDayCnt] as numeric(10, 0))
			END AS VTVFCnt,
		   CASE WHEN [SpontaneousNSTPerDayCnt] = 'NULL' THEN NULL
				WHEN [SpontaneousNSTPerDayCnt] = '' THEN NULL
				ELSE CAST([SpontaneousNSTPerDayCnt] as numeric(10, 0))
			END AS NSTCnt,
		   CASE WHEN [APercentPaced (%)] = 'NULL' THEN NULL
				WHEN [APercentPaced (%)] = '' THEN NULL
				ELSE CAST([APercentPaced (%)] as numeric(10, 0))
			END AS APaced_pct,
		   CASE WHEN [VPercentPaced (%)] = 'NULL' THEN NULL
				WHEN [VPercentPaced (%)] = '' THEN NULL
				ELSE CAST([VPercentPaced (%)] as numeric(10, 0))
			END AS VPaced_pct,
		   CASE WHEN [EffectivCRTPercentPcngDay (%)] = 'NULL' THEN NULL
				WHEN [EffectivCRTPercentPcngDay (%)] = '' THEN NULL
				ELSE CAST([EffectivCRTPercentPcngDay (%)] as numeric(10, 0))
			END AS CRT_pct
		FROM Dflt.[0001161159-Cardiac-20201015]
	UNION
	SELECT [DeviceSerialNumber] as serial,
		   [trenddate] as TrendDate,
		   CASE WHEN [remotesessionannotation] = 'true' THEN 1
				WHEN [remotesessionannotation] = 'false' THEN 0
				ELSE NULL END AS TransmissionFlag,
		   CASE WHEN [dayheartratebpm] = 'NULL' THEN NULL
				WHEN [dayheartratebpm] = '' THEN NULL
				ELSE CAST([dayheartratebpm] as numeric(10, 0))
			END AS DayHR,
		   CASE WHEN [nightheartratebpm] = 'NULL' THEN NULL
				WHEN [nightheartratebpm] = '' THEN NULL
				ELSE CAST([nightheartratebpm] as numeric(10, 0))
			END AS NightHR,
		   CASE WHEN [heartratevariabilityms] = 'NULL' THEN NULL
				WHEN [heartratevariabilityms] = '' THEN NULL
				ELSE CAST([heartratevariabilityms] as numeric(10, 0))
			END AS HRV,
		   CASE WHEN [accumulateddiffohms] = 'NULL' THEN NULL
				WHEN [accumulateddiffohms] = '' THEN NULL
				ELSE CAST([accumulateddiffohms] as numeric(10, 0))
			END AS FluidIndex,
		   CASE WHEN [activitiesofdlylivingminutes] = 'NULL' THEN NULL
				WHEN [activitiesofdlylivingminutes] = '' THEN NULL
				ELSE CAST([activitiesofdlylivingminutes] as numeric(10, 0))
			END AS PhysicalActivity_min,
		   CASE WHEN [timeinatafms] = 'NULL' THEN NULL
				WHEN [timeinatafms] = '' THEN NULL
				ELSE CAST([timeinatafms] as numeric(20, 0))
			END AS TimeInATAF_ms,
		   CASE WHEN [spontaneousvtvfperdaycnt] = 'NULL' THEN NULL
				WHEN [spontaneousvtvfperdaycnt] = '' THEN NULL
				ELSE CAST([spontaneousvtvfperdaycnt] as numeric(10, 0))
			END AS VTVFCnt,
		   CASE WHEN [spontaneousnstperdaycnt] = 'NULL' THEN NULL
				WHEN [spontaneousnstperdaycnt] = '' THEN NULL
				ELSE CAST([spontaneousnstperdaycnt] as numeric(10, 0))
			END AS NSTCnt,
		   CASE WHEN [apercentpaced] = 'NULL' THEN NULL
				WHEN [apercentpaced] = '' THEN NULL
				ELSE CAST([apercentpaced] as numeric(10, 0))
			END AS APaced_pct,
		   CASE WHEN [vpercentpaced] = 'NULL' THEN NULL
				WHEN [vpercentpaced] = '' THEN NULL
				ELSE CAST([vpercentpaced] as numeric(10, 0))
			END AS VPaced_pct,
		   CASE WHEN [effectivcrtpercentpcngday] = 'NULL' THEN NULL
				WHEN [effectivcrtpercentpcngday] = '' THEN NULL
				ELSE CAST([effectivcrtpercentpcngday] as numeric(10, 0))
			END AS CRT_pct
	FROM Dflt.[0001183368-Cardiac-20201015]
	),
	eventlog0 as (
	SELECT [DeviceSerialNum] as serial,
			CAST([TSofInitialDetctn] as date) as TrendDate,
			NumofATPSeqs,
			NumOfShocksDelvd
	FROM [Dflt].[device_final]
	),
	eventlog1 as (
	SELECT serial, TrendDate,
			SUM(NumofATPSeqs) as NumofATPSeqs,
			SUM(NumOfShocksDelvd) as NumOfShocksDelvd
	FROM eventlog0
		GROUP BY serial, TrendDate
	)
SELECT a.ScrSSN, demo.Serial,
	   b.TrendDate, TransmissionFlag, DayHR, NightHR, HRV, FluidIndex, PhysicalActivity_min,
	   CAST(TimeInATAF_ms/(1000*60) as numeric(10, 0)) as TimeInATAF_min, VTVFCnt, NSTCnt, APaced_pct, VPaced_pct, CRT_pct,
	   CASE WHEN c.NumofATPSeqs IS NULL THEN 0 ELSE c.NumofATPSeqs END AS NumofATPSeqs,
	   CASE WHEN c.NumOfShocksDelvd IS NULL THEN 0 ELSE c.NumOfShocksDelvd END AS NumOfShocksDelvd
INTO Dflt.DR_DeviceData
	   FROM (SELECT DISTINCT ScrSSN from Dflt.DR_BLData) as a inner join Dflt.Demographics as demo on a.ScrSSN = demo.ScrSSN
															  inner join comb0 as b on demo.Serial = b.serial
															  left join eventlog1 as c on b.serial = c.serial AND b.TrendDate = c.TrendDate

/*** aggregate ***/

DROP TABLE IF EXISTS #device_tv;
;with comb as (
	SELECT a.ScrSSN, a.startdate, a.id, a.time, a.itv_startdate,
			b.Serial, b.TrendDate, 
			TransmissionFlag, DayHR, NightHR, HRV, FluidIndex, PhysicalActivity_min,
			TimeInATAF_min, VTVFCnt, NSTCnt, APaced_pct, VPaced_pct,
			NumofATPSeqs, NumOfShocksDelvd	
		FROM #long as a left join Dflt.DR_DeviceData as b on a.ScrSSN = b.ScrSSN
			WHERE DATEADD(MONTH, -1, itv_startdate) <= TrendDate 
			 AND TrendDate < itv_startdate)
SELECT id, serial, time,
	  SUM(TransmissionFlag) as TransmissionFlag_cnt,
		AVG(DayHR) as DayHR_mean,
		AVG(NightHR) as NightHR_mean,
		AVG(HRV) as HRV_mean,
		AVG(FluidIndex) as FluidIndex_mean,
		MAX(FluidIndex) as FluidIndex_max,
		AVG(PhysicalActivity_min) as PhysicalActivity_mean,
		MAX(PhysicalActivity_min) as PhysicalActivity_max,
		MIN(PhysicalActivity_min) as PhysicalActivity_mininal,
		AVG(TimeInATAF_min) AS TimeInATAF_mean,
		SUM(VTVFCnt) AS VTVFCnt,
		SUM(NSTCnt) AS NSTCnt,
		AVG(APaced_pct) AS APaced_pct_mean,
		AVG(VPaced_pct) AS VPaced_pct_mean,
		SUM(NumofATPSeqs) AS NumofATPSeqs,
		SUM(NumOfShocksDelvd) AS NumOfShocksDelvd
INTO #device_tv
	FROM comb
	GROUP BY id, serial, time

/* 26.) --------------------------------------------------------------
		date of med crossover (for PP censoring)
*/

DROP TABLE IF EXISTS #RX_CensorDT;
with c1 as (
		SELECT a.ScrSSN, a.Arm, a.startdate, a.enddate,
				CASE WHEN b.Arm = 'ACEI/ARB' THEN 'RX_CEN_ACEI_ARB'
					 ELSE 'RX_CEN_ARNI' END AS Rx_cat, 
				b.startdate as rx_startdate, b.enddate as rx_enddate
		FROM (SELECT DISTINCT ScrSSN, Arm, startdate, enddate from Dflt.DR_TVData) as a inner join #exposure as b on a.ScrSSN = b.ScrSSN
			WHERE b.startdate > a.startdate),
	c2 as (
	SELECT distinct *,
		concat(RX_Cat,'_DT') as RX_Cat_dt
	FROM c1)	
SELECT DISTINCT b.*, 
				CASE WHEN Arm = 'ACEI/ARB' THEN RX_CEN_ARNI_dt
					 ELSE RX_CEN_ACEI_ARB_dt END AS Rx_crossover_dt
INTO #RX_CensorDT
FROM (SELECT ScrSSN, Arm, startdate, enddate, rx_startdate, RX_Cat_dt FROM c2) as t
PIVOT
(
MIN(rx_startdate)
FOR RX_Cat_dt in 
(RX_CEN_ACEI_ARB_dt, 
RX_CEN_ARNI_dt)) as b 
GO

/* 27.) --------------------------------------------------------------
		comb time-varying covs
*/

DROP TABLE IF EXISTS Dflt.DR_TVData;
SELECT a.ScrSSN, a.Arm, a.startdate, a.enddate, x.Rx_crossover_dt,
	   a.id, a.Death_itt_time, a.time, a.itv_startdate,
	   CAST(w.Weight as numeric(10, 1)) as weight_tv,
	   bp.systolic as SBP_tv, bp.Diastolic as DBP_tv,
	   echo.LVEF as LVEF_tv, 
	   CAST(lab.creatinine_tv as numeric(10, 1)) as creatinine_tv, 
	   CAST(lab.potassium_tv as numeric(10, 1)) as potassium_tv, 
	   CAST(lab.BNP_tv as numeric(10, 1)) as BNP_tv, lab.BNP_tvdt, 
	   CAST(lab.proBNP_tv as numeric(10, 1)) as proBNP_tv, lab.proBNP_tvdt,
	   Hosp_num_tv, HFH_num_tv, HFH_cumsum_tv,
	   RX_betaBlocker_tvdt, RX_Diuretic_potassium_tvdt as RX_MRA_tvdt, RX_SGLT2_tvdt, 
	   RX_Diuretic_TZD_tvdt, RX_Diuretics_loop_tvdt, RX_Digoxin_tvdt, RX_lipid_statin_tvdt,
	   serial,
	   TransmissionFlag_cnt, 
	   CAST(DayHR_mean as numeric(10, 1)) as DayHR_mean,
	   CAST(NightHR_mean as numeric(10, 1)) as NightHR_mean,
	   CAST(HRV_mean as numeric(10, 1)) as HRV_mean,
	   CAST(FluidIndex_mean as numeric(10, 1)) as FluidIndex_mean,
	   CAST(FluidIndex_max as numeric(10, 1)) as FluidIndex_max,
	   CAST(PhysicalActivity_mean as numeric(10, 1)) as PhysicalActivity_mean,
	   CAST(PhysicalActivity_max as numeric(10, 1)) as PhysicalActivity_max,
	   CAST(PhysicalActivity_mininal as numeric(10, 1)) as PhysicalActivity_mininal,
	   CAST(TimeInATAF_mean as numeric(10, 1)) as TimeInATAF_mean,
	   CAST(VTVFCnt as numeric(10, 0)) as VTVFCnt,
	   CAST(NSTCnt as numeric(10, 0)) as NSTCnt,
	   CAST(APaced_pct_mean as numeric(10, 2)) as APaced_pct_mean,
	   CAST(VPaced_pct_mean as numeric(10, 2)) as VPaced_pct_mean,
	   CAST(NumofATPSeqs as numeric(10, 0)) as NumofATPSeqs,
	   CAST(NumOfShocksDelvd as numeric(10, 0)) as NumOfShocksDelvd
INTO Dflt.DR_TVData
	FROM #long as a left join #weights_tv as w on a.id = w.id and a.time = w.time
					left join #BP_tv as bp on a.id = bp.id and a.time = bp.time
					left join #LVEF_tv as echo on a.id = echo.id and a.time = echo.time
					left join #Labs_tv as lab on a.id = lab.id and a.time = lab.time
					left join #Hosp_tv as hosp on a.id = hosp.id and a.time = hosp.time
					left join #RX_tv as rx on a.id =rx.id and a.time = rx.time
					left join #device_tv as icd on a.id = icd.id and a.time = icd.time
					left join #RX_CensorDT as x on a.ScrSSN = x.ScrSSN and a.startdate = x.startdate
