/************************************************************************************/
/* Research Question: Full-time and Part-time Employment for 4-digit NAICS          */
/************************************************************************************/

DATA work.ftptemp4digNAICS;
SET RTRAdata.LFS202125 (keep = ID PROV AGE LFSSTAT FTPTMAIN NAICS_5 SYEAR SMTH);

/* Province B.C.*/
if PROV = 59;

/* Age 15 or over */
if AGE >= 15;

/* Labour Force Status: In the labour force*/

if LFSSTAT IN (1,2);

/* Part-time and Full-time status */

if ftptmain = '1' then ftpt = 'Full-time';
else if ftptmain = '2' then ftpt = 'Part-time';
else ftpt = 'Unknown';

run;

%RTRAFreq(
     InputDataset=work.ftptemp4digNAICS,
     OutputName=ftptemp2125,
     ClassVarList= SYEAR SMTH ftpt NAICS_5,
     UserWeight=FINALWT);
run;
