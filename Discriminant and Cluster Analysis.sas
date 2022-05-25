 libname stat730 '/home/u58002446/Stat 730';

 data Wines;
 infile '/home/u58002446/Stat 730/wineFl2021.csv' dlm=',' firstobs=2;
 input fixedacidity volatileacidity citricacid residualsugar chlorides freesulfurdioxide 
totalsulfurdioxide
 density pH sulphates alcohol quality type;
 run;

 /*1A*/
 proc sgplot data=wines;
 scatter x=volatileacidity y=chlorides / group=type;
 run;

 /*1B*/
 proc sgplot data=wines;
 scatter x=volatileacidity y=totalsulfurdioxide / group=quality;
 run;

 /*1C*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class type;
 Var volatileacidity chlorides;
 Run;

 /*1D*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=No
 pcov listerr crosslisterr Crossvalidate testlist;
 Class type;
 Var volatileacidity chlorides;
 Run;

 /*1E*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class quality;
 Var volatileacidity chlorides;
 Run;

 /*1F*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=No
 pcov listerr crosslisterr Crossvalidate testlist;
 Class quality;
 Var volatileacidity chlorides;
 priors prop;
 Run;

 /*1G*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class type;
 Var alcohol density;
 Run;

 /*Bonus Point 1*/
 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class quality;
 Var citricacid chlorides;
 Run;

 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class quality;
 Var residualsugar freesulfurdioxide;
 Run;

 Proc Discrim Data=Wines Method=Normal testdata=Wines Pool=Yes
 pcov listerr crosslisterr Crossvalidate testlist;
 Class quality;
 Var pH sulphates;
 Run;

 data Schools;
 infile '/home/u58002446/Stat 730/APBI_NYcounty.csv' dlm=',' firstobs=2;
 input countycode countydesc $ Values $ BusinessMarketing ComputerScience EnglishLanguage
FinePerformingArts
 GlobalStudies Mathematics Other ReligiousEducation Science SecondLanguage
SocialStudies;
 run;

 proc sort data=Schools;
 by values;
 run;

 /*2A 2E Average*/
 PROC CLUSTER DATA=Schools S METHOD=AVERAGE
 CCC PSEUDO OUTTREE=TREE;
 by Values;
 VAR BusinessMarketing ComputerScience EnglishLanguage FinePerformingArts
 GlobalStudies Mathematics Other ReligiousEducation Science SecondLanguage
SocialStudies;
 RUN;

 /*2A 2E Wards*/
 PROC CLUSTER DATA=Schools S METHOD=WAR
 CCC PSEUDO OUTTREE=TREE;
 by Values;
 VAR BusinessMarketing ComputerScience EnglishLanguage FinePerformingArts
 GlobalStudies Mathematics Other ReligiousEducation Science SecondLanguage
SocialStudies;
 RUN;

 /*2B 2E Standardized Average*/
 PROC CLUSTER DATA=Schools S STANDARD METHOD=AVERAGE
 CCC PSEUDO OUTTREE=TREE;
 by Values;
 VAR BusinessMarketing ComputerScience EnglishLanguage FinePerformingArts
 GlobalStudies Mathematics Other ReligiousEducation Science
SecondLanguage SocialStudies;
 RUN;

 /*2B 2E Standardized Wards*/
 PROC CLUSTER DATA=Schools S STANDARD METHOD=WAR
 CCC PSEUDO OUTTREE=TREE;
 by Values;
 VAR BusinessMarketing ComputerScience EnglishLanguage FinePerformingArts
 GlobalStudies Mathematics Other ReligiousEducation Science
SecondLanguage SocialStudies;
 RUN;
