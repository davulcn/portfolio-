***********************************************************;
*  Statistical Computing SAS Project Group 16             *;
***********************************************************;
*  Nurefsan Davulcu & Antoniya Antonova					  *;
***********************************************************;

/*QUESTION 1*/
LIBNAME SASproj "H:/statcomp2020/project";
%let datafolder = H:/statcomp2020/project;

/*QUESTION 2*/

ods pdf file="H:/statcomp2020/project/SASproj.pdf" startpage=no pdftoc=1;

ods noproctitle;

ods proclabel "SAS Project Report - Group 16";

title1 "SAS Project Report - Group 16" height=11 color=bib;


ods proclabel "Nurefsan Davulcu & Antoniya Antonova";

title2 "Nurefsan Davulcu & Antoniya Antonova";

ods proclabel "1. Introduction";


proc odstext contents="1. Introduction";

h1 '1. Introduction';
  p    'Traffic accidents may not be as publicly covered as airplane crashes but they certainly charge a heavy 
toll on the economy since they have repercussions on the productivity, medical costs,
legal and court costs,
emergency services costs, insurance administration costs,
	congestion costs, property damage, and workplace losses.';


  p   'A report from the U.S. Department of Transportation from 2015 gives, to name 
but a few examples, the following :The $242 billion cost of motor vehicle crashes represents the equivalent of nearly $784 for each
of the 308.7 million people living in the United States, and 1.6 percent of the $14.96 trillion real U.S. Gross Domestic Product for
2010.';


  p   'In this report we study the data provided in the US Accidents data set which 
was collected over the period of 52 months starting February 2016 until June 2020.
Data was collected through different channels including two APIs, over 49 states, and 
contains approximately 3.5 million records to this day.
The focus of our report was the state of New York where we looked at the 
number of accidents per country, 
	the severity of the accidents and the weather conditions during the accidents ';


h2 'Question 2';
run;

ods select variables;
proc contents data=SASproj.group16_ny;
run;

title;

ods pdf exclude all;

/*ods select default;

proc print data=SASproj.group16_ny (obs=10);
run;*/

/*code below adds the variables table to the report
ods pdf file="H:/statcomp2020/project/SASproj.pdf";
ods pdf close;*/
/*QUESTION 3*/
data SASproj.group16_ny;
	set SASproj.group16_ny (rename=(Temperature_F_=Temp Wind_Chill_F_=WindChill 
		Humidity___=Humidity Pressure_in_=Pressure Visibility_mi_=Visibility 
		Wind_Speed_mph_=WindSpeed Precipitation_in_=Precip));
	label Temp=Temperature(F) WindChill=Wind Chill(F) Humidity=Humidity(_%) 
		Pressure=Pressure(in) Visibility=Visibility(mi) WindSpeed=Wind Speed (mph) 
		Precip=Precipitation(in);
run;

/*check it worked
ods select variables;

proc contents data=SASproj.group16_ny;
run;

ods select default;*/
/*QUESTION 4*/

ods pdf exclude none;
proc odstext contents="Question 4";
h1 'Question 4';
run;

%let vars=Pressure Humidity Temp WindChill Visibility WindSpeed Precip;

PROC TABULATE data=SASproj.group16_ny;
	class severity;
	var &vars;
	table severity, (&vars)*(mean var);
run;

ods pdf exclude all;

/*Question 5 - Which county has the highest number of accidents?

/*Part A
1.Use the first.variable and an accumulating column to achieve this.
Output only the final number for each county.
Do not include the table in your report.*/

PROC SORT data=SASproj.group16_ny;
	by County;
run;

DATA q5a;
	set SASproj.group16_ny (keep=County);
	by County;

	if first.County=1 then
		Tot_Accident=0;
	Tot_Accident+1;

	if last.County=1 then
		output;
run;

PROC SORT data=q5a;
	by descending Tot_Accident;
run;


/*second way - using proc summary*/


proc summary data=SASproj.group16_ny nway;
	class county / descending order=freq;
	output out=top10;
run;

ods pdf exclude none;

proc odstext contents="Question 5";

h1 'Question 5';

run;

/*WHY I CAN'T ADD THIS TABLE ... PFFF*/

data top10;
	set top10 (obs=10 drop=_TYPE_);
run;
/*aha!! worked witht he below*/
proc print data=top10 ;
run;

ods pdf exclude all;
/*same*/
/*not THIS not THIS not THIS -
/*but this is also good just need to get rid of extra columns :)
proc freq data=SASproj.group16_ny order=freq;
table county;
output out=top10 (obs=10);
run;
*/
/*Question 6 - Top10 -> subset with data set from question 3 TO THESE 10 counties.
Save that table as a permanent data set named top10.*/
/*not working- gotta fix*/
proc sql;
	create table SASproj.top10 as select * from SASproj.group16_ny as a inner 
		join (select county from top10) as b on a.county=b.county;
quit;

/*check it worked correct- YES
proc freq data=SASproj.top10 order=freq;
table county;
run;

proc print data=top10; run;*/
/*QUESTION 7*/
ods pdf exclude none;
proc odstext contents="Question 7";
h1 'Question 7';
run;

ods rtf file="&datafolder/Proj_Report.rtf";

proc sgplot data=SASproj.top10;
	vbar County / categoryorder=respdesc;
run;

ods rtf close;

ods text="Bar Graph in question 7 shows the start difference in numbers of accidents
within the top 10 list, with county Erie having more than three times less accidents 
than the county Westchester which is in the first position";
run;

ods pdf exclude all;


/*QUESTION 8*/
/*Part A*/
ods select MissPattern;

PROC MI data=SASproj.group16_ny nimpute=0;
	var &vars;
run;

/*Part B*/

PROC MEANS data=SASproj.group16_ny nmiss;
	var &vars;
	output out=cmissing nmiss(&vars)=/autoname;
run;
ods pdf exclude none;

proc odstext contexts="Question 8 Part B";
h1 'Question 8 Part B';
run;
ods text="It appears that data entries on Precipitation, Wind Chill and Wind
speed are incomplete most commonly";
proc print data=cmissing;
run;

ods pdf exclude all;
/*DONT INCLUDE THIS IN REPORT INCLUDE TRANSPOSE2 BELOW*/
/*Part C*/



proc transpose data=cmissing out=transpose (rename=(COL1=nmiss));
run;


data transpose2;
	set transpose;
	where _NAME_ not in ("_TYPE_" "_FREQ_");
run;

proc print data=transpose2;
run;



/*Part D*/
proc sql;
	select nmiss into : nmiss1-:nmiss7 from transpose2;
quit;

/*Part E*/

ods pdf exclude none;
proc odstext contents="Question 8 Part E";
h1'Question 8 Part E';
run;
%macro print_macro();
	%do i=1 %to 7;
		%put macro value nmiss&i is: &&nmiss&i;
	%end;
%mend;

/* print nmiss1-7 in log*/
%print_macro() /*Part F*/
proc sgplot data=transpose2;
vbar _NAME_ / response=nmiss CATEGORYORDER=RESPDESC;
run;

/*Part F*/
ods pdf exclude all;

data temp;
	set SASproj.top10;
	cmissing=cmiss(of &vars);
run;

proc print data=temp (obs=10);
run;

/*Part G*/
data SASproj.top10_cmiss;
	set temp;
	where cmissing<=3;
run;

/*QUESTION 9*/
/* Part A */
proc format;
	value severe 0='less severe' 1='more severe';
run;

data SASproj.top10_cmiss;
	set SASproj.top10_cmiss;

	if severity in (1, 2, 3) then
		severity4=0;
	else
		severity4=1;
run;

/* Part B */

ods pdf exclude none;
proc odstext contents="Question 9 Part B";
h1'Question 9 Part B';
p 'With the data displaying the number of accidents over
 the period covered by the study it is interesting to notice 
 that where as the total number of accidents, 
 and the  less severe accidents peak in 2019
 and plummet in 2020, 
 the more severe accidents not only continuously rise, 
 they almost double in 2020. ';
run;
DATA SASproj.top10_cmiss;
	set SASproj.top10_cmiss;
	Year=year(datepart(Start_time));
run;

PROC FREQ data=SASproj.top10_cmiss;
	table severity4*(Year) / out=sev_Year;
	format severity4 severe.;

	/*format Start_time Year4.; - this doesn't work so I have created year var above*/
run;

/* Part C */
proc odstext contents="Question 9 Part C";
h1'Question 9 Part C';
run;
PROC SGPLOT data=SASproj.top10_cmiss;
	vline Year / group=County;
run;

/* Part D */

ods pdf exclude all;

DATA SASproj.top10_cmiss;
	set SASproj.top10_cmiss;
	Month=month(datepart(Start_time));
	Day=day(datepart(Start_time));
	Hour=hour(timepart(Start_time));
run;

/* Part E */

ods pdf exclude none;
proc odstext contents="Question 9 Part E";
h1 'Question 9 Part E';
p 'When studying the numbers of accidents per month,
 day of the month and hour, and level of severity, 
We can observe a peak in July, 
followed by a drop in August, 
which is a typical holiday month, 
Whereas with the days of the week
 a somewhat clear seven-day pattern appears,
 of course day 31 being less common is seen to display 
 almost double less numbers of accidents. 
 In turn, the numbers for accidents to an hour clearly
 reflect the peak in morning hours rush hour, 
 with a drop midday and a peak at what is a typical 9 to 5 routine,
 and plummeting in the small hours of the night.';
run;
proc sgplot data=SASproj.top10_cmiss;
	vbar Month / group=severity4 groupdisplay=cluster;
run;

proc sgplot data=SASproj.top10_cmiss;
	vbar Day / group=severity4 groupdisplay=cluster;
run;

proc sgplot data=SASproj.top10_cmiss;
	vbar Hour / group=severity4 groupdisplay=cluster;
run;

ods pdf exclude all;

/*QUESTION 10 - association
PROC GLM for odds ration and CI*/




/*QUESTION 11 - HISTOGRAM WITH KERNEL DENSITY*/

ods pdf exclude none;
proc odstext contents="Question 11";
h1'Question 11';
run;
proc univariate data=SASproj.top10_cmiss;
	var start_lat start_lng;

	/* computes descriptive statisitcs */
	histogram start_lat / kernel overlay odstitle="Histogram for start_lat";
	histogram start_lng / kernel overlay odstitle="Histogram for start_lng";
	ods select histogram;
run;

ods pdf exclude all;

/*QUESTION 12 - Weather conditions affecting severity*/
/* Part A */
PROC SQL;
	select count(*) as Accidents, Weather_Condition from SASproj.top10_cmiss where 
		severity4=1 group by Weather_Condition order by Accidents desc;
quit;

/*mostly cloudly weather conditions have the highest number of severe accidents*/
/* Part B */

ods pdf exclude none;
proc odstext contents="Question 12 Part B";
h1'Question 12 Part B';
run;
/*Three weather variables with highest correlation*/
title "Best 3 Correlated Predictors";

proc corr data=SASproj.top10_cmiss nosimple best=3;
	var &vars;
	with severity4;
run;


ods pdf close;

/*Pressure precipitation and visibility*/
%MACRO weather_analysis(weather);
	/*boxplot*/
	title "Boxplot for &weather by Severity";

	proc sgplot data=SASproj.top10_cmiss;
		vbox &weather / category=severity4;
		format severity4 severe.;
	run;

	/*histogram - both work*/
	title "Histogram for &weather by Severity";

	proc sgpanel data=SASproj.top10_cmiss;
		panelby severity4 / layout=rowlattice;
		histogram &weather;
		format severity4 severe.;
	run;

	/*Two-sample t-test*/
	title "T-test for &weather";

	proc ttest data=SASproj.top10_cmiss;
		class severity4;
		var &weather;
	run;

%MEND;

%weather_analysis(Pressure);
%weather_analysis(Precip);
%weather_analysis(Visibility);