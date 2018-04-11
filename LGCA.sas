LIBNAME _ALL_ CLEAR;
LIBNAME ARS 'C:\Users\shaohua\Documents\TSE II\dataset\';

DATA  ARS.rdata ;
LENGTH
 HHID $ 6
 PN $ 3
 educ $ 5
;

INFILE  "C:\Users\shaohua\Documents\TSE II\dataset\mergedata.txt" 
     DSD 
     LRECL= 215 ;
INPUT
 HHID
 PN
 optN1_1
 optP1_1
 optP2_1
 optP3_1
 optN2_1
 optN3_1
 conN1_1
 conN2_1
 conN3_1
 conN4_1
 conN5_1
 conP1_1
 conP2_1
 conP3_1
 conP4_1
 conP5_1
 purP1_1
 purN1_1
 purP2_1
 purN2_1
 purN3_1
 purN4_1
 purP3_1
 opt_1
 con_1
 pur_1
 ARS_1
 optN1_3
 optP1_3
 optP2_3
 optP3_3
 optN2_3
 optN3_3
 conN1_3
 conN2_3
 conN3_3
 conN4_3
 conN5_3
 conP1_3
 conP2_3
 conP3_3
 conP4_3
 conP5_3
 purP1_3
 purN1_3
 purP2_3
 purN2_3
 purN3_3
 purN4_3
 purP3_3
 opt_3
 con_3
 pur_3
 ARS_3
 optN1_2
 optP1_2
 optP2_2
 optP3_2
 optN2_2
 optN3_2
 conN1_2
 conN2_2
 conN3_2
 conN4_2
 conN5_2
 conP1_2
 conP2_2
 conP3_2
 conP4_2
 conP5_2
 purP1_2
 purN1_2
 purP2_2
 purN2_2
 purN3_2
 purN4_2
 purP3_2
 opt_2
 con_2
 pur_2
 ARS_2
 educ
 wave1
 wave2
 wave3
;
LABEL  optN1_1 = "optN1.1" ;
LABEL  optP1_1 = "optP1.1" ;
LABEL  optP2_1 = "optP2.1" ;
LABEL  optP3_1 = "optP3.1" ;
LABEL  optN2_1 = "optN2.1" ;
LABEL  optN3_1 = "optN3.1" ;
LABEL  conN1_1 = "conN1.1" ;
LABEL  conN2_1 = "conN2.1" ;
LABEL  conN3_1 = "conN3.1" ;
LABEL  conN4_1 = "conN4.1" ;
LABEL  conN5_1 = "conN5.1" ;
LABEL  conP1_1 = "conP1.1" ;
LABEL  conP2_1 = "conP2.1" ;
LABEL  conP3_1 = "conP3.1" ;
LABEL  conP4_1 = "conP4.1" ;
LABEL  conP5_1 = "conP5.1" ;
LABEL  purP1_1 = "purP1.1" ;
LABEL  purN1_1 = "purN1.1" ;
LABEL  purP2_1 = "purP2.1" ;
LABEL  purN2_1 = "purN2.1" ;
LABEL  purN3_1 = "purN3.1" ;
LABEL  purN4_1 = "purN4.1" ;
LABEL  purP3_1 = "purP3.1" ;
LABEL  opt_1 = "opt.1" ;
LABEL  con_1 = "con.1" ;
LABEL  pur_1 = "pur.1" ;
LABEL  ARS_1 = "ARS.1" ;
LABEL  optN1_3 = "optN1.3" ;
LABEL  optP1_3 = "optP1.3" ;
LABEL  optP2_3 = "optP2.3" ;
LABEL  optP3_3 = "optP3.3" ;
LABEL  optN2_3 = "optN2.3" ;
LABEL  optN3_3 = "optN3.3" ;
LABEL  conN1_3 = "conN1.3" ;
LABEL  conN2_3 = "conN2.3" ;
LABEL  conN3_3 = "conN3.3" ;
LABEL  conN4_3 = "conN4.3" ;
LABEL  conN5_3 = "conN5.3" ;
LABEL  conP1_3 = "conP1.3" ;
LABEL  conP2_3 = "conP2.3" ;
LABEL  conP3_3 = "conP3.3" ;
LABEL  conP4_3 = "conP4.3" ;
LABEL  conP5_3 = "conP5.3" ;
LABEL  purP1_3 = "purP1.3" ;
LABEL  purN1_3 = "purN1.3" ;
LABEL  purP2_3 = "purP2.3" ;
LABEL  purN2_3 = "purN2.3" ;
LABEL  purN3_3 = "purN3.3" ;
LABEL  purN4_3 = "purN4.3" ;
LABEL  purP3_3 = "purP3.3" ;
LABEL  opt_3 = "opt.3" ;
LABEL  con_3 = "con.3" ;
LABEL  pur_3 = "pur.3" ;
LABEL  ARS_3 = "ARS.3" ;
LABEL  optN1_2 = "optN1.2" ;
LABEL  optP1_2 = "optP1.2" ;
LABEL  optP2_2 = "optP2.2" ;
LABEL  optP3_2 = "optP3.2" ;
LABEL  optN2_2 = "optN2.2" ;
LABEL  optN3_2 = "optN3.2" ;
LABEL  conN1_2 = "conN1.2" ;
LABEL  conN2_2 = "conN2.2" ;
LABEL  conN3_2 = "conN3.2" ;
LABEL  conN4_2 = "conN4.2" ;
LABEL  conN5_2 = "conN5.2" ;
LABEL  conP1_2 = "conP1.2" ;
LABEL  conP2_2 = "conP2.2" ;
LABEL  conP3_2 = "conP3.2" ;
LABEL  conP4_2 = "conP4.2" ;
LABEL  conP5_2 = "conP5.2" ;
LABEL  purP1_2 = "purP1.2" ;
LABEL  purN1_2 = "purN1.2" ;
LABEL  purP2_2 = "purP2.2" ;
LABEL  purN2_2 = "purN2.2" ;
LABEL  purN3_2 = "purN3.2" ;
LABEL  purN4_2 = "purN4.2" ;
LABEL  purP3_2 = "purP3.2" ;
LABEL  opt_2 = "opt.2" ;
LABEL  con_2 = "con.2" ;
LABEL  pur_2 = "pur.2" ;
LABEL  ARS_2 = "ARS.2" ;
RUN;


proc contents data=ARS.rdata;
run;

proc print data=ARS.rdata;
run;

/*Plot the individual data */
proc gplot data = ARS.rdata;
plot 

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 1;
	   order 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 2;
	   order 0 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 3;
	   order 2 2 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');


proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 4;
	   order 1 2 2 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 5;
	   order 2 2 2 2 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 6;
	   order 2 2 2 2 2 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

proc traj data = ARS.rdata outplot = OP outstat = OS out = OF outest = OE ITDETAIL;
	   id HHID PN;
	   var ARS_1 ARS_2 ARS_3;
	   indep wave1 wave2 wave3;
	   model ZIP;
	   ngroups 6;
	   order 2 2 2 2 2 2;
	   IORDER -1;
run;

%TRAJPLOT(OP,OS,'ARS vs. Wave','Zero Inflated Poisson Model','ARS','Wave');

/* Six-class model is the best fit*/
