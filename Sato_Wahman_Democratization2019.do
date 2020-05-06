********************************************************************************
*Name: Yuko Sato
*Created Data:11/13/2017
*Last Modified:7/24/2019
*Objective: Statistical Results (Democratization)
********************************************************************************
cd "/Users/yukosato/Documents/Opposition_project/MPSA2017"

use Sato_Wahman2019.dta, replace
tset newid year

********************************************************************************

*Table 1: Main models - V-Dem electoral score With MM mobilization data (Political behavior /processes protests with more than 1000 participants) 
*** Individual Effects Models
reg difv2x_polyarchy_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, cluster(newid)
xtreg difv2x_polyarchy_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
preserve
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
keep if e(sample)
bysort newidn: gen fe = _N 
xtreg difv2x_polyarchy_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001  if fe >1, fe vce(cluster newid) 
restore

*** Interaction Effects Models
reg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, cluster(newid)
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
preserve
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
keep if e(sample)
bysort newidn: gen fe = _N 
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001 if fe >1, fe vce(cluster newid) 
restore

*** Figure 2: Predicted change in democracy score (Model 5).
*tsset newid year 
set scheme lean2
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
margins oppcoal, at(nonvprotest_process1000l=(0(1)10))
marginsplot, xdimension(nonvprotest_process1000l) yline(0) title("") legend(label(1 "No Coordination") label(2 "Coordination") order (2 1)) /*
*/plot1opts(recast(line) lwidth(.8) lpattern(dash_dot) lcolor(gs10)) plot2opts(recast(line) lwidth(.8) lpattern(dash)) ci1opts(recast(rline) lcolor(gs10) lpattern(dash_dot)) ci2opts(recast(rline) lpattern(dash)) level (95)/*
*/xtitle(Number of Lagged Protest Events (Political Behavior/Process)) ytitle(Predicted Value) name(ME_opposition, replace)

*** Table 2: Predicted changes and illustrative examples (Model 5).
tsset newid year 
xtreg difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)

*Senario 1 (Maximum mobilization with Opposition, mean /median for others)
di _b[_cons] + (4*_b[nonvprotest_process1000l])+ (1*_b[1.oppcoal]) +(4 *_b[1.oppcoal#c.nonvprotest_process1000l]) +(4.53778 *_b[gdpgrol]) ///
+(7.407799 *_b[log_GDPcapl])+ (3.31627 *_b[log_lODAl]) + (80.57907*_b[tradel])+(1 *_b[presidential]) + (62.4715 *_b[lag_govsupport]) + (.3962857*_b[lv2x_polyarchy])
** .10004748
*Senario 2 (Maximum mobilization without opposition)
di _b[_cons] + (4*_b[nonvprotest_process1000l]) +(4.53778*_b[gdpgrol]) ///
+(7.407799 *_b[log_GDPcapl])+ (3.31627 *_b[log_lODAl]) + (80.57907 *_b[tradel])+(1 *_b[presidential]) + (62.4715 *_b[lag_govsupport]) + (.3962857*_b[lv2x_polyarchy])
** .0160638
*Senario 3 (no mobilization with opposition)
di _b[_cons] + (_b[1.oppcoal]) +(4.53778 *_b[gdpgrol]) ///
+(7.407799 *_b[log_GDPcapl])+ (3.31627 *_b[log_lODAl]) + (80.57907 *_b[tradel])+(1 *_b[presidential]) + (62.4715 *_b[lag_govsupport]) + (.3962857*_b[lv2x_polyarchy])
** -.00698709
*Senario 4 (no mobilization without opposition)
di _b[_cons] +(4.53778 *_b[gdpgrol]) ///
+(7.407799 *_b[log_GDPcapl])+ (3.31627 *_b[log_lODAl]) + (80.57907 *_b[tradel])+(1 *_b[presidential]) + (62.4715 *_b[lag_govsupport]) + (.3962857*_b[lv2x_polyarchy])
** .01375274


********************* Robustness Check (Appendix) ******************************

**** Table A3: Robustness check (1) Non-Violent Protest Events (More than 100 Participants) 
* With MM mobilization data (Political behavior /processes protests)  
xtreg difv2x_polyarchy_2 nonvprotest_processl oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_polyarchy_2 c.nonvprotest_processl##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
* With MM mobilization data (Totall number of protests) 
xtreg difv2x_polyarchy_2 protest_nonvl oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_polyarchy_2 c.protest_nonvl##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
* With Banks
xtreg difv2x_polyarchy demonstrationsl oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw, vce(cluster newid)
xtreg difv2x_polyarchy_2 c.demonstrationsl##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw, vce(cluster newid)	

*** Table A4: Effects of Variables on Change in Democracy Score (Disaggregated)
* Freedom of Association
xtreg difv2x_frassoc_thick_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_frassoc_thick  lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_frassoc_thick_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl L.trade presidential lv2x_frassoc_thick  lag_govsupport leader oppw Beralus2001, vce(cluster newid)
* Freedom of Expression
xtreg difv2x_freexp_altinf_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_freexp_altinf lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_freexp_altinf_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_freexp_altinf lag_govsupport leader oppw Beralus2001, vce(cluster newid)
* Clearn Election
xtreg difv2xel_frefair_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2xel_frefair lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2xel_frefair_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2xel_frefair lag_govsupport leader oppw Beralus2001, vce(cluster newid)

*** Table A5: Alternative Dependent Variables
* V-Dem (1-years change: t - t-1)
xtreg difv2x_polyarchy nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_polyarchy c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
* V-Dem (5-years change: t+1 - t-4)
xtreg difv2x_polyarchy_5 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_polyarchy_5 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
	
* Democratic Transition (Wahman et al.)
btscs democratization_WTH year newidn, g(j)
gen j2= j^2
gen j3 = j^3

logit democratization_WTH nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw j j2 j3, vce(cluster newid)
logit democratization_WTH c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw j j2 j3, vce(cluster newid)
	
* Democratic Transition (Boix et al.)
btscs democratization_BMR year newidn, g(l)
gen l2= l^2
gen l3 = l^3

logit democratization_BMR nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw l l2 l3, vce(cluster newid)	
logit democratization_BMR c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw l l2 l3, vce(cluster newid)	
	
*** Table A6: Alternative Dependent Variables (Continued)
* Up-Turn Test
xtreg posi_difv2x_polyarchy_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)	
xtreg posi_difv2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
	
* Down-Turn Test
btscs nega_v2x_polyarchy_2 year newidn, g(u)
gen u2= u^2
gen u3 = u^3	
	
logit nega_v2x_polyarchy_2 nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw u u2 u3, vce(cluster newid)
logit nega_v2x_polyarchy_2 c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw u u2 u3, vce(cluster newid)

*** Large change
xtreg difv2x_polyarchy_2L nonvprotest_process1000l oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
xtreg difv2x_polyarchy_2L c.nonvprotest_process1000l##i.oppcoal gdpgrol log_GDPcapl log_lODAl tradel presidential lv2x_polyarchy lag_govsupport leader oppw Beralus2001, vce(cluster newid)
		
