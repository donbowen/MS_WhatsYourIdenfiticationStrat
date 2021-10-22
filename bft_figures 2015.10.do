version 11.2
set rmsg on
set more off

clear
clear matrix
clear all

set memory 800m
set matsize 800
set scrollbufsize 500000		
capture program drop _all

*** PRESETS

global temp 			"./temp"
cap mkdir "$temp"
global infolder 		"./in_data"
global final_fig_out 	"./out_figure"		
cap mkdir "$final_fig_out"

global start_year = 1980

qui do "bft_functions" // ad-hoc functions

/**********************************************************************************************************
*
*	JOB:			Analysis 
*	AUTHORS:		Don Bowen, Laurent Fresard, Jerome Taillard
*	PROJECT:		Technology Diffusion and Identification/BFT (2014)
* 	INPUT: 			"$infolder/bft_all_paper_obs" 				
*
**********************************************************************************************************/

/*###################################################################################

1	Adoption of the Identification Technology
2	Citations and adoption (time evolution)
3	Editorial boards
4	DD and IV language (best practices)

###################################################################################*/

/*###################################################################################
*	Figure. 1	IDbroad adoption with vertical lines
###################################################################################*/
qui {
	/*##############################################
	
	The logistic function we fit is:
	
	p = K / [ 1 + e(-a-bT) ]
	
	where 
	
	p is the market share of the innovation (fraction),
	K is the equilibrium market share (destination, as frac), and
	T is a time trend variable
	
	Rearranging, 
	
	ln[ P / (K-P) ] = a + bT
	
	this is what we will regress below...
	
	##############################################*/

	set more off
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	
	*** KEEP ONLY AS OF 1980 (but if we change this it needs to change in the follow code as well, so use variable)
	keep if year >=$start_year

	*** get the percentages/"market share"/adoption level
	collapse idenbroad , by(year)
	
	*** DEFINE THE TRANSFORMED LHS VARIABLE
	local k = 1
	foreach var in "idenbroad"  {
		replace `var' = .0001 if  `var' == 0		
		g 	LHS_`var' = log(`var' / (`k' - `var'))
	}		
	
	*** DEFINE THE RHS TIME TREND VARIABLE
	g	time_trend = year - $start_year
	
	*** DO THE REG
	reg LHS_idenbroad time_trend , robust
	predict temp
	
	matrix coeff_beta1 = e(b)

	g	broad_thres_5 	= $start_year + round((-2.94 - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
	g	broad_thres_10 	= $start_year + round((-2.2 - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
	g	broad_thres_15 	= $start_year + round((-1.73 - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
	g	broad_thres_20 	= $start_year + round((-1.39 - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
		
	*** TRANSLATE THE PREDICTION BACK TO THE [0, 1] RANGE
	g	yhat 			= `k' / (1 + exp(-temp) )
	
	*** KEEP THE VARIABLES NEEDED TO GRAPH
	keep year idenbroad yhat time_trend broad_thres*

	*** THIS SECTION SETS UP THE DATA FOR THE FIGURE 
	// 		(We want to plot idenbroad year, yhat year - that's easy)
	//		(This section creates variables to add vertical lines (either from 0 to 1 or 0 to phat)
	g key = _n
	tsset key year
	tsappend, add(1)
	foreach thres in 5 10 15 20 {	
		qui sum broad_thres_`thres'
		replace broad_thres_`thres' = `r(mean)'
		local broad_thres_`thres'_year = `r(mean)'
		replace broad_thres_`thres' = . if broad_thres_`thres' != year
		bysort year (key): replace broad_thres_`thres' = 0 if _n == 1 & broad_thres_`thres' != .
		*bysort year (key): replace broad_thres_`thres' = yhat if _n == 2 & broad_thres_`thres' != .
		bysort year (key): replace broad_thres_`thres' = 1 if _n == 2 & broad_thres_`thres' != .
	}	
	
	twoway 	(line idenbroad year, 	lpattern(solid) lwidth(medthick) lcolor(black)) ///
			(line yhat year, 		lpattern(dash)	lwidth(medthick) lcolor(blue)) ///
				(line  broad_thres_5 year, 		lpattern("..")	lwidth(medthick) lcolor(red)) ///
				(line  broad_thres_10 year, 		lpattern("...")	lwidth(medthick) lcolor(red)) ///
				(line  broad_thres_15 year, 		lpattern("...")	lwidth(medthick) lcolor(red)) ///
				(line  broad_thres_20 year, 		lpattern("...")	lwidth(medthick) lcolor(red)) ///
			, ytitle("Fraction of ECF Articles") xtitle("") /// ytitle("Proportion of ECF papers") xtitle("Years")  ///
			legend(order(1 "Identification Articles (Broad)" 2 "Fitted values" 3 "5% Adoption in `broad_thres_5_year'" 4 "10% Adoption in `broad_thres_10_year'" 5 "15% Adoption in `broad_thres_15_year'" 6 "20% Adoption in `broad_thres_20_year'") rows(3) ) ///
			yscale(range(0 (.05) 1)) ylabel(0(.10) 1) title("") ///
			xlabel(1980(1)2012,  angle(vertical) )		 graphregion(color(white) lwidth(medium)) 
	graph export "$final_fig_out/Figure 1.png", replace	
}
/*###################################################################################
*	Figure 2	- Citations and adoption (time evolution)
###################################################################################*/

	/*###################################################################################
	*	ECF - ID usage
	###################################################################################*/
qui {
	*** get matched pairs

	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	qui matchprem idenbroad					
		// matchprem produces "$temp/CEM_id_control_matches", which is
		//		id - id_control pairs	
		// to this we will bring in event time cites for id, and id_control

	*** get event time cites for all papers	

	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	qui prepforciteyearregs idenbroad
		// -> refmted to id-cite_year units for the 1796 papers, with l_cites_in
	keep id l_cites_in cite_year pub_year
	save "$temp/paperageunits_id", replace
	rename id id_control
	rename l_cites_in_ l_cites_in_control
	drop pub_year
	save "$temp/paperageunits_id_control", replace
	
	*** merge event time cites into matched pairs

	use "$temp/CEM_id_control_matches", clear
	duplicates report *
	g pairtransientid = _n 
	distinct pair
	joinby id using "$temp/paperageunits_id"
	distinct pair
	count
	merge m:1 id_control cite_year using "$temp/paperageunits_id_control", keep(1 3)
	drop if pub_year < 1980
	distinct pair
	
	g y = l_cites_in_ - l_cites_in_control
	
	BFT_matchyearregs
	rename idcoeff estimate
	
	g stderr = estimate / t
	g	ub = estimate + 1.96 * stderr
	g	lb = estimate - 1.96 * stderr
	replace 	ub 	= ub*100
	replace 	lb 	= lb*100
	replace 	estimate 	= estimate*100
	g	zero = 0
	twoway  (rarea ub lb year , pstyle(ci)) ///
			(line estimate year, lpattern(solid) lwidth(thick) lcolor(black) ) ///
			(line zero year, lwidth(med) lcolor(black)) ///
			,  legend(off)  title("") ylabel(-50(25)75, gmax)  xlabel(1980(5)2012) xtitle("")	///
			ytitle("Marginal Effect (%) of ID on Annual Citations") graphregion(color(white) lwidth(medium)) 
	noi li year est t, noo clean
	graph export "$final_fig_out/Figure 2 a ECF.png", replace		
}
	/*###################################################################################
	*	Econ - ID language
	###################################################################################*/
qui {
	*** get matched pairs
	
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 1	
	matchsample_econ used_id_lang	
	
		// matchprem produces "$temp/CEM_id_control_matches", which is
		//		id - id_control pairs	
		// to this we will bring in event time cites for id, and id_control

	*** get event time cites for all papers	
		
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 1	
	qui prepforciteyearregs used_id_lang
		// -> refmted to id-cite_year units for the 1796 papers, with l_cites_in
	keep id l_cites_in cite_year pub_year
	save "$temp/paperageunits_id", replace
	rename id id_control
	rename l_cites_in_ l_cites_in_control
	drop pub_year
	save "$temp/paperageunits_id_control", replace
	
	*** merge event time cites into matched pairs
	
	use "$temp/CEM_id_control_matches", clear
	duplicates report *
	g pairtransientid = _n 
	distinct pair
	joinby id using "$temp/paperageunits_id"
	distinct pair
	count
	merge m:1 id_control cite_year using "$temp/paperageunits_id_control", keep(1 3)
	drop if pub_year < 1980
	distinct pair
	
	g y = l_cites_in_ - l_cites_in_control

	BFT_matchyearregs
	rename idcoeff estimate
	
	g stderr = estimate / t
	g	ub = estimate + 1.96 * stderr
	g	lb = estimate - 1.96 * stderr
	replace 	ub 	= ub*100
	replace 	lb 	= lb*100
	replace 	estimate 	= estimate*100
	g	zero = 0
	twoway  (rarea ub lb year , pstyle(ci)) ///
			(line estimate year, lpattern(solid) lwidth(thick) lcolor(black) ) ///
			(line zero year, lwidth(med) lcolor(black)) ///
			,  legend(off)  title("") ylabel(-50(25)75, gmax)  xlabel(1980(5)2012) xtitle("")	///
			ytitle("Marginal Effect (%) of ID on Annual Citations") graphregion(color(white) lwidth(medium)) 
	noi li year est t, noo clean
	graph export "$final_fig_out/Figure 2 b Econ.png", replace		
}
/*###################################################################################
*	Figure. 3	Editorial boards
###################################################################################*/

	// USES DIFFERENT DATASET
	
/*###################################################################################
*	Figure. 4	DD and IV language (best practices)
###################################################################################*/
qui {

	/*###################################################################################
	*	Figure.  	Diff in diff keywords
	###################################################################################*/
	{
	set more off
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	
	drop if year < $start_year 
	keep if broad_dd == 1
	
	collapse (count) id (sum) bertrand falsificationtest paralleltrend placebotest, by(year)
	
	// make time series begin in 1980
	count
	local new = `r(N)' + 1
	set obs `new'
	replace year = 1980 in `new'
	tsset year
	tsfill	
	foreach v of varlist * {
		replace `v' = 0 if `v' == .
	}	
		
	// we lost the labels in the collapse
	label var id 							"DD papers"
	label var bertrand			"Bertrand et al. (2004)"
	label var falsificationtest 			"Falsification Test"
	label var paralleltrend				"Parallel Trend"
	label var placebotest				"Placebo Test"
	
	twoway ///
		(line  id year,			lwidth(thick) lpattern(solid) lcolor(blue)) ///
		(line  bertrand year,		lwidth(medthick) lpattern(dash) lcolor(black)) ///
		(line  falsificationtest year,		lwidth(medthick) lpattern(dot) lcolor(black)) ///
		(line  paralleltrend  year,			lwidth(medthick) lpattern("_..."  ) lcolor(black)) ///			
		(line  placebotest year,			lwidth(medthick) lpattern(longdash_dot) lcolor(black)) , ///
		ytitle("Number of Articles") xlabel(1980(5)2012) graphregion(color(white) lwidth(medium)) 		
	graph export "$final_fig_out/Figure 4 DD.png", replace		
	}
	

	/*###################################################################################
	*	Figure.  	IV keywords
	###################################################################################*/
	{
	set more off
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	
	drop if year < $start_year 
	keep if broad_iv == 1
	
	collapse (count) id (sum) exclusionr overiden weakinstru stockandyogo hausman_test, by(year)

	// make time series begin in 1980
	count
	local new = `r(N)' + 1
	set obs `new'
	replace year = 1980 in `new'
	tsset year
	tsfill
	
	foreach v of varlist * {
		replace `v' = 0 if `v' == .
	}
		
	// we lost the labels in the collapse
	label var id 							"IV papers"
	label var exclusionr					"Exclusion Restriction"
	label var overiden					"Overidentification"
	label var weakinstru					"Weak Instrument"
	label var stockandyogo				"Stock and Yogo"	
	label var hausman_test 					"Hausman Test"
	
	twoway ///
		(line  id year,						lwidth(thick) 	 lpattern(solid) lcolor(blue)) ///
		(line  weakinstru year,			lwidth(medthick) lpattern(dot) lcolor(black)) ///
		(line  stockandyogo  year,			lwidth(medthick) lpattern("_..."  ) lcolor(black)) ///					
		(line  overiden  year,			lwidth(medthick) lpattern(shortdash ) lcolor(black)) ///			
		(line  exclusionr year,			lwidth(medthick) lpattern(dash_dot) lcolor(black)) ///
		(line  hausman_test year,			lwidth(medthick) lpattern(dash) lcolor(black)) 		, ///
		ytitle("Number of Articles") xlabel(1980(5)2012) graphregion(color(white) lwidth(medium)) 
	graph export "$final_fig_out/Figure 4 IV.png", replace		
	}
}	
	
