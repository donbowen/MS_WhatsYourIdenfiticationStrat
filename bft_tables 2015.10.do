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
global infolder 		"./in_data"
global final_fig_out 	"./out_figure"		
global final_tab_out 	"./out_table"	

global start_year = 1980

global ecf_sample 		ecf == 1 & econjour == 0
global econ_sample 		econjour == 1
global paper_lvl_data   "$infolder/bft_all_paper_obs"

qui do "bft_functions" // ad-hoc functions

/*###################################################################################

	JOB:			Analysis 
	AUTHORS:		Don Bowen, Laurent Fresard, Jerome Taillard
	PROJECT:		Technology Diffusion and Identification/BFT (2014)
 	INPUT: 			"$infolder/bft_all_paper_obs" 				

###################################################################################*/

/*###################################################################################

1	Descriptive Statistics
2	Adoption of Identification Technology 
3	Adoption of Economics and Finance Journals
4	Broader Definition of Empirical Corporate Finance
5	Determinants of Adoption
6	Seniority
7	Straddlers

###################################################################################*/

/*###################################################################################
*	Table 1 	- Descriptive stats
###################################################################################*/

	/*######################## 		Panel A.  	###################################*/
qui {	
	use if econjour == 0 using "$paper_lvl_data", clear 			
	
	g	decade = 1970 	if year < 1980
	replace decade = 1980 if year < 1990 & decade == .
	replace decade = 1990 if year < 2000 & decade == .
	replace decade = 2000 if year < 2010 & decade == .
	replace decade = 2010 if decade == .
	
	g tot = 1
		
	lab def origin 1970 "1970s" 1980 "1980s" 1990 "1990s" 2000 "2010s" 2010 "2010-2012", modify
	estpost tabstat tot, s(sum) by(decade)
	estim store tot
	foreach v in emp cf ecf {
		estpost tabstat `v', s(sum) by(decade)
		estim store `v'_s
		replace `v' = `v' * 100
		estpost tabstat `v', s(mean) by(decade)
		estim store `v'_m
		replace `v' = `v' / 100
	}
	
	noi esttab tot emp_s emp_m cf_s cf_m ecf_s ecf_m , noobs ///
		cells(`"sum(l(N) fmt(0) pattern(1 1 0 1 0 1 0))              mean(l("(%)") fmt(0) pattern(0 0 1 0 1 0 1) ) "') ///
		extracols(2 4 6) nonum mlabels(,none) mgroups("" "Empirical" "CF" "ECF", pattern(0 1 0 1 0 1)) ///
		varlabels(1970 "1970s" 1980 "1980s" 1990 "1990s" 2000 "2000s" 2010 "2010-2012")
}	
	/*######################## 		Panel B.  	###################################*/	
qui {
	use if $ecf_sample using "$paper_lvl_data", clear
	set more off
	eststo clear 
	
	foreach v of varlist broad_*  {
		replace `v' = 0 if `v' == .
	}
	
	keep broad_iv broad_dd broad_selec broad_rdd broad_exper
	order broad_iv broad_dd broad_selec broad_rdd broad_exper
	egen combined = rowtotal(broad_*)
	replace combined = (combined > 0 & combined != .)
	estpost tabstat broad_* combined, s(sum mean) c(s)
	estimates store broad_
	qui esttab broad_  , cells("sum(label(N)) mean(label(\%) fmt(2) par )") mtitles("Broad" ) nonum replace
		
	// these extra steps let me manipulate the variable and header names, which esttab after estpost/est store didn't want to do
	matrix coefs = r(coefs)
	matrix B = I(2)
	matrix B[2,2]= 100
	matrix C = coefs*B	
	mat rownames C = "IV" "DD" "Selection" "RDD" "Experiment" "Any Technique"
	mat colnames C = "N" "(\%)"
	noi matrix list C
	esttab matrix(C , fmt(0 1)), nomtitles title("Panel B: Use of Identification Technologies")
	
	*** First year of techs  	

	use if $ecf_sample using "$paper_lvl_data", clear
	eststo clear 
	keep year broad_iv broad_dd broad_selec broad_rdd broad_exper	
	sum broad* 
	foreach v of varlist broad_*  {
		replace `v' = .    if `v' != 1
		replace `v' = year if `v' == 1
	}	
	noi estpost tabstat broad_iv broad_dd broad_selec broad_rdd broad_exper, s(min) c(s)
}	
	
/*###################################################################################
*	Table 2 	- ECF adoption of ID
	
	The logistic function we fit is:
	
	p = K / [ 1 + e(-a-bT) ]
	
	where 
	
	p is the market share of the innovation (fraction),
	K is the equilibrium market share (destination, as frac), and
	T is a time trend variable
	
	Rearranging, 
	
	ln[ P / (K-P) ] = a + bT
	
	this is what we will regress below...
	
###################################################################################*/
qui {
	qui use if $ecf_sample using "$paper_lvl_data", clear
	
	keep if year >= $start_year

	la var idenbroad     "ID"
	la var broad_rdd    "RDD"
	la var broad_experi "Experiment"

	foreach v in idenbroad broad_iv broad_dd broad_selec broad_rdd broad_experi {
		replace `v' = 0 if `v' == .
	}
	
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(1)  		
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(.8)  		
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(.6)  		
}
/*###################################################################################
*	Table 3 	- Adoption of ID language (Econ vs ECF)
###################################################################################*/
qui {
	use if ( $ecf_sample | $econ_sample ) using "$paper_lvl_data", clear
	drop idenbroad
	keep if searchable
	
	g econ_sample = $econ_sample
	
	noi bft_t3 used_id_lang econ_sample
}	
/*###################################################################################
*	Table 4 	- Broader Definition of Empirical Corporate Finance
###################################################################################*/
qui {
	qui use if ecf == 1 using "$paper_lvl_data", clear
	
	keep if year >= $start_year

	la var idenbroad     "ID"
	la var broad_rdd    "RDD"
	la var broad_experi "Experiment"

	foreach v in idenbroad broad_iv broad_dd broad_selec broad_rdd broad_experi {
		replace `v' = 0 if `v' == .
	}
	
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(1)  		
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(.8)  		
	noi scurvereg idenbroad broad_iv broad_dd broad_selec , k(.6)  		
}
/*###################################################################################
*	Table 5 	- Determinants of Adoption
###################################################################################*/

	/* different sample - career panel */
	
/*###################################################################################
*	Table 6 	- Role of Seniority (Buckets)
###################################################################################*/

	/* different sample - career panel */

/*###################################################################################
*	Table 7 	- Straddler demos
###################################################################################*/

	/* different sample - career panel */
