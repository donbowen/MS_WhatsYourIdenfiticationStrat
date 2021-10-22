cap prog drop scurvereg
prog def scurvereg
*scurvereg dummyvarname, k(1)
* estimates s-curve for many vars, each with one ceiling "k" value
syntax varlist, [k(real 1 ) htmlfileout(string) texfileout(string) printrealtrend ]
qui {
	eststo clear
	
	if "`printrealtrend'" == "printrealtrend" {
		preserve
		noi tabstat `varlist', s(mean) by(year) format(%9.2g)
		restore
	}
	
	*** LOOP OVER VARIABLES TO ESTIMATE
	foreach 1 in `varlist' {
	preserve
			
		noi di as res "DOING `1'"

		collapse `1', by(year)
		tsset year
		g	time_trend = year - $start_year
		
		replace `1' = 0.0001 if `1' == . | `1' == 0  	// else log transform fails
		keep if year >=$start_year				

		*** DEFINE THE TRANSFORMED LHS VARIABLE AND THE RHS TIME VARIABLE
		g tempsave = `1'
		
		* the transformation below takes log(`k' - `1')... ensure this is positive
		replace `1' = `k' - .0001 if `1' >= `k' & `1' != . 		
		replace `1' = log(`1' / (`k' - `1'))

		*** DO THE REG 	
		eststo: reg `1' time_trend , robust
		matrix coeff_beta1 = e(b)
		
		*** PREP OUTPUT OUTPUT FOR TABLE
		foreach level in 5 10 15 20 25 30 35 40 45 50 {
			if `level' == 5 local thresh  = -2.94
			if `level' == 10 local thresh = -2.2
			if `level' == 15 local thresh = -1.73
			if `level' == 20 local thresh = -1.39
			if `level' == 25 local thresh = -1.1
			if `level' == 30 local thresh = -0.85
			if `level' == 35 local thresh = -0.62
			if `level' == 40 local thresh = -0.41
			if `level' == 45 local thresh = -0.2
			if `level' == 50 local thresh =  0			
			
			* report threshold year if reached by sample end (speculative)
			local thresyearhere = $start_year + round((`thresh' - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
			if `thresyearhere' > 2012 estadd scalar broad_thres_`level' = . 
			if `thresyearhere' <= 2012 estadd scalar broad_thres_`level' = `thresyearhere'
		}
		*** build a nasty string variable for esttab wrather than write it out
		local scalar_strings = ""
		foreach level in 5 10 15 20 25 30 35 40 45 50 {
			local scalar_strings = `"`scalar_strings' "broad_thres_`level' `level'\%""'
		}
		drop temp 
		
	restore
	}
	
	*** MAKE TABLE
	
	noi esttab, ///  using "$final_tab_out/raw from stata/ECF ID Adoption.tex", ///
		r2 obs scalars(`scalar_strings') ///title("Logistic Fraction of Adoption") ///
		l nonum replace substitute("Constant" "$\alpha$" "time_trend" "$\beta$") 
				
}
end

*###############################################################################
*###############################################################################
*###############################################################################
*###############################################################################

cap prog drop bft_t3
prog def bft_t3
/*###################################################################################
*	Table.  3	Adoption of Identification Language in Economics and Finance
###################################################################################*/
qui {
preserve
{

*local 1 used_id_lang
*local 2 econjour 
	
	*************************************************************************
	**** Begin: Set up for regression tests, where we want finance ECF articles + econ articles only
	* (we're going to compare finance ecf to all econ articles)
	*************************************************************************
		
	* collapse by econfield year 
	collapse (count) tot_published_papers=id (sum) `1', by(`2' year) 
	
	* get fraction of papers that use a technique for the econ journal articles, separately do same for finance Ec
	local ID_techniques "`1'"
	foreach var of local ID_techniques  {
		gen p_`var' = `var' / tot_published_papers
	}

	* the tests below take logs of the p_* variables. we don't want to lose any, so set them to a small number if zero
	foreach var of local ID_techniques  {
		replace p_`var' = .0001 if  p_`var' == 0
	}

*li year econjour p_ tot_ used , noobs clean
	
	*************************************************************************
	**** set up for the test
	*************************************************************************
	
	* DEFINE THE TRANSFORMED KEY PROPORTION OF INTEREST

	tsset `2' year // PANEL DATA

	* KEEP ONLY AS OF 1980 (but if we change this it needs to change in the follow code as well, so use variable)
	keep if year >= $start_year

	* DEFINE A TIME TREND VARIABLE (TAKES VALUE ZERO IN 1980)
	gen time_trend = year - $start_year
	label variable time_trend `"$\beta$"'
	
	eststo clear
	
	foreach var of local ID_techniques  {
	
	foreach k in 1 0.8 0.6 {
		local equnum = `k'*10
		gen LHS`var' = log(p_`var' / (`k' - p_`var'))
	

	****************************************************************************************************************
	**** RUN THE REGRESSIONS
	****************************************************************************************************************
		
		******************************
		**** ECON JOURNALS
		******************************
		reg LHS`var' time_trend if `2' == 1
		summarize LHS`var'
		estimates store econ_equ`equnum'

		*******************************************************************************************************************
		**** COMPUTATION OF "ORIGIN" POINT
		**** SIMILAR TO GRILICHES (HE USES 10% ADOPTION RATE AS THRESHOLD FOR ADOPTION (AS OPPOSED TO EXPERIMENTATION))
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -1.39 FOR 20%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -1.73 FOR 15%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -2.2 FOR 10%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -2.94 FOR 5% 
		**** WE USE 5% THRESHOLD
		*******************************************************************************************************************
		matrix coeff_beta1 = e(b)
		
		foreach level in 5 10 15 20 25 30 35 40 45 50 {
			if `level' == 5 local thresh  = -2.94
			if `level' == 10 local thresh = -2.2
			if `level' == 15 local thresh = -1.73
			if `level' == 20 local thresh = -1.39
			if `level' == 25 local thresh = -1.1
			if `level' == 30 local thresh = -0.85
			if `level' == 35 local thresh = -0.62
			if `level' == 40 local thresh = -0.41
			if `level' == 45 local thresh = -0.2
			if `level' == 50 local thresh =  0			
			*di "`thresh'"
			scalar slope_econ = el(coeff_beta1, 1,1)
			scalar origin_econ_`level' = $start_year + round((`thresh' - el(coeff_beta1, 1,2))/el(coeff_beta1, 1,1))
			scalar origin_`level' = origin_econ_`level'
			estadd scalar origin_`level'
		}
		

		******************************
		**** FINANCE JOURNALS
		******************************
		reg LHS`var' time_trend if `2' == 0
		estimates store finance_equ`equnum'

		*******************************************************************************************************************
		**** COMPUTATION OF "ORIGIN" POINT
		**** SIMILAR TO GRILICHES (HE USES 10% ADOPTION RATE AS THRESHOLD FOR ADOPTION (AS OPPOSED TO EXPERIMENTATION))
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -1.39 FOR 20%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -1.73 FOR 15%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -2.2 FOR 10%
		**** THE INVERSE CUMULATIVE THRESHOLD IS AT -2.94 FOR 5% 
		**** WE USE 5% THRESHOLD
		*******************************************************************************************************************
		matrix coeff_beta2 = e(b)

		foreach level in 5 10 15 20 25 30 35 40 45 50 {
			if `level' == 5 local thresh  = -2.94
			if `level' == 10 local thresh = -2.2
			if `level' == 15 local thresh = -1.73
			if `level' == 20 local thresh = -1.39
			if `level' == 25 local thresh = -1.1
			if `level' == 30 local thresh = -0.85
			if `level' == 35 local thresh = -0.62
			if `level' == 40 local thresh = -0.41
			if `level' == 45 local thresh = -0.2
			if `level' == 50 local thresh =  0						
			*di "`thresh'"
			scalar slope_fin = el(coeff_beta2, 1,1)
			scalar origin_fin_`level' = $start_year + round((`thresh' - el(coeff_beta2, 1,2))/el(coeff_beta2, 1,1))
			scalar origin_`level' = origin_fin_`level'
			estadd scalar origin_`level'
		}
	
		*******************************************************************************************************************
		**** TEST OF DIFFERENCES IN COEFFICIENTS ACROSS MODELS (NON-NESTED)
		****
		**** STEP 1: RUN SEEMINGLY UNRELATED EQUATION (SUEST)
		**** STEP 2: PERFORM LINEAR TEST OF SLOPE DIFFERENCES
		**** STEP 3: PERFORM NON-LINEAR TEST OF ORIGINS DIFFERENCES
		*******************************************************************************************************************
		
		**** STEP 1: RUN SEEMINGLY UNRELATED EQUATION (SUEST)
		suest econ_equ`equnum' finance_equ`equnum'

		**** STEP 2: PERFORM LINEAR TEST OF SLOPE DIFFERENCES
		test [econ_equ`equnum'_mean]time_trend = [finance_equ`equnum'_mean]time_trend
		estadd scalar diff_in_slopes = slope_fin - slope_econ : econ_equ`equnum'
		estadd scalar diff_in_slopes = r(p) : finance_equ`equnum'

		**** STEP 3: PERFORM NON-LINEAR TEST OF ORIGINS DIFFERENCES
		foreach level in 5 10 15 20 25 30 35 40 45 50 {
			if `level' == 5 local thresh  = -2.94
			if `level' == 10 local thresh = -2.2
			if `level' == 15 local thresh = -1.73
			if `level' == 20 local thresh = -1.39
			if `level' == 25 local thresh = -1.1
			if `level' == 30 local thresh = -0.85
			if `level' == 35 local thresh = -0.62
			if `level' == 40 local thresh = -0.41
			if `level' == 45 local thresh = -0.2
			if `level' == 50 local thresh =  0			
			*di "`thresh'"		
			*di "`level'"
			testnl (`thresh'-[econ_equ`equnum'_mean]_cons)/[econ_equ`equnum'_mean]time_trend = (`thresh'-[finance_equ`equnum'_mean]_cons)/[finance_equ`equnum'_mean]time_trend
			estadd scalar diff_in_origins_`level' = origin_fin_`level' - origin_econ_`level' : econ_equ`equnum'
			estadd scalar diff_in_origins_`level' = r(p) : finance_equ`equnum'
		}

		**** STEP 4: SAVE AND OUTPUT
		
	local stat_strings1 = ""	// origins
	local stat_strings1_1 = ""	// origins fmt
	local stat_strings1_2 = "" 	// origins label
	local stat_strings2 = ""	// diff in origins
	local stat_strings2_1 = ""	// diff in origins fmt
	local stat_strings2_2 = ""	// diff in origins label
	foreach level in 5 10 15 20 25 30 35 40 45 50 {
		local stat_strings1 = `"`stat_strings1' origin_`level' "'
		local stat_strings1_1 = `"`stat_strings1_1' 0 "'		
		local stat_strings1_2 = `"`stat_strings1_2' `level'\%"'		

		local stat_strings2 = `"`stat_strings2' diff_in_origins_`level' "'
		local stat_strings2_1 = `"`stat_strings2_1' 3 "'	
		local stat_strings2_2 = `"`stat_strings2_2' Diff`level'"'		
	}	
	di `"`stat_strings1'"'
	di `"`stat_strings1_1'"'
	di `"`stat_strings1_2'"'
	di `"`stat_strings2'"'
	di `"`stat_strings2_1'"'
	di `"`stat_strings2_2'"'
	
	
	
	drop LHS`var'
	}	
	/*
	esttab econ_equ10 finance_equ10 econ_equ8 finance_equ8 econ_equ6 finance_equ6 using "$final_tab_out/raw from stata/econ_vs_finance_`var'.tex", /// 
	ar2 obslast b(%12.3f) star(* 0.10 ** 0.05 *** 0.01) label title(`LHS_var')  mtitles("Economics" "Finance") nogaps nodepvars par lines ///
	/// stats(		origin_5  diff_in_origins_5 p_val_origins_5 origin_10 diff_in_origins_10 p_val_origins_10 origin_15 diff_in_origins_15 p_val_origins_15 origin_20 diff_in_origins_20 p_val_origins_20 				diff_in_slopes p_val_slopes r2 N, fmt(0 0 3 0 0 3 0 0 3 0 0 3 3 3 3 0) label("Origin5" "Diff in origins5" "P-val5" "Origin10" "Diff in origins10" "P-val10" "Origin15" "Diff in origins15" "P-val15" "Origin20" "Diff in origins20" "P-val20" "Diff in slopes" "P-val" "r-squared" "N")) order(`RHS_varlist') substitute("_cons" "Constant") replace ///
	stats(	`stat_strings1' r2 N `stat_strings2' diff_in_slopes, ///
	fmt(`stat_strings1_1' 3 0 `stat_strings2_1'3 ) label(`stat_strings1_2' "R$^2$" "N" `stat_strings2_2'   "Diff in slopes" )) order(`RHS_varlist') substitute("Constant" "$\alpha$") replace				
	*/
	}

}

	esttab econ_equ10 finance_equ10 econ_equ8 finance_equ8 econ_equ6 finance_equ6 , /// 
	ar2 obslast b(%12.3f) star(* 0.10 ** 0.05 *** 0.01) label title(`LHS_var') nogaps nodepvars par lines ///
	/// stats(		origin_5  diff_in_origins_5 p_val_origins_5 origin_10 diff_in_origins_10 p_val_origins_10 origin_15 diff_in_origins_15 p_val_origins_15 origin_20 diff_in_origins_20 p_val_origins_20 				diff_in_slopes p_val_slopes r2 N, fmt(0 0 3 0 0 3 0 0 3 0 0 3 3 3 3 0) label("Origin5" "Diff in origins5" "P-val5" "Origin10" "Diff in origins10" "P-val10" "Origin15" "Diff in origins15" "P-val15" "Origin20" "Diff in origins20" "P-val20" "Diff in slopes" "P-val" "r-squared" "N")) order(`RHS_varlist') substitute("_cons" "Constant") replace ///
	stats(	`stat_strings1' r2 N `stat_strings2' diff_in_slopes, ///
	fmt(`stat_strings1_1' 3 0 `stat_strings2_1'3 ) label(`stat_strings1_2' "R$^2$" "N" `stat_strings2_2'   "Diff in slopes" )) order(`RHS_varlist') substitute("Constant" "$\alpha$") replace				

	matrix B = r(coefs)
	matrix C = r(stats)
	matrix list B
	matrix list C	

	matrix A = J(10,9,1)

	forval model = 1/3 {
		forval econ = 1/2 {
			local acol = (`model'-1)*3 + `econ'
			local bcol = (`model'-1)*2 + `econ'
			matrix A[1,`acol'] = round(B[1,`bcol'*3-2],.001)
			matrix A[2,`acol'] = round(B[1,`bcol'*3-1],.001)
			matrix A[3,`acol'] = round(B[2,`bcol'*3-2],.001)
			matrix A[4,`acol'] = round(B[2,`bcol'*3-1]	,.001)
			matrix A[5,`acol'] = C["r2",`bcol']
			matrix A[6,`acol'] = C["N",`bcol']	
			matrix A[7,`acol'] = C["origin_5",`bcol']	
			matrix A[8,`acol'] = C["origin_10",`bcol']	
			matrix A[9,`acol'] = C["origin_15",`bcol']	
			matrix A[10,`acol']= C["origin_20",`bcol']				
		}
		local acol = (`model'-1)*3 + 3
		local bcol = (`model'-1)*2 + 2
		matrix A[7,`acol'] = C["diff_in_origins_5",`bcol']	
		matrix A[8,`acol'] = C["diff_in_origins_10",`bcol']	
		matrix A[9,`acol'] = C["diff_in_origins_15",`bcol']	
		matrix A[10,`acol']= C["diff_in_origins_20",`bcol']	
		
		matrix A[1,`acol'] = C["diff_in_slopes",`bcol'-1]	
		matrix A[2,`acol'] = C["diff_in_slopes",`bcol']	
	}
	matrix colnames A = k1_EC k1_F k1_diff k8_EC k8_F k8_diff k6_EC k6_F k6_diff
	matrix list A
}
mat li A
restore
end

*###############################################################################
*###############################################################################
*###############################################################################
*###############################################################################

cap prog drop matchprem
prog def matchprem	
qui {	
/*###################################################################################
*	IDENT PREMIUM, COARSE EXACT MATCHING
###################################################################################*/
		
	keep id issue_id jour `1' year citationinfo jour authorcount l_previous_cites l_previous_papers

	* get the month of publications	

	replace citationinfo = proper(citationinfo)
	g month = ""
	foreach m in Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {
		replace month = "`m'" if regexm(citationinfo,"`m'")
	}
	// rfs: 1 per season through 2007
	replace month = "Jan" if month == "" & jour == "RFS" & year < 2008 & regexm(citationinfo, "No. 1")
	replace month = "Apr" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 2")
	replace month = "Jul" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 3")
	replace month = "Oct" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 4")
	
	foreach m in Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {
		replace month = "`m'" if regexm(citationinfo,"`m'")
	}
	replace month = "Oct" if issue_id == 1056
	replace month = "Jan" if issue_id == 1071   
	replace month = "Oct" if issue_id == 1158  
	
	tab month, m
	drop if month == ""	// 1 
	
	g month_num = mofd(date(month,"M"))
	tab month_num if month == "Jan"		// jan is 0
	tab month_num if month == "Dec"		// dec is 11
	
	g yearhalf = floor(month_num / 3)
	tab month_num yearhalf
	
	drop month issue_id

	tab authorcount
	
	* match variables: year, yearhalf, prev papers
	* convert variables into strata
		
	sum l_previous_papers, d
	replace l_previous_papers = . if l_previous_papers == 0
	xtile papers_strata = l_previous_papers, n(3)		// strata: terciles
	replace papers = 0 if papers == .						// strata: 0, terciles if positive
	tab papers
	tabstat l_previous_papers, s(min max) by(papers)
		
	* count treatment and control by strata, impose restrictions
	
	****************************************************************************
	egen strata = group(jour year yearhalf papers_strata)
	****************************************************************************
	egen strata_pap = count(`1'), by(strata)
	egen strata_iden = sum(`1'), by(strata)
	g strata_noniden = strata_pap - strata_iden
	
	* note how we lose treatment obs as the number of possible controls is added
	
	distinct id if `1' == 1 									// 436
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 1	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 2	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 3	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 4	// 381
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 5	// 329
		
	* save 	
	
	****************************************************************************
	keep if strata_iden > 0 & strata_non >= 1
	****************************************************************************
	
	preserve 
	keep if `1' == 1
	keep id strata l_previous_cites
	save "$temp/CEM_papers_iden", replace
	restore
	keep if `1' == 0
	keep id strata l_previous_cites
	rename (id l_previous_cites ) ( id_control l_previous_cites_control)
	save "$temp/CEM_papers_non", replace
	
*** build the id/control dataset

	use "$temp/CEM_papers_iden", clear
	count
	joinby strata using "$temp/CEM_papers_non"
	g diff = abs(l_previous_cites - l_previous_cites_control)
	sort id diff	
	egen scoreg = group(id diff)
	by id: egen myrank = rank(scoreg), tr
	distinct id					// a check:
	distinct id if myrank == 1	// this should have same num of distinct; extras on second means we have to break ties
	keep if myrank == 1
	set seed 1000
	sample 1, count by(id)
	count						// this should have same num of distinct
	keep id id_control
	save "$temp/CEM_id_control_matches", replace
	
*** merge in event time cites for each, produce event time difference in cites

	preserve
	global basesample "$temp/fin_ecf_articles"
	use "$infolder/bft_all_paper_obs", clear
	keep if econjour == 0 & ecf == 1
	save "$basesample", replace
	restore

	use "$temp/CEM_id_control_matches", clear
	rename (id id_control) (id_temp id)
	merge m:1 id using "$basesample", keepusing(event_time*) keep(1 3)
	rename event_time* control*
	drop _m
	rename (id_temp id) (id id_control) 
	merge m:1 id using "$basesample", keepusing(year event_time*) keep(1 3)
	rename event_time* iden*

	reshape long iden_cites_ control_cites_, i(id) j(age)
	
	drop if age > 15

	* % difference
	
	g    lgdiff = log(1+iden) - log(1+control)
	eststo clear
	eststo: areg lgdiff i.age, vce(cluster id) absorb(year)
}
	esttab, replace ar2 b(3) abs compress label ///
		star(* 0.10 ** 0.05 *** 0.01)     
end

*###############################################################################
*###############################################################################
*###############################################################################
*###############################################################################

	global begin_burnin_in = 1970 	// results insensitive to this
	global first_reg_yr = 1980
	global last_reg_yr = 2012	
	global youngest_age 0 			// CITES ALWAYS ZERO IN AGE ZERO YEAR
	global oldest_age 15			// 22 is the max
									// results insensitive to this

capture program drop prepforciteyearregs
program define prepforciteyearregs
/* LOADS A SAMPLE AND PREPS FOR THE REGRESSIONS */
{
	global x l_previous_cites authorcount l_pagecount  
	global var `1'
	
	// save a database (id linked) of paper (control) variables
	preserve
	keep id year jour econjour $var $x 
	sort id
	rename year pub_year
	save "$temp/paper_vars", replace
	restore
	
	// create a paper-year panel containing # cites per year (1950-2012)
	keep id cites_in*
	
	reshape long cites_in_, i(id) j(cite_year)
	
	// link back in the paper (control) variables
	merge m:1 id using "$temp/paper_vars"
	
	// create vars
	g	l_cites_in_ 	= log(1+cites_in_)	
	g	age 			= cite_year - pub_year
	label var l_cites_in "Ann. Cites"
	
	// trim observations
	global youngest_age 0 	// CITES ALWAYS ZERO IN AGE ZERO YEAR
	global oldest_age 15	// 22 is the max
	
	drop if age < $youngest_age // cites here are always 0
	drop if age > $oldest_age  
	
	drop if pub_year < $begin_burnin_in
		
	egen cohort_CROSS_age = group(age pub_year jour)		
}
end

*###############################################################################
*###############################################################################
*###############################################################################
*###############################################################################

cap prog drop BFT_matchyearregs	
prog def BFT_matchyearregs	
qui {
	eststo clear
	sum cite_year 
	local first_reg_yr = `r(min)' + 1
	local last_reg_yr = `r(max)'
	forval yy = `first_reg_yr'/`last_reg_yr' {
		di "`yy'"
		qui		eststo, title("`yy'"): reg y if cite_year <= `yy' , cluster(pair)
		qui		estadd scalar year_`yy' = `yy'
	}	
	distinct id if e(sample)
	local firms_in_test = `r(ndistinct)'
	
	esttab, replace ar2 b(3) abs compress label ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps   keep(_cons)
	
	matrix A = r(coefs)
	matrix list A
	
	local length_v = `last_reg_yr' - `first_reg_yr' + 1
	matrix B = J(`length_v',3,0)
	forval yy = `first_reg_yr'/`last_reg_yr' {
		local i = `yy' - `first_reg_yr' + 1
		
		matrix B[`i',1] = `yy'
		matrix B[`i',2] = A[1, "est`i':b"]
		matrix B[`i',3] = A[1, "est`i':t"]		
	}	
	matrix colnames B = year idcoeff t
	matrix list B
	clear
	svmat B ,	names(col)
	*save "$temp/idcoeffs_and_t", replace
}
	di as result "Firms in reg: " as result `firms_in_test'
	list, noo
end

*###############################################################################
*###############################################################################
*###############################################################################
*###############################################################################

	cap prog drop matchsample_econ
	prog def matchsample_econ
qui {
* THIS PROGRAM IS *NEARLY* IDENTICAL TO matchprem
* Lines that are left aligned are changed or added relative to matchprem 
* The last set of lines in matchprem run an event time regression. Those lines were deleted.
drop if year < 1980

	keep id issue_id jour `1' year citationinfo jour authorcount l_previous_cites l_previous_papers

	* get the month of publications	

	replace citationinfo = proper(citationinfo)
	g month = ""
	foreach m in Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {
		replace month = "`m'" if regexm(citationinfo,"`m'")
	}
	// rfs: 1 per season through 2007
	replace month = "Jan" if month == "" & jour == "RFS" & year < 2008 & regexm(citationinfo, "No. 1")
	replace month = "Apr" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 2")
	replace month = "Jul" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 3")
	replace month = "Oct" if month == "" & jour == "RFS" & year < 2008 &  regexm(citationinfo, "No. 4")
	
	foreach m in Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {
		replace month = "`m'" if regexm(citationinfo,"`m'")
	}
	replace month = "Oct" if issue_id == 1056
	replace month = "Jan" if issue_id == 1071   
	replace month = "Oct" if issue_id == 1158  
	
	tab month, m
	drop if month == ""	// 1 
	
	g month_num = mofd(date(month,"M"))
	tab month_num if month == "Jan"		// jan is 0
	tab month_num if month == "Dec"		// dec is 11
	
	g yearhalf = floor(month_num / 3)
	tab month_num yearhalf
	
	drop month issue_id

	tab authorcount
	
	* match variables: year, yearhalf, prev papers
	* convert variables into strata
		
	sum l_previous_papers, d
if `r(N)' > 0 {
	replace l_previous_papers = . if l_previous_papers == 0
	xtile papers_strata = l_previous_papers, n(3)		// strata: terciles
	replace papers = 0 if papers == .						// strata: 0, terciles if positive
	tab papers
}
else {
g papers_strata = 0
}
	tabstat l_previous_papers, s(min max) by(papers)
		
	* count treatment and control by strata, impose restrictions
	
	****************************************************************************
	egen strata = group(jour year yearhalf papers_strata)
	****************************************************************************
	egen strata_pap = count(`1'), by(strata)
	egen strata_iden = sum(`1'), by(strata)
	g strata_noniden = strata_pap - strata_iden
	
	* note how we lose treatment obs as the number of possible controls is added
	
	distinct id if `1' == 1 									// 436
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 1	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 2	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 3	// 399
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 4	// 381
	distinct id if `1' == 1 & strata_iden > 0 & strata_non >= 5	// 329
		
	* save 	
	
	****************************************************************************
	keep if strata_iden > 0 & strata_non >= 1
	****************************************************************************
	
	preserve 
	keep if `1' == 1
	keep id strata l_previous_cites
	save "$temp/CEM_papers_iden", replace
	restore
	keep if `1' == 0
	keep id strata l_previous_cites
	rename (id l_previous_cites ) ( id_control l_previous_cites_control)
	save "$temp/CEM_papers_non", replace
	
*** build the id/control dataset

	use "$temp/CEM_papers_iden", clear
	count
	joinby strata using "$temp/CEM_papers_non"
	g diff = abs(l_previous_cites - l_previous_cites_control)
sum diff
if `r(N)' > 0 {
	sort id diff	
	egen scoreg = group(id diff)
	by id: egen myrank = rank(scoreg), tr
	distinct id					// a check:
	distinct id if myrank == 1	// this should have same num of distinct; extras on second means we have to break ties
	keep if myrank == 1
}
	set seed 1000
	sample 1, count by(id)
	count						// this should have same num of distinct
	keep id id_control
	save "$temp/CEM_id_control_matches", replace
}
	end
