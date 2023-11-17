/* MakeIndices.do */
* This file creates the indices based on Anderson (2008)
* It is called by DataPrepMasterFile.do
*******************************************************************************

set matsize 11000

foreach vset in $vset_list `extravars' { // loop through all outcome families, including those for the polarization robustness check
	if strpos("`vset'","secondary")!=0 { // secondary outcome family doesn't have an index
		continue
	}
	else {
		mkmat `varset_`vset'_idx' if sample_main==1, matrix(tempm) // create matrix of outcome variables in given index
		mata:	tempm=st_matrix("tempm") // read into Mata
		
		mata: R =  cols(tempm) // obtain number of items in index
		mata: R

		* create matrix that indicates non-missing values in outcome variables
		mata: tempm
		mata: idk = 0*tempm // make all non-missing values zero
		mata: idk
		mata: _editvalue(idk, 0, 1) // turn all non-missings into ones
		mata: idk
		mata: _editmissing(idk,0) // turn all missings into zeros
		mata: idk
		
		* replace all missing outcome variabel values with zeros
		mata: _editmissing(tempm,0)
		mata: tempm
		
		* obtain inverse covariance matrix
		mata: invcov = invsym(tempm'*tempm)
		mata: invcov
		
		* obtain weights
		mata: weights = rowsum(invcov) // weights are sum of rows of inv. cov. matrix
		mata: weights
		mata : st_matrix("factors",weights') // put weight vector back into Stata
		matrix list factors
		svmat double factors, names(idx) // and give elements of vector the prefix "idx"
		
		* put weights back into Stata
		mata : st_matrix("numvars",R) // read number of items in index into Stata
		matrix list numvars
		svmat double numvars, names(nvars) // call it nvars
		local max=nvars1[1] // transform it into local
		forvalues x=1/`max' { // look through all weights
			replace idx`x'=idx`x'[1] if idx`x'==. // and fill them in for all observations
		}
		
		* set some vectors up for later
		gen normalizer=0
		gen normalizer_b=0
		gen index_`vset'=0
		gen index_`vset'_b=0		
		
		foreach time in "" _b { // loop through endline and baseline
			local x=1
			foreach yvar in `varset_`vset'_idx' { // loop through all normalized variables in the index
				replace normalizer`time'=normalizer`time'+idx`x' if `yvar'`time'!=. & `yvar'`time'_imp==0 // obtain sum of weights of all non-missing for a given person
				replace index_`vset'`time'=index_`vset'`time'+idx`x'*`yvar'`time' if `yvar'`time'!=.  & `yvar'`time'_imp==0 // add the current variable times its weight
				local x = `x'+1
			}
			replace index_`vset'`time' = index_`vset'`time'/normalizer`time' // divide index value by normalizer for each person. if all index components are missing for an observation, then normalizer = 0 and the index then becomes missing.
			drop normalizer`time'
		}
					
		drop idx* nvars1  // drop old variables to prepare for next item in loop
		mata: mata clear  // drop Mata elements
		clear matrix // drop all Stata matrixes
		
		* label index for all but the polarization robustness check indexes
		if "`vset'"=="sub_time" | "`vset'"=="sub_news" | "`vset'"=="polarize" | "`vset'"=="voting" | "`vset'"=="news" | "`vset'"=="swb" | "`vset'"=="social" | "`vset'"=="fbopinions" | "`vset'"=="postexp"  {
			label var index_`vset' "{bf:`indexname_`vset''}"
		}
	}

}

foreach var_excl in `varset_polarize' { // label the polarization robustness check indexes
	local label: variable label `var_excl'
	label var index_`var_excl' "`label'"
}

** Standardize indices
foreach vset in $vset_list `extravars' {
	if "`vset'" != "secondary" {
		sum index_`vset' if T ==0 & sample_main==1
		replace index_`vset'_b = (index_`vset'_b-r(mean))/r(sd)
		replace index_`vset' = (index_`vset'-r(mean))/r(sd)
	}
}
