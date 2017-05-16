* THIS IS AN EXAMPLE FILE TO SHOW MY WORK [    ] DENOTES REMOVED CHARACTERS FOR CONFIDENTIALITY

*Author: Frank Cousin

* Importing [    ] files
*import delimited using c:\[    ].txt, delim("|") clear


global state_type "[    ]"


clear all
cd c:\[    ]\data\\${state_type}

*********************************
* TO CREATE AN APPENDED FILE OF ALL STATE_TYPE COMPARISON FILES
* creating txt list of file names, importing and then append using variable values
*********************************
! dir *.dta > filelist.txt

import delimited using c:\[    ]\data\\${state_type}\filelist.txt, delim("\t") clear

gen filesnames = regexs(1) if regexm(v1, "[    ]_(.*).dta")
drop v1
drop in 1/5
drop in -2/-1

levelsof filesnames, local(filesnames)

	foreach x in `filesnames' {
		append using [    ]_`x'.dta, force
	}
drop if filesnames != "" 
drop filesnames
save appended_compare_all_data_${state_type}.dta, replace
use appended_compare_all_data_${state_type}.dta, replace

*drop if regexm(filename,"^[    ]")
*br if regexm(filename,"^[    ]") // there are 660,000 obs of [    ]
*I'm not sure if they should be taken out or not?
* 16,760 [    ] in the appended dataset
* I've taken out the duplicates and recalculated proportions using new n

**************************************
global [    ]1 "[    ]1" // [    ]
global [    ]2 "[    ]2" // [    ]
**************************************
* Adding flags for discrepancy type
**************************************

	gen flag30 = 1 if rec_${[    ]1} == 3 & rec_${[    ]2} == 0 | rec_${[    ]2} == 3 & rec_${[    ]1} == 0
	recode flag30 (.=0)
	gen flag10 = 1 if rec_$[    ]1 == 1 & rec_$[    ]2 == 0 | rec_$[    ]2 == 1 & rec_$[    ]1 == 0
	recode flag10 (.=0)
	gen flag31 = 1 if rec_$[    ]1 == 3 & rec_$[    ]2 == 1 | rec_$[    ]2 == 3 & rec_$[    ]1 == 1
	recode flag31 (.=0)
	gen n = 1
	gen any_discrep = flag31 + flag30 + flag10
	
	
display " "

*****************************
* working towards collapse
*****************************

*br uniqid_sample flag3 flag30

* flag all observations of individuals that have at least one flag with a 3
bysort uniqid_sample : egen ind_flag30 = max(flag30)
bysort uniqid_sample : egen ind_flag31 = max(flag31)
bysort uniqid_sample : egen ind_flag10 = max(flag10)

*keep if ind_flag30 ==1 | ind_flag31 ==1 //| ind_flag10 ==1
// could just do egen (above), bysort will allow for more flexability later
* for all use of threes, not just ind w/ discreps

* flag if each [    ] used a 3 on this individual
gen threeflag1 = 0
recode threeflag1 (0=1) if rec_[    ]1 ==3
gen threeflag2 = 0
recode threeflag2 (0=1) if rec_[    ]2 ==3

* sum the number of threes used on each individual by [    ]
bysort uniqid_sample : egen [    ]1_3total = total(threeflag1)
bysort uniqid_sample : egen [    ]2_3total = total(threeflag2)

sort total

* collapse by [    ]
collapse (max) [    ]1_3total [    ]2_3total (sum) flag3* flag10 (first) filename, by(uniqid_sample)

gen obs_n = _N

gen successful3s = [    ]1_3total + [    ]2_3total - flag30 - flag31
gen three_agreements = ([    ]1_3total + [    ]2_3total - flag30 - flag31) /2

sort three_agreements


count if [    ]1_3total >= 2 & [    ]2_3total >= 2 & flag30 >=1 // x uniqid's have at least one 3 from each [    ], and at least one 3-0 discrep
display _N
gen p_33and30 = r(N)/ _N

count if [    ]1_3total == 0 & [    ]2_3total >= 2 & flag30 >=1 | [    ]1_3total == 2 & [    ]2_3total >= 0 & flag30 >=1 // x uniqid's have at least one 3 from each [    ], and at least one 3-0 discrep
display _N
gen p_30and30 = r(N)/ _N

hist flag30 if [    ]1_3total >=2 & [    ]2_3total >=2 & flag30 >=1, freq width(1) xtitle("# of 3-0 discreps") title("Frequency of 3-0 discreps per [    ]") subtitle("when each [    ] used at least two 3s")
graph save flag30_freq_${state_type}.gph, replace
tab flag30 if [    ]1_3total >=2 & [    ]2_3total >=2 & flag30 >=1
* [    ]1 and 2 both used threes, at least one 3-0 discrep

count if flag30 >=1
gen p_30discrepancy = r(N)/ _N
count if flag31 >= 1
gen p_31discrepancy = r(N)/ _N
count if flag10 >= 1
gen p_10discrepancy = r(N)/ _N
count if flag30 >=1 | flag31 >= 1 | flag10 >= 1
gen p_discrepancy = r(N)/ _N

tabstat flag*, stats(sum)

gen state_type = regexs(1) + regexs(2) if regexm(filename, "^([A-Z][A-Z]_).*_([a-z][a-z])$")
order state_type, first

save collapsed_compare_${state_type}.dta, replace
use collapsed_compare_${state_type}.dta, replace


* there are [    ] observations in [    ]??? append all collapsed files and search for dups
clear all
set obs 1
gen x=.
foreach x in "[    ]" "[    ]" "[    ]" "[    ]" {
	cd c:\[    ]\data\\`x'
	append using collapsed_compare_`x'.dta, // force
}
drop in 1
drop x

sort filename uniqid_sample
duplicates report filename uniqid_sample
duplicates drop filename uniqid_sample, force // 16758 observations deleted
* they all look like [    ]


*drop all proportions and obs_n vars
drop p_*
drop obs_n

*recalculating after dropping dups
bysort state_type : egen obs_n = count(state_type)


gen [    ]30flag = 1 if flag30 >= 1
gen [    ]31flag = 1 if flag31 >= 1
gen [    ]10flag = 1 if flag10 >= 1
gen [    ]discrepflag = 1 if flag30 >= 1 | flag31 >= 1 | flag10 >= 1

bys state_type : egen n_30_[    ] = total([    ]30flag)
bys state_type : egen n_31_[    ] = total([    ]31flag)
bys state_type : egen n_10_[    ] = total([    ]10flag)
bys state_type : egen n_discrep_[    ] = total([    ]discrepflag)

gen p_30 = n_30_[    ] / obs_n
gen p_31 = n_31_[    ] / obs_n
gen p_10 = n_10_[    ] / obs_n
gen p_discrep = n_discrep_[    ] / obs_n

save c:\[    ]\data\comprehensive_compare.dta, replace
use c:\[    ]\data\comprehensive_compare.dta, replace


gen diff_3_usage = [    ]_3total - [    ]2_3total
gen abs_diff = ((diff_3_usage)^2)^(1/2)

* figures
hist flag30 if [    ]1_3total >=2 & [    ]2_3total >=2 & flag30 >=1, by(state_type) freq width(1)
hist abs_diff if [    ]1_3total >=2 & [    ]2_3total >=2 & flag30 >=1 | [    ]1_3total >=2 & [    ]2_3total >=2 & flag31 >=1, freq by(state_type)
twoway (hist [    ]1_3total, freq start(0) width(1)) (hist [    ]2_3total, freq start(0) width(1) fcolor(none) lcolor(black)) if flag30 >=1 & state_type== "[    ]" |  flag31 >=1 & state_type== "[    ]" | flag30 >=1 & state_type== "[    ]" |  flag31 >=1 & state_type== "[    ]" , by(state_type) title($stateyear $linktype "three usage per [    ]") xtitle("Number of threes used") legend(order(1 "$[    ]1" 2 "$[    ]2"))


save discrep3data.dta, replace


collapse (sum) [    ]1_3total [    ]2_3total flag* successful3s three_agreements (max) n_* p_* obs_n, by(state_type)

*order p*, after(flag10)

export excel using c:\[    ]\data\allfile_v2.xlsx, replace firstrow(variables)
