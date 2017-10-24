* [     ] denotes redacted characters for confidentiality

* advertising report * google [ product line ]
*mkdir /users/frankcousin/desktop/protrainings/ad_reports
cd /[file path]
global downloads /[file path]

* all data came in one file. don't need the following
/*
global old ____
global new ____

foreach x in $old $new {
import delimited ${downloads}/`x'.csv, delim(",") varnames(1) clear
	destring cost, replace ignore("$")
	
	* t vars
	gen date2 = date(day, "MDY")
	format date2 %td
	gen month2 = mofd(date2)
	format month2 %tmmonYY
	gen week2 = wofd(date2)
	format week2 %tw
	
	save `x'.dta, replace}
*
clear
use ${old}.dta, replace
append usings ${new}.dta
*/

import delimited ${downloads}/ad_report_[productline].csv, delim(",") varnames(1) clear
destring cost avgcpc avgcost, replace ignore("$")
destring clicks impressions, replace ignore(",") // idk why these are string
destring ctr convrate searchimprshare, replace percent ignore("%-<")
* these need to be destringed: cost$, clicks, impressions, ctr, avgcpc$, avgcost$, convrate, searchimprshare

* t vars
gen date2 = date(day, "MDY")
format date2 %td
gen month2 = mofd(date2)
format month2 %tmmonYY
gen week2 = wofd(date2)
format week2 %tw
gen costpconv = cost/ conversions
gen cashv = conversions * [ cost ]
gen lifetv = (conversions + (conversions *[rate])) * [cost]
* new dichotomous var
gen new = regexm(campaign, ".*[product line].*")
drop if impressions == 0
tab campaign new

**************************
******** drops ***********
**************************
drop if impressions > 25000
*** designated drop of DVD
drop if inlist(campaign, "[irrelevant product]", "[irrelevant product2]")
*** start month drop
drop if month2 < tm(2013m9)
*drop if month2 < tm(2014m7)
*** setting ndate from tabstat
tabstat date2, stats(min) save
matrix min = r(StatTotal)
gen ndate = date2 - min[1,1]
tab new
**************************

tabstat ctr [fweight=impressions], by(new) stats(mean)
tabstat avgcpc convrate [fweight=clicks], by(new) stats(mean)
tabstat costpconv, stats(mean p50 min max n) by(new)
tabstat cashv, stats(sum) by(new)
tabstat lifetv, stats(sum) by(new)
twoway (scatter impressions date2 if new==0) (scatter impressions date2 if new==1)
// [there was an error by AdWords for extreme values]
gen newXdate = new * ndate
gen newXcost = new * cost

* model 1. testing whether the campaigns have a different avg of conversions, controlling for impressions
reg conversions i.new impressions, robust

* model 2. testing whether the campaigns have diff avg of conversions controlling for impressions and month
reg conversions i.new ndate impressions, robust

* model 3. testing difference between campaigns
reg conversions i.new ndate newXdate impressions, robust

* model 4. focus of this is on cost differences
reg conversions i.new cost newXcost ndate newXdate, robust

* model 5. focus is conversions over cost amounts
reg conversions i.new cost newXcost, robust // which is more effecient depends on cost amount and inclusion/exclusion of outliers
twoway (scatter conversions cost if new ==0, msize(.1)) (scatter conversions cost if new==1, msize(.1)) (lfit conversions cost if new==0) (lfit conversions cost if new==1), legend(label(3 "[old campaign]") label(4 "[new campaign]") label(1 "day") label(2 "day")) ytitle("Predicted Conversions") xtitle("Spending per Day") xlabel(0(100)400 42.34)
twoway (lfit conversions cost if new==0) (lfit conversions cost if new==1), legend(label(1 "[old campaign]") label(2 "[new campaign]")) ytitle("Predicted Conversions") xtitle("Spending per Day") xlabel(0(100)400 42.34)

margins, predict() at(new= 0 cost== 50 newXcost= 0)
margins, predict() at(new= 1 cost== 50 newXcost= 50)

* follow-up hypothesis tests
test newXdate - ndate = 0 // trends (slopes) are not significantly different. can also use: newXmonth = month2
//test (1.new + newXmonth) - (0.new + month2) = 0 // trends and intercept are not significantly different
test 1.new + newXdate = _cons + ndate // trends and intercept are not significantly different
* model 2. creating yhat
reg conversions i.new ndate impressions
predict yhat2, xb
twoway (lfit yhat2 date2 if new==0) (lfit yhat2 date2 if new==1) , tlabel(,format(%tdMondd-YY))
* model 3. creating yhat
reg conversions i.new ndate newXdate impressions
matrix re = e(b)
predict yhat3, xb
twoway (scatter impressions date2 if new==0, msize(.1)) (scatter impressions date2 if new ==1, msize(.1)) (lfit yhat3 date2 if new==0) (lfit yhat3 date2 if new==1), tlabel(,format(%tdMondd-YY)) legend(label(1 "[old campaign] impressions") label(2 "[new campaign] impressions"))
twoway (scatter conversions date2 if new==0, msize(.1)) (scatter conversions date2 if new ==1, msize(.1)) (lfit yhat3 date2 if new==0) (lfit yhat3 date2 if new==1), tlabel(,format(%tdMondd-YY)) legend(label(1 "[old campaign] conversions") label(2 "[new campaign] conversions"))
twoway (scatter yhat3 date2 if new==0) (scatter yhat3 date2 if new==1), tlabel(,format(%tdMondd-YY))

* estimating y at this point in time. impressions are zero bc they are constant between new and old
margins, predict() at(new=0 ndate=1921 newXdate=0 impressions=0) // [estimated] conv
margins, predict() at(new=1 ndate=1921 newXdate=1921 impressions=0) // [estimated] conv

* finding the area between the lines at ndate 1860 to 1921 (august 4 to october 4)
local c =0
forvalues i = 1860/1921 {
	margins, predict() at(new=0 ndate=`i' newXdate=0 impressions=0)
	matrix list r(b)
	matrix y = r(b)
	local c = `c' + y[1,1]
}
display `c' // [calculated number] and some change


local d =0
forvalues i = 1860/1921 {
	margins, predict() at(new=1 ndate=`i' newXdate=`i' impressions=0)
	matrix list r(b)
	matrix y = r(b)
	local d = `d' + y[1,1]
}
display `d'
display = `c' - `d'

global start 1860
global end 1921

display = (re[1,6]/2 * $end ) + (re[1,3]/2 * $end^2 )
display = (re[1,6]/2 * $start ) + (re[1,3]/2 * $start^2 )

local c0 = (re[1,6]/2 * $end ) + (re[1,3]/2 * $end^2 )
local c1 = (re[1,6]/2 * $start ) + (re[1,3]/2 * $start^2 )

local c2 = (((re[1,2]+ re[1,6])/2) * $end) + ((re[1,3] + re[1,4])/2 * $end^2 )
local c3 = (((re[1,2]+ re[1,6])/2) * $start) + ((re[1,3] + re[1,4])/2 * $start^2 )
display = (((re[1,2]+ re[1,6])/2) * $end) + ((re[1,3] + re[1,4])/2 * $end^2 )
display = (((re[1,2]+ re[1,6])/2) * $start) + ((re[1,3] + re[1,4])/2 * $start^2 )

local s0 = `c0' - `c1'
local s1 = `c2' - `c3'
local s3 = `s0' - `s1'
display = `c0' - `c1'
display = `c2' - `c3'
display = `s0' - `s1'
display `s3'
// with dropping impressions [from advertising company error] space between is [sales difference for period]
// without dropping impressions [from advertising company error] space between is [sales difference for period]


save daydata.dta, replace
* weight ********************
*** rates: calculated rates are ctr, cpc, avgcosts, convrate, searchimprshare

* impressions
*** ctr

* clicks
*** avgcpc
*** avg cost
*** convrate

* estimated number of impressions we were eligible to receive ??? what is that
*** searchimprshare
*****************************
use daydata.dta, replace
preserve
collapse (sum) cost clicks impressions conversions, by(new month2)
sort month2 new
save sums.dta, replace
restore

preserve
collapse (sum) impressions (mean) ctr avgposition [fweight=impressions], by(new month2)
sort month2 new
save impweighted.dta, replace
restore

preserve
collapse (sum) clicks (mean) avgcpc convrate [fweight= clicks], by(new month2)
sort month2 new
save clickweighted.dta, replace
restore
*********
use sums.dta, replace
sort month2 new
merge 1:1 month2 new using impweighted.dta, gen(m1)
merge 1:1 month2 new using clickweighted.dta, gen(m2)
gen costpconv = cost/ conversions
gen cashv = conversions * 19.95
gen lifetv = (conversions + (conversions *.5)) * 19.95


tabstat ctr [fweight=impressions], by(new) stats(mean)
tabstat avgcpc convrate [fweight=clicks], by(new) stats(mean)

* choose a visual
twoway (line convrate month2 if new==1, yaxis(1)) (line convrate month2 if new==0, yaxis(1)) (line ctr month2 if new ==1, yaxis(1)) (line ctr month2 if new==0, yaxis(1))
twoway (line convrate month2 if new==1, yaxis(1)) (line convrate month2 if new==0, yaxis(1)) (line ctr month2 if new ==1, yaxis(2)) (line ctr month2 if new==0, yaxis(2))
twoway (line impressions month2 if new==1) (line impressions month2 if new==0) (line clicks month2 if new==1) (line clicks month2 if new==0) (line conversions month2 if new==1) (line conversions month2 if new==0)
twoway (line clicks month2 if new==1) (line clicks month2 if new==0) (line conversions month2 if new==1) (line conversions month2 if new==0)
twoway (line conversions month2 if new==1) (line conversions month2 if new==0)
* [note]
twoway (lfitci conversions month2 if new==0) (lfitci conversions month2 if new==1) (scatter conversions month2), tlabel(,format(%tmmonYY)) xtitle(month) ytitle(conversions)

export excel using [product line]_report.xls, replace firstrow(variables)

***********************
***********************
* Advertising report Bing
import delimited using ${downloads}/ad_report_[product line]1.csv, delim(",") varnames(9) clear
drop in -1 // dropping "rights reserved" line
rename (gregoriandate spend conversionrate averagecpc impressionshare) (day cost convrate avgcpc searchimprshare)
gen cid = regexs(1) if regexm(campaignid, "\[([0-9]*)\]")
drop campaignid
destring ctr convrate searchimprshare, replace percent ignore("%")
gen new = regexm(campaignname, ".*[new campaign name].*")

*t vars
gen date2 = date(day, "YMD")
format date2 %td
gen month2 = mofd(date2)
format month2 %tm
tabstat date2, stats(min) save
matrix min = r(StatTotal)
gen ndate = date2 - min[1,1]
drop if impressions >20000

twoway (scatter impressions date2 if new==0) (scatter impressions date2 if new==1)
**

// what's happening with [error caused by advertising company] July
gen newXdate = new * ndate
gen newXcost = new * cost
* model 1. testing whether the campaigns have a different avg of conversions, controlling for impressions
reg conversions i.new impressions, robust
* model 2. testing whether the campaigns have diff avg (intercept) of conversions, controlling for impressions
reg conversions i.new ndate impressions, robust
* model 3. allowing each line to vary intercepts and trends
reg conversions i.new ndate newXdate impressions, robust
* model 4. focus of this is on cost differences
reg conversions i.new cost newXcost ndate newXdate, robust
* follow-up hypothesis tests
test newXdate - ndate = 0 // trends (slopes) are not significantly different. can also use: newXmonth = month2
//test (1.new + newXmonth) - (0.new + month2) = 0 // trends and intercept are not significantly different
test 1.new + newXdate = _cons + ndate // trends and intercept are not significantly different
* model 2. creating yhat
reg conversions i.new ndate impressions, robust
predict yhat2, xb
twoway (lfit yhat2 date2 if new==0) (lfit yhat2 date2 if new==1) , tlabel(,format(%tdMondd-YY))
* model 4. creating yhat
reg conversions i.new cost newXcost ndate newXdate, robust

* model 5. focus is conversions over cost amounts
reg conversions i.new cost newXcost, robust // this which is more effecient depends on cost amount and inclusion/exclusion of outliers
twoway (scatter conversions cost if new ==0, msize(.1)) (scatter conversions cost if new==1, msize(.1)) (lfit conversions cost if new==0) (lfit conversions cost if new==1), legend(label(3 "[old campaign]") label(4 "[old campaign]") label(1 "day") label(2 "day")) ytitle("Predicted Conversions") xtitle("Spending per Day")
twoway (lfit conversions cost if new==0) (lfit conversions cost if new==1), legend(label(1 "[old campaign]") label(2 "[new campaign]")) ytitle("Predicted Conversions") xtitle("Spending per Day")

margins, predict() at(new= 0 cost== 50 newXcost= 0)
margins, predict() at(new= 1 cost== 50 newXcost= 50)


matrix re = e(b)
predict yhat4, xb
twoway (scatter impressions date2 if new==0, msize(.1)) (scatter impressions date2 if new ==1, msize(.1)) (lfit yhat4 date2 if new==0) (lfit yhat4 date2 if new==1), tlabel(,format(%tdMondd-YY)) legend(label(1 "[old campaign] impressions") label(2 "[new campaign] impressions"))
twoway (scatter conversions date2 if new==0, msize(.1)) (scatter conversions date2 if new ==1, msize(.1)) (lfit yhat4 date2 if new==0) (lfit yhat4 date2 if new==1), tlabel(,format(%tdMondd-YY)) legend(label(1 "[old campaign] conversions") label(2 "[new campaign] conversions")) ytitle("Predicted Conversions") xtitle("Date")
twoway (scatter yhat4 date2 if new==0) (scatter yhat4 date2 if new==1), tlabel(,format(%tdMondd-YY))

* estimating y at this point in time. impressions are zero bc they are constant between new and old
margins, predict() at(new=0 ndate=1921 newXdate=0 impressions=0) // [estimate] conv
margins, predict() at(new=1 ndate=1921 newXdate=1921 impressions=0) // [estimate] conv

* finding the area between the lines at ndate 1860 to 1921 (august 4 to october 4)
local c =0
forvalues i = 1860/1921 {
	margins, predict() at(new=0 ndate=`i' newXdate=0 impressions=0)
	matrix list r(b)
	matrix y = r(b)
	local c = `c' + y[1,1]
}
display `c' // [calculated number] and some change


local d =0
forvalues i = 1860/1921 {
	margins, predict() at(new=1 ndate=`i' newXdate=`i' impressions=0)
	matrix list r(b)
	matrix y = r(b)
	local d = `d' + y[1,1]
}
display `d'
display = `c' - `d'

global start 1860
global end 1921

display = (re[1,6]/2 * $end ) + (re[1,3]/2 * $end^2 )
display = (re[1,6]/2 * $start ) + (re[1,3]/2 * $start^2 )

local c0 = (re[1,6]/2 * $end ) + (re[1,3]/2 * $end^2 )
local c1 = (re[1,6]/2 * $start ) + (re[1,3]/2 * $start^2 )

local c2 = (((re[1,2]+ re[1,6])/2) * $end) + ((re[1,3] + re[1,4])/2 * $end^2 )
local c3 = (((re[1,2]+ re[1,6])/2) * $start) + ((re[1,3] + re[1,4])/2 * $start^2 )
display = (((re[1,2]+ re[1,6])/2) * $end) + ((re[1,3] + re[1,4])/2 * $end^2 )
display = (((re[1,2]+ re[1,6])/2) * $start) + ((re[1,3] + re[1,4])/2 * $start^2 )

local s0 = `c0' - `c1'
local s1 = `c2' - `c3'
local s3 = `s0' - `s1'
display = `c0' - `c1'
display = `c2' - `c3'
display = `s0' - `s1'
display `s3'



reg conversions i.new cost newXcost ndate newXdate if date2 <= td(15may2015) | date2 >= td(1jan2016) & date2 <= td(15may2016) | date2 >= td(1jan2017) & date2 <= td(15may2017), robust

/*
xpose, clear varname
order _varname, before v
reshape wide, i(month2) j(new) */

/*
* what would controlling for the month do??
*** it would allow me to test whether the trend is different between the two periods
gen newXmonth = new * month2
* model 1. testing whether the campaigns have a different avg of conversions, controlling for impressions
reg conversions i.new impressions
* model 2. testing whether the campaigns have diff avg of conversions controlling for impressions and month
reg conversions i.new month2 impressions
* model 3. testing difference between campaigns
reg conversions i.new month2 newXmonth impressions
* follow-up hypothesis tests
test newXmonth - month2 = 0 // trends (slopes) are not significantly different. can also use: newXmonth = month2
//test (1.new + newXmonth) - (0.new + month2) = 0 // trends and intercept are not significantly different
test 1.new + newXmonth = _cons + month2 // trends and intercept are not significantly different
* model 2. creating yhat
reg conversions i.new month2 impressions
predict yhat2
twoway (lfit yhat2 month2 if new==0) (lfit yhat2 month2 if new==1)
* model 3. creating yhat
reg conversions i.new month2 newXmonth impressions
predict yhat3
twoway (lfit yhat3 month2 if new==0) (lfit yhat3 month2 if new==1)
*/
