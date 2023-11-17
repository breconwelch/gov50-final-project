********************************************************************************
* DataPrepMasterFile.do
*
* Clean and merges all datasets
********************************************************************************

********************************************************************************
* Define environment
********************************************************************************
adopath + "../lib/ado"
yaml read YAML using "../config.yaml"
yaml read YAML_user using "../config_user.yaml"
include "../lib/stata/SetGlobals.do"

global data "input/data"
global confidential_main "input/confidential_main"
global confidential_L2 "input/confidential_L2"
global output = "output"

yaml global start_experiment = YAML.metadata.dates.start_experiment
yaml global midline = YAML.metadata.dates.midline
yaml global endline = YAML.metadata.dates.endline

*****************************************************
****** Import, clean and reshape SMS responses ******
*****************************************************
*import timezone information
import excel using "$data/us_timezones.xlsx", clear firstrow
moss state, match("\((.*)\)") regex
drop state _count _pos
ren _match1 state
moss timezone, match("\((.*)\)") regex
drop timezone
ren _match1 timezone
keep state timezone
tempfile statetime
save `statetime'

*extract state information from baseline
use "$confidential_main/baseline_anonymous.dta", clear
ren *, lower
keep id state state_prescreen

* fill in those with missing state abbreviations for those who qualified for midline but whose Zip codes weren't found
replace state="MS" if state_prescreen=="Mississippi" & state==""
replace state="WI" if state_prescreen=="Wisconsin" & state==""
replace state="UT" if state_prescreen=="Utah" & state==""
replace state="CA" if state_prescreen=="California" & state==""
replace state="RI" if state_prescreen=="Rhode Island" & state==""
replace state="FL" if state_prescreen=="Florida" & state==""
replace state="IA" if state_prescreen=="Iowa" & state==""
replace state="OH" if state_prescreen=="Ohio" & state==""
replace state="MA" if state_prescreen=="Massachusetts" & state==""
replace state="IN" if state_prescreen=="Indiana" & state==""
replace state="AL" if state_prescreen=="Alabama" & state==""
replace state="MI" if state_prescreen=="Michigan" & state==""
drop state_prescreen

*merge the two
merge m:1 state using `statetime'
drop _merge

tempfile tz
save `tz'

***import SMS responses
use "$confidential_main/sms_anonymous.dta", clear
*destring some variables
destring *_sms mood*, replace
ren ID id

*merge in time zone info
merge m:1 id using `tz'
drop if _merge==2
drop _merge

*create date variables
gen datetime_sent = clock(startdate, "YMDhms")
gen datetime_reply = clock(enddate, "YMDhms")
gen day_sent = dofc(datetime_sent)
gen day_reply = dofc(datetime_reply)

*convert to local time
foreach v in datetime_sent datetime_reply {
	replace `v'=`v'+3*60*60*1000 if timezone=="EST"
	replace `v'=`v'+2*60*60*1000 if timezone=="CST"
	replace `v'=`v'+1*60*60*1000 if timezone=="MST"
	replace `v'=`v'-(1*60*60*1000) if timezone=="AKST"
	replace `v'=`v'-(2*60*60*1000) if timezone=="HST"
}
* Arizona and Hawaii do not use Daylight Savings Time
replace datetime_sent=datetime_sent-(1*60*60*1000) if (timezone=="HST" & day_sent<21492) | (state=="AZ" & day_sent<21492)
replace datetime_reply=datetime_reply-(1*60*60*1000) if (timezone=="HST" & day_sent<21492) | (state=="AZ" & day_sent<21492)

drop day_sent day_reply
gen day_sent = dofc(datetime_sent)
gen day_reply = dofc(datetime_reply)

gen hour_sent = hh(datetime_sent)
gen hour_reply = hh(datetime_reply)
sort day_sent
gen day_numeric_sent = day_sent-$start_experiment
tab day_numeric_sent
sort day_reply
gen day_numeric_reply = day_reply-$start_experiment
drop if day_sent==. & could_not_distribute!=1

* switch the scale for lonely_sms for all sms before October 7th (note that we exclude them from the analysis)
destring lonely_sms, replace
replace lonely_sms=(lonely_sms-11)*(-1) if day_sent<21464

* consolidate the happiness question variables into one
replace happy_sms=day1_happy_sms if happy_sms==. & lonely_sms==. & mood1==. & mood2==. & mood3==. & mood4==. & mood5==. & mood6==.
drop day1_happy_sms

* consolidate the mood questions into one
lab def mood 1 "happy" 2 "angry" 3 "worried" 4 "sad" 5 "loving/tender" 6 "bored" 7 "lonely/left out" 8 "shameful/guilty" 9 "absorbed in doing something worthwhile" 10 "other positive feeling" 11 "other negative feeling" 12 "other neutral feeling"

gen mood = mood1
replace mood=1 if mood2==1 | mood3==4 | mood4==7 | mood5==7 | mood6==4
replace mood=2 if mood2==2 | mood3==5 | mood4==8 | mood5==8 | mood6==5
replace mood=3 if mood2==3 | mood3==6 | mood4==9 | mood5==9 | mood6==6
replace mood=4 if mood2==7 | mood3==1 | mood4==1 | mood5==4 | mood6==7
replace mood=5 if mood2==8 | mood3==2 | mood4==2 | mood5==5 | mood6==8
replace mood=6 if mood2==9 | mood3==3 | mood4==3 | mood5==6 | mood6==9
replace mood=7 if mood2==4 | mood3==7 | mood4==4 | mood5==1 | mood6==1
replace mood=8 if mood2==5 | mood3==8 | mood4==5 | mood5==2 | mood6==2
replace mood=9 if mood2==6 | mood3==9 | mood4==6 | mood5==3 | mood6==3
replace mood=10 if mood2==10 | mood3==10 | mood4==10 | mood5==10 | mood6==10
replace mood=11 if mood2==11 | mood3==11 | mood4==11 | mood5==11 | mood6==11
replace mood=12 if mood2==12 | mood3==12 | mood4==12 | mood5==12 | mood6==12

lab val mood mood
ren mood mo
drop mood*
ren mo mood_sms

* positive emotion indicator for mood variable
gen pos_emotion_sms=.
replace pos_emotion_sms=1 if mood_sms==1 | mood_sms==5 | mood_sms==9 | mood_sms==10
replace pos_emotion_sms=0 if mood_sms!=. & pos_emotion_sms!=1
*replace pos_emotion_sms=-1 if pos_emotion_sms!=1 & mood_sms!=12 & mood_sms!=.

keep id day_* datetime* hour_* timezone happy_sms lonely_sms mood_sms pos_emotion_sms could_not_distribute undeliv_phone not_supp_interac_svys
order id timezone day_numeric_sent day_numeric_reply happy_sms lonely_sms mood_sms pos_emotion_sms could_not_distribute undeliv_phone not_supp_interac_svys
sort id day_numeric_sent could_not_distribute

* drop the duplicate bounce messages
duplicates drop id day_numeric_sent could_not_distribute if could_not_distribute==1, force
* drop the irrelevant bounce messages (i.e. when the "thank you for responding" message got bounced)
sort id day_numeric_sent could_not_distribute
drop if id==id[_n-1] & day_numeric_sent==day_numeric_sent[_n-1] & could_not_distribute==1

* re-orient lonely sms variable so that larger values mean more "positive" (i.e. less lonely)
replace lonely_sms=(lonely_sms-11)*(-1)

*** collapse into pre- and post-midline within-person averages
* exclude first 3 days of loneliness responses because we flipped the scale
gen temp=lonely_sms
replace lonely_sms=. if day_sent<21464

* exclude day of midline and endline
gen pre=.
replace pre=1 if day_numeric_sent<14
gen post=.
replace post=1 if day_numeric_sent>15 & day_numeric_sent<42 & day_numeric_sent!=.

*compute averages
foreach v in happy lonely pos_emotion {
	gen temp1 = `v'_sms*pre
	gen temp2 = `v'_sms*post
	bysort id: egen `v'_sms_summary_b=mean(temp1)
	bysort id: egen `v'_sms_summary=mean(temp2)
	drop temp1 temp2
}

replace lonely_sms=temp
drop temp pre post

* change to wide format (1 row per person)
reshape wide day_reply day_numeric_reply day_sent datetime* hour_* happy_sms lonely_sms mood_sms pos_emotion_sms could_not_distribute undeliv_phone not_supp_interac_svys, i(id) j(day_numeric_sent)

tempfile tempsms
save `tempsms' // will be merged to rest of data below


*********************************************************
**  Import and clean voting data  **
*********************************************************

use "$confidential_L2/voting_anonymous.dta", clear

* Generate a variable that describes the quality of the merge
gen merge_quality=0
replace merge_quality=1 if mergetype=="fmlbz"
replace merge_quality=2 if mergetype=="fmlbs"
replace merge_quality=3 if mergetype=="fmlz"
replace merge_quality=4 if mergetype=="flbz"
replace merge_quality=5 if mergetype=="flz"
replace merge_quality=6 if mergetype=="fmls"
replace merge_quality=7 if mergetype=="fmlb"
replace merge_quality=8 if mergetype=="flb"

* Keep the best merge quality for each individual
bysort id: egen min_merge_q=min(merge_quality)
keep if merge_quality==min_merge

* Generate a variable that tells us whether the merge is unique
gen unique_merge=0
gen ones=1
bysort id: egen temp=total(ones)
replace unique_merge=1 if temp==1
drop ones temp

* For the peopole with multiple merges, take the average across all the people they are matched with
bysort id voteid: drop if _n>1 // drop duplicate merges
bysort id: egen vote_2016=mean(general_2016)
bysort id: egen vote_2018=mean(general_2018)
bysort id: egen donated=mean(fec_donated)

* Drop variables that are not of interest and duplicates at the ID level
keep id mergetype merge_quality unique_merge vote_2016 vote_2018 donated
duplicates drop id, force

tempfile tempvoting
save `tempvoting'

*********************************************************
**  Import and clean data on reasons for reactivation  **
*********************************************************
use "$confidential_main/reasons_reactivation_anonymous.dta", clear

destring id, replace

* clean up reason for reactivation variable
gen reac_reason=.
replace reac_reason=1 if strpos(reactivate_reason, "logged into my account using the F")
replace reac_reason=2 if strpos(reactivate_reason, "I used an app")
replace reac_reason=3 if strpos(reactivate_reason, "Other (please specify)")
replace reac_reason=4 if strpos(reactivate_reason, "Somebody else logged into")
replace reac_reason=5 if reactivate_reason=="" // missing means the person answered don't know
lab def rr 1 "logged into FB account" 2 "logged into other app" 3 "other reason" 4 "s.o. else logged into own FB account" 5 "don't know"
lab val reac_reason rr
lab def or 1 "reactivated on purpose" 5 "accidental reactivation"
lab val classify_other or
drop reactivate_reason

drop if reac_reason==. // drop those who didn't fill in the survey

* clean up date variable
gen start = clock(startdate, "DMYhm")
drop startdate
ren start startdate
format startdate %tc

bysort id (startdate): gen sequence=_n
gen day = dofc(startdate)
format day %td
drop startdate enddate
order id sequence day
ren day rr_day

* reshape to wide format
reshape wide reac* classify_other rr_day, i(id) j(sequence)

tempfile temprr
save `temprr'

*********************************************************
** Import, clean and reshape FB activation status data **
*********************************************************

use "$confidential_main/fb_deact_status_anonymous.dta", clear

* drop the milliseconds etc.
replace datetime = substr(datetime, 1, strpos(datetime, ".") - 1)
gen datetime2 = clock(datetime, "YMDhms")
drop datetime
ren datetime2 datetime
format datetime %tc
* note: time zone is UTC, need to switch to PT (7 hours behind during DST, 8 hours behind otherwise)
replace datetime = datetime - 7*60*60*1000
gen day = dofc(datetime)
replace datetime = datetime - 60*60*1000 if day>=21492 // after switch from DST to regular
drop day
gen day = dofc(datetime)
format day %td
gen hour = hh(datetime)
gen day_numeric = day-$start_experiment

* clean up enabled/disabled indicator
gen enabled2 = .
replace enabled2 = 0 if enabled=="disabled"
replace enabled2 = 1 if enabled=="enabled"
replace enabled2 = .e if enabled=="error"
drop enabled
ren enabled2 enabled
lab def enabled 0 "deactivated" 1 "not deactivated" .e "error"
lab val enabled enabled

gen disabled = 1- enabled

*** compute percent of time observed deactivated ***
*** (in days between midline and endline)
gen dis2 = disabled
replace dis2 = . if day_numeric<16
replace dis2 = . if day_numeric>41
bysort id: egen D = mean(dis2)
bysort id: egen numchecks = count(dis2)
replace D=. if numchecks==0
drop dis2

*** compute percent of time observed deactivated by day ***
gen dis2 = disabled
bysort id day_numeric: egen D_day = mean(dis2)
bysort id day_numeric: egen numchecks_day = count(dis2)
replace numchecks_day=0 if numchecks_day==.
replace D_day=. if numchecks_day<1

drop dis2

* change to wide format (1 row per person, 1 observation per day)
bysort id day_numeric: egen enab = total(enabled), missing
bysort id day_numeric: egen disab = total(disabled), missing

gen enabled_some=.
gen disabled_some=.
replace enabled_some=1 if enab>0 & enab!=.
replace enabled_some=0 if enab==0 & enab!=.
replace disabled_some=1 if disab>0 & disab!=.
replace disabled_some=0 if disab==0 & disab!=.
drop enabled disabled disab enab datetime hour
duplicates drop id day_numeric, force

tsset id day_numeric
tsfill, full

bysort day_numeric (day): replace day=day[1] if day==.
bysort id (D): replace D=D[1] if D==.
bysort id (numchecks): replace numchecks=numchecks[1] if numchecks==.

* set deactivation status to "enabled" for all those who notified us at midline that they would not deactivate (for technical reasons; developers (i.e. people who created an App running on Facebook) cannot deactivate their accounts)
replace D_day=0 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793) & D_day==.
replace D=0 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793)
replace numchecks_day=1 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793) & numchecks_day==.
replace numchecks=26 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793)
replace enabled_some=1 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793) & enabled_some==. & day_numeric>13
replace disabled_some=0 if (id==2263 | id==2045  | id==11379  | id==1647  | id==2793) & disabled_some==. & day_numeric>13

replace numchecks_day=0 if numchecks_day==.

summarize day_numeric
local current_day = r(max)

* reshape
reshape wide enabled_some disabled_some D_day numchecks_day day, i(id) j(day_numeric)

* merge in reason for reactivation
merge 1:1 id using `temprr'
drop _merge

tempfile tempfb
save `tempfb' // will be merged to rest of data below

*********************************************************
** Import, clean and reshape Twitter data **
*********************************************************

use "$confidential_main/tweets_anonymous.dta", clear
gen originalorder=_n
gen datetime2 = clock(datetime, "YMDhm")
drop datetime
ren datetime2 datetime
replace datetime=datetime-3*60*60*1000 // convert to Pacific time
gen day = dofc(datetime)
format day %td
drop datetime

* bound date of re-tweet: using date of previous tweet (note that tweets are ordered such that most recent one is first)
gen tweetorder=originalorder*(-1)
gen day_imputed=day
format day_imputed %td
replace day_imputed=. if is_retweet==1 // only impute retweet dates
forvalues x=1/105 {
	bysort id (tweetorder): replace day_imputed=day_imputed[_n-1] if day_imputed==. & day<=day_imputed[_n-1] & day!=. & _n>1 // replace w/ date of previous tweet if previous date is after retweet date
	bysort id (tweetorder): replace day_imputed=day if day_imputed==. & day>day_imputed[_n-1] & day!=. & day_imputed[_n-1]!=.  & _n>1 // keep retweet date if retweet date is after date of previous tweet
}
ren day day_original
ren day_imputed day
drop originalorder
replace day=1 if day==. // fill this in for later

* compute number of tweets in 4 weeks before midline, 4 weeks after midline, and 4 weeks after endline
gen period=.
replace period=1 if day<$midline & day>=$midline-27 // 4 weeks before ML
replace period=2 if day<$endline & day>=$endline-27 // 4 weeks between ML and EL
replace period=3 if day<$endline+28 & day>$endline  // 4 weeks after EL
gen no_retweet = 1-is_retweet
bysort id period: egen tweets= total(tweeted), missing // sets total to missing if all missing
bysort id period: egen retweets= total(is_retweet), missing
bysort id period: egen tweets_original= total(no_retweet), missing

* compute number of tweets by week
gen day_numeric = day-$start_experiment
gen week=.
forvalues x = -2/9 {
    replace week=`x' if day_numeric >= -14 & day_numeric < 7 * (`x' + 1) & week==.
}
preserve
bysort id week: egen tweets_week= total(tweeted), missing // sets total to missing if all missing
bysort id week: egen retweets_week= total(is_retweet), missing
bysort id week: egen tweets_original_week= total(no_retweet), missing
duplicates drop id week, force
keep id *week week has_twitter
replace week=week+3 // make week run from 1 through 12
replace week=13 if week==. // temporarily set week indicator for weeks outside range, or for those with no tweet in given week, to 13
reshape wide tweets_week tweets_original_week retweets_week, i(id) j(week)
foreach type in tweets_week tweets_original_week retweets_week {
	drop `type'13
	forvalues x=1/12 {
		replace `type'`x'=. if has_twitter!=1 // assign missing if doesn't have valid Twitter handle
		replace `type'`x'=0 if `type'`x'==. & has_twitter==1 // assign zero if no tweets in given period, but has valid Twitter handle
		replace `type'`x'=log(`type'`x'+1) // turn into logs
	}
}
tempfile tweets_by_week
save `tweets_by_week'
restore

duplicates drop id period, force
replace period=5 if period==. // set this to non-missing so that we can reshape
keep id period tweets tweets_original retweets has_twitter

reshape wide tweets tweets_original retweets, i(id) j(period)
foreach type in tweets tweets_original retweets {
	ren `type'1 `type'_b
	ren `type'2 `type'
	ren `type'3 `type'_post
	drop `type'5
}

foreach type in tweets tweets_original retweets {
	foreach sub in _b "" _post {
		replace `type'`sub'=. if has_twitter!=1 // assign missing if doesn't have valid Twitter handle
		replace `type'`sub'=0 if `type'`sub'==. & has_twitter==1 // assign zero if no tweets in given period, but has valid Twitter handle
		replace `type'`sub'=log(`type'`sub'+1) // turn into logs
	}
}

* merge in Twitter data at weekly level
merge 1:1 id using `tweets_by_week'
drop _merge

* save for later
tempfile temptwitter
save `temptwitter'

*****************************
****** Import Baseline ******
*****************************

use "$confidential_main/baseline_anonymous.dta", clear
ren *, lower
destring _all, replace

* clean date variables
gen enddate2 = clock(enddate, "MDYhm")
drop enddate
ren enddate2 enddate
format enddate %tc
gen endday=dofc(enddate)
format endday %td
ren endday endday_b

ren fb_goodbad_5 fb_goodbad_1 // this variable is called fb_goodbad_1 in Endline

tempfile temp
save `temp'

preserve
keep id
tempfile tempsamplefull
save `tempsamplefull'
restore

*****************************
****** Import Midline *******
*****************************

use "$confidential_main/midline_anonymous.dta", clear
ren *, lower
destring _all, replace

ren first_4weeks_num price
ren second_4weeks_num1 price2
ren second_4weeks_num2 price2_2

gen startdate2 = clock(startdate, "YMDhms")
drop startdate
ren startdate2 startdate_midline
gen enddate2 = clock(enddate, "YMDhms")
drop enddate
ren enddate2 enddate_midline
format startdate_midline enddate_midline %tc
ren progress progress_midline
ren durationinseconds durationinseconds_midline
keep id lowqual wta* *understanding* startdate_midline enddate_midline progress_midline durationinseconds_midline price* first_question_counts fb* opsystem battery_days deactiv_yesno
gen wta2=wta2_group1
replace wta2=wta2_group2 if wta2==.
drop *group*
drop if id==1461 & wta1==. // this person started the survey twice (but only filled in the first question the first time around)

* clean up self-reported time spent on FB app
ren fbhrs_iphone fbhrs_nonbattery
replace fbhrs_nonbattery = fbhrs_noniphone if fbhrs_nonbattery==. // consolidate answers of those who have the FB app but either don't have an iPhone or couldn't find their battery report in the iPhone
drop fbhrs_noniphone

* merge to baseline
merge 1:1 id using `temp' // some people did not do midline
gen no_midline=0
replace no_midline=1 if startdate_midline==.
gen randomized=0
replace randomized=1 if wta_understanding2!="" // made it to randomization stage in midline
drop _merge

gen survey=1

tempfile temp
save `temp'

*****************************
******* Import Endline ******
*****************************

use "$confidential_main/endline_anonymous.dta", clear
ren *, lower

foreach var of varlist finished news_knowledge_5-news_knowledge_19 { // for some reason this gets read in differently in baseline
	replace `var'="1" if `var'=="True"
	replace `var'="0" if `var'=="False"
}

destring _all, replace
drop second_4weeks_num1 second_4weeks_num2 first_question_counts wta1 wta2 lowqual first_4weeks_num t accept_second_4weeks
gen startdate2 = clock(startdate, "YMDhms")
drop startdate
ren startdate2 startdate
gen enddate2 = clock(enddate, "YMDhms")
drop enddate
ren enddate2 enddate
format startdate enddate %tc

ren q106_pagesubmit time_news_pagesubmit
drop q106*

* create indicator for whether started/completed endline
merge 1:1 id using `tempsamplefull'
gen no_endline=0
replace no_endline=1 if _merge==2
replace no_endline=1 if wta3==. // code incomplete endline responses as non-responses for now
drop _merge

* mark incomplete endline responses
gen partial_endline=0
replace partial_endline=1 if pol_feeling_1!=. & wta3==.

gen survey=3

tempfile tempendline
save `tempendline'

*****************************
******* Import Post-Endline survey ******
*****************************

use "$confidential_main/postendline_anonymous.dta", clear

* clean up date variable
ren startdate startdate_postendline
gen startdate_postendline2 = clock(startdate_postendline, "DMYhm")
drop startdate_postendline
ren startdate_postendline2 startdate_postendline
format startdate_postendline %tc

* drop second occurence of case of one participant who filled out post-endline survey twice
bysort id (startdate_postendline): drop if _n>1 & apps_less_use[_n-1]!=""

* merge to endline
merge 1:1 id using `tempendline'
ren _merge tempmerge

* create indicator for whether started/completed post-endline survey
merge 1:1 id using `tempsamplefull'
gen no_postendline=0
replace no_postendline=1 if _merge==2 | (_merge==3 & tempmerge==2) | (apps_less_use=="") // last condition sets post endline to missing if participant didn't answer all the way through to very last question.
drop tempmerge _merge

*****************************
***** Append BL/ML & EL *****
*****************************
* 2 rows per participant: one for baseline and midline, and one for endline
append using `temp'

* fill in variables only available at endline

foreach bytevar in no_endline partial_endline no_postendline wta3 deact_24h_goodbad_1 deact_4wk_goodbad_1 fbhrs_noapp startdate_postendline wta_change_code1 wta_change_code2 wta_change_code3  {
	bysort id (survey): replace `bytevar'=`bytevar'[_n+1] if `bytevar'==.
}

ren deact_24h_goodbad_1 deact_24h_goodbad
ren deact_4wk_goodbad_1 deact_4wk_goodbad

foreach stringvar of varlist agenda followup vote_registr fb_use_plan autom_reac  {
	bysort id (survey): replace `stringvar'=`stringvar'[_n+1] if `stringvar'==""
}

*****************************
****** Clean variables ******
*****************************

*** make all string values lower case
ds, has(type string)
foreach var of varlist `r(varlist)' {
	replace `var'=lower(`var')
}

gen age = 2018-yob_prescreen
replace age = . if yob_prescreen<1900|yob_prescreen>2016

*** Demographics
gen female=.
replace female =1 if gender=="female"
replace female =0 if gender=="male" | gender=="other"
gen male=.
replace male =1 if gender=="male"
replace male =0 if gender=="female" | gender=="other"

drop gender

gen hhld_inc2=. // set values to middle of intervals, and set to 200 for top category
replace hhld_inc2=5 if strpos(hhld_inc, "$9,999")>0
replace hhld_inc2=15 if strpos(hhld_inc, "$10,000")>0
replace hhld_inc2=25 if strpos(hhld_inc, "$20,000")>0
replace hhld_inc2=35 if strpos(hhld_inc, "$30,000")>0
replace hhld_inc2=45 if strpos(hhld_inc, "$40,000")>0
replace hhld_inc2=55 if strpos(hhld_inc, "$50,000")>0
replace hhld_inc2=67 if strpos(hhld_inc, "$60,000")>0
replace hhld_inc2=88 if strpos(hhld_inc, "$75,000")>0
replace hhld_inc2=112 if strpos(hhld_inc, "$100,000")>0
replace hhld_inc2=138 if strpos(hhld_inc, "$125,000")>0
replace hhld_inc2=200 if strpos(hhld_inc, "$150,000")>0
replace hhld_inc2=.n if strpos(hhld_inc, "prefer not to answer")>0
labmask hhld_inc2, values(hhld_inc)
drop hhld_inc
ren hhld_inc2 hhld_inc

gen educ2=.
replace educ2=1 if strpos(educ_prescreen, "less than a high school diploma")>0
replace educ2=2 if strpos(educ_prescreen, "high school diploma or equivalent")>0
replace educ2=3 if strpos(educ_prescreen, "some college")>0
replace educ2=4 if strpos(educ_prescreen, "associate")>0
replace educ2=5 if strpos(educ_prescreen, "bachelor")>0
replace educ2=6 if strpos(educ_prescreen, "graduate degree")>0
labmask educ2, values(educ_prescreen)
drop educ_prescreen
ren educ2 educ

gen educyears=.
replace educyears = 11 if educ==1
replace educyears = 12 if educ==2
replace educyears = 13 if educ==3
replace educyears = 14 if educ==4
replace educyears = 16 if educ==5
replace educyears = 19 if educ==6

gen race2=.
replace race2=1 if strpos(race, "american indian")>0
replace race2=2 if strpos(race, "asian")>0
replace race2=3 if strpos(race, "black")>0
replace race2=4 if strpos(race, "hispanic")>0
replace race2=5 if strpos(race, "white")>0
replace race2=6 if strpos(race, "other")>0
labmask race2, values(race)
drop race
ren race2 race
ren race_6_text race_other_specify

gen white=.
replace white=0 if race!=. & race!=5
replace white=1 if race==5
gen black=.
replace black=0 if race!=. & race!=3
replace black=1 if race==3

*** political views

gen repdem2=.
replace repdem2=1 if strpos(repdem, "strongly demo")>0
replace repdem2=2 if strpos(repdem, "weakly demo")>0
replace repdem2=3 if strpos(repdem, "toward the demo")>0
replace repdem2=4 if repdem=="independent"
replace repdem2=5 if strpos(repdem, "toward the rep")>0
replace repdem2=6 if strpos(repdem, "weakly rep")>0
replace repdem2=7 if strpos(repdem, "strongly rep")>0
labmask repdem2, values(repdem)
drop repdem
ren repdem2 repdem

gen republican=.
replace republican=0 if repdem!=. & repdem!=6 & repdem!=7
replace republican=1 if repdem==6 | repdem==7
gen democrat=.
replace democrat=0 if repdem!=. & repdem!=1 & repdem!=2
replace democrat=1 if repdem==1 | repdem==2

** party for polarization variables
gen republican_polar=.
replace republican_polar=0 if repdem!=. & repdem!=5 & repdem!=6 & repdem!=7
replace republican_polar=1 if repdem==5 | repdem==6 | repdem==7
gen democrat_polar=.
replace democrat_polar=0 if repdem!=. & repdem!=1 & repdem!=2 & repdem!=3
replace democrat_polar=1 if repdem==1 | repdem==2 | repdem==3

gen libcon2=.
replace libcon2=1 if strpos(libcon, "extremely lib")>0
replace libcon2=2 if libcon=="liberal"
replace libcon2=3 if strpos(libcon, "slighthly lib")>0
replace libcon2=4 if strpos(libcon, "moderate")>0
replace libcon2=5 if strpos(libcon, "slighthly conservative")>0
replace libcon2=6 if libcon=="conservative"
replace libcon2=7 if strpos(libcon, "extremely conservative")>0
replace libcon2=8 if strpos(libcon, "thought much about this")>0
labmask libcon2, values(libcon)
drop libcon
ren libcon2 libcon

ren pol_feeling_1 dem_feeling
ren pol_feeling_2 rep_feeling
ren pol_feeling_3 trump_feeling

foreach var of varlist rep_pov dem_pov {
	gen `var'2=.
	replace `var'2=0 if `var'=="never"
	replace `var'2=1 if `var'=="once"
	replace `var'2=2.5 if `var'=="two or three times"
	replace `var'2=4 if `var'=="four times or more"
*	labmask `var'2, values(`var')
	drop `var'
	ren `var'2 `var'
}

*** election module
gen vote12=.
replace vote12=1 if vote1=="yes"
replace vote12=0 if vote1=="no"
replace vote12=0.5 if vote1=="unsure"
*labmask vote12, values(vote1)
drop vote1
ren vote12 vote_yesno

gen vote22=.
replace vote22=1 if vote2=="republican candidate"
replace vote22=0 if vote2=="democratic candidate"
replace vote22=0.5 if vote2=="other/don't know"
drop vote2
ren vote22 vote_repdem

* re-scale the generic Ballot about how convinced voter is
ren vote3* vote_convinced
gen vote_conv_resc = .
replace vote_conv_resc = (vote_convinced-50)/50 // -1 is certain dem, +1 is certain rep
replace vote_conv_resc = 0 if vote_repdem==0.5 // assign 0 to those who said they'd vote "other" or "don't know"

*** news
foreach var of varlist follow_politics follow_trump {
	gen `var'2=.
	replace `var'2=1 if `var'=="not at all closely"
	replace `var'2=2 if `var'=="somewhat closely"
	replace `var'2=3 if `var'=="rather closely"
	replace `var'2=4 if `var'=="very closely"
	labmask `var'2, values(`var')
	drop `var'
	ren `var'2 `var'
}

ren news_source_2_12 news_source_2_8

forvalues x=1/8 {
	gen news_source_2_`x'2=.
	replace news_source_2_`x'2 = 1 if news_source_2_`x'=="never"
	replace news_source_2_`x'2 = 2 if news_source_2_`x'=="hardly ever"
	replace news_source_2_`x'2 = 3 if news_source_2_`x'=="sometimes"
	replace news_source_2_`x'2 = 4 if news_source_2_`x'=="fairly often"
	replace news_source_2_`x'2 = 5 if news_source_2_`x'=="very often"
	drop news_source_2_`x'
	ren news_source_2_`x'2 news_source_2_`x'
}

ren news_source_3_11 news_source_3_8
ren news_source_3_6 news_source_3_7
ren news_source_3_5 news_source_3_6
ren news_source_3_10 news_source_3_5

forvalues x=1/8 {
	gen news_source_3_`x'2=.
	replace news_source_3_`x'2 = 1 if news_source_3_`x'=="a lot less"
	replace news_source_3_`x'2 = 2 if news_source_3_`x'=="a little less"
	replace news_source_3_`x'2 = 3 if news_source_3_`x'=="same"
	replace news_source_3_`x'2 = 4 if news_source_3_`x'=="a little more"
	replace news_source_3_`x'2 = 5 if news_source_3_`x'=="a lot more"
	drop news_source_3_`x'
	ren news_source_3_`x'2 news_source_3_`x'
}

forvalues x=1/8 {
	gen news_source_`x'=news_source_2_`x'
	replace news_source_`x'=news_source_3_`x' if news_source_2_`x'==.
	drop news_source_2_`x' news_source_3_`x'
}

lab def news 1 "never/a lot less" 2 "hardly ever/a little less" 3 "sometimes/same" 4 "fairly often/a little more" 5 "very often/a lot more"
lab val news_source_* news


forvalues x=5/19 {
	replace news_knowledge_`x'="0.5" if news_knowledge_`x'=="unsure"
	destring news_knowledge_`x', replace
	gen correct_`x'=.
	replace correct_`x'=0.5 if news_knowledge_`x'==0.5
	replace correct_`x'=0 if  news_knowledge_`x'!=0.5 & news_knowledge_`x'!=.
}

replace correct_5 = 1 if news_knowledge_5==1 // true news event, true statement
replace correct_6 = 1 if news_knowledge_6==1 // true news event, true statement
replace correct_7 = 1 if news_knowledge_7==1 // true news event, true statement
replace correct_8 = 1 if news_knowledge_8==1 // true news event, true statement
replace correct_9 = 1 if news_knowledge_9==1 // true news event, true statement
replace correct_10 = 1 if news_knowledge_10==1 // true news event, true statement
replace correct_11 = 1 if news_knowledge_11==1 // true news event, true statement
replace correct_12 = 1 if news_knowledge_12==0 // fake news
replace correct_13 = 1 if news_knowledge_13==0 // fake news
replace correct_14 = 1 if news_knowledge_14==0 // fake news
replace correct_15 = 1 if news_knowledge_15==0 // fake news
replace correct_16 = 1 if news_knowledge_16==0 // fake news
replace correct_17 = 1 if news_knowledge_17==0 // related to true news event, false statement
replace correct_18 = 1 if news_knowledge_18==0 // related to true news event, false statement
replace correct_19 = 1 if news_knowledge_19==0 // related to true news event, false statement

gen news_knowledge_score=.
replace news_knowledge_score=correct_5+correct_6+correct_7+correct_8+correct_9+correct_10+correct_11+correct_17+correct_18+correct_19

*** fake news knowledge measures ***
gen fake_news_score=. // count unsure as 0.5 points
replace fake_news_score=correct_12+correct_13+correct_14+correct_15+correct_16

gen strictly_correct_score=0 // count unsure as incorrect answer (0 points)
forvalues x=12/16 {
	replace strictly_correct_score=strictly_correct_score+1 if correct_`x'==1
}
replace strictly_correct_score=. if correct_16==.

gen weakly_correct_score=0
forvalues x=12/16 { // count unsure as correct answer (1 point)
	replace weakly_correct_score=weakly_correct_score+1 if correct_`x'==1 | correct_`x'==0.5
}
replace weakly_correct_score=. if correct_16==.

*** indicators for true / false / unsure answers ***
forvalues x=5/19 {
	gen true_`x'=.
	replace true_`x'=0 if news_knowledge_`x'!=1 & news_knowledge_`x'!=.
	replace true_`x'=1 if news_knowledge_`x'==1
	gen false_`x'=.
	replace false_`x'=0 if news_knowledge_`x'!=0 & news_knowledge_`x'!=.
	replace false_`x'=1 if news_knowledge_`x'==0
	gen unsure_`x'=.
	replace unsure_`x'=0 if news_knowledge_`x'!=0.5 & news_knowledge_`x'!=.
	replace unsure_`x'=1 if news_knowledge_`x'==0.5
}

gen unsure_score=unsure_12+unsure_13+unsure_14+unsure_15+unsure_16

** rescale attitude

foreach t in trade race metoo kavanaugh immigration mueller gun media trump {
	ren attitude_`t'* attitude_`t'
	replace attitude_`t' = attitude_`t' - 5
}

*** Subjective well-being and social connectedness

gen swb_happiness2=swb_happiness
replace swb_happiness2="1" if strpos(swb_happiness, "not a very")>0
replace swb_happiness2="7" if strpos(swb_happiness, "(a very happy")>0
destring swb_happiness2, replace
labmask swb_happiness2, values(swb_happiness)
drop swb_happiness
ren swb_happiness2 swb_happiness

gen swb_relhappiness2=swb_relhappiness
replace swb_relhappiness2="1" if strpos(swb_relhappiness, "less happy")>0
replace swb_relhappiness2="7" if strpos(swb_relhappiness, "more happy")>0
destring swb_relhappiness2, replace
labmask swb_relhappiness2, values(swb_relhappiness)
drop swb_relhappiness
ren swb_relhappiness2 swb_relhappiness

foreach var of varlist swb_swl1 swb_swl2 swb_swl3 {
	gen `var'2=.
	replace `var'2=1 if `var'=="strongly disagree"
	replace `var'2=2 if `var'=="disagree"
	replace `var'2=3 if `var'=="slightly disagree"
	replace `var'2=4 if `var'=="neither agree nor disagree"
	replace `var'2=5 if `var'=="slightly agree"
	replace `var'2=6 if `var'=="agree"
	replace `var'2=7 if `var'=="strongly agree"
	labmask `var'2, values(`var')
	drop `var'
	ren `var'2 `var'
}

foreach var of varlist swb_lnlns1 swb_lnlns2 swb_lnlns3 {
	gen `var'2=.
	replace `var'2=1 if `var'=="hardly ever"
	replace `var'2=2 if `var'=="some of the time"
	replace `var'2=3 if `var'=="often"
	labmask `var'2, values(`var')
	drop `var'
	ren `var'2 `var'
}

foreach var of varlist swb_eurhappsvy_4 swb_eurhappsvy_5 swb_eurhappsvy_6 swb_eurhappsvy_7 {
	replace `var'="2" if `var'=="2."
	replace `var'="3" if `var'=="3."
	replace `var'="1. none or almost none of the time" if `var'=="1. none or almost none of the times"
	replace `var'="4. all or almost all of the time" if `var'=="4. all or almost all of the time."

	gen `var'2=.
	replace `var'2=1 if `var'=="1. none or almost none of the time"
	replace `var'2=2 if `var'=="2"
	replace `var'2=3 if `var'=="3"
	replace `var'2=4 if `var'=="4. all or almost all of the time"
	labmask `var'2, values(`var')
	drop `var'
	ren `var'2 `var'
}

foreach var in dinner cinema phone voted country party together shopping parents kids none {
	gen leisure_`var'=.
	replace leisure_`var'=0 if (leisure_activities1=="" & leisure_activities2_1!="") |  (leisure_activities1=="" & leisure_activities3_1!="")
	replace leisure_`var'=0 if strpos(leisure_activities1, "`var'")==0 & leisure_activities1!=""
	replace leisure_`var'=1 if !(strpos(leisure_activities1, "`var'")==0)
}
drop leisure_activities1

gen leisure_offline = leisure_dinner+leisure_cinema+leisure_phone+leisure_party+leisure_together+leisure_shopping+leisure_parents+leisure_kids
gen leisure_diverse = leisure_voted+leisure_country

forvalues x=1/5 {
	gen leisure_activities2_`x'2=.
	replace leisure_activities2_`x'2=0 if leisure_activities2_`x'=="0 minutes"
	replace leisure_activities2_`x'2=15 if leisure_activities2_`x'=="between 1 and 30 minutes"
	replace leisure_activities2_`x'2=45 if leisure_activities2_`x'=="between 31 minutes and 1 hour"
	replace leisure_activities2_`x'2=90 if leisure_activities2_`x'=="between 1 and 2 hours"
	replace leisure_activities2_`x'2=150 if leisure_activities2_`x'=="between 2 and 3 hours"
	replace leisure_activities2_`x'2=240 if leisure_activities2_`x'=="more than 3 hours"
	*labmask leisure_activities2_`x'2, values(leisure_activities2_`x')
	drop leisure_activities2_`x'
	ren leisure_activities2_`x'2 leisure_activities2_`x'
}

forvalues x=1/5 {
	gen leisure_activities3_`x'2=.
	replace leisure_activities3_`x'2=1 if leisure_activities3_`x'=="a lot less"
	replace leisure_activities3_`x'2=2 if leisure_activities3_`x'=="a little less"
	replace leisure_activities3_`x'2=3 if leisure_activities3_`x'=="same"
	replace leisure_activities3_`x'2=4 if leisure_activities3_`x'=="a little more"
	replace leisure_activities3_`x'2=5 if leisure_activities3_`x'=="a lot more"
	labmask leisure_activities3_`x'2, values(leisure_activities3_`x')
	drop leisure_activities3_`x'
	ren leisure_activities3_`x'2 leisure_activities3_`x'
}

forvalues x=1/5 {
	gen leisure_activities_`x'=leisure_activities2_`x'
	replace leisure_activities_`x'=leisure_activities3_`x' if leisure_activities2_`x'==.
	drop leisure_activities2_`x' leisure_activities3_`x'
}

*lab def leisure 1 "0 minutes/a lot less" 2 "between 1 and 30 minutes/a little less" 3 "between 31 minutes and 1 hour/same" 4 "between 1 and 2 hours/a little more" 5 "between 2 and 3 hours/a lot more" 6 "more than 3 hours"
*lab val leisure_activities_* leisure

ren leisure_activities_1 leisure_sm
ren leisure_activities_2 leisure_oo
ren leisure_activities_3 leisure_tvm
ren leisure_activities_4 leisure_nns
ren leisure_activities_5 leisure_frf

*** Social media use
ren fb_activepassive_1 fb_activepassive
sum fb_activepassive if qualified==1 & lowqual==0
replace fb_activepassive = (fb_activepassive-r(mean))/r(sd)
ren fb_activepassive2_1 fb_activepassive2
sum fb_activepassive2 if qualified==1 & lowqual==0
replace fb_activepassive2 = (fb_activepassive2-r(mean))/r(sd)

gen fb_usetime2 = .
replace fb_usetime2=1 if strpos(fb_usetime, "n/a")>0
replace fb_usetime2=2 if strpos(fb_usetime, "morning (6")>0
replace fb_usetime2=3 if strpos(fb_usetime, "afternoon")>0
replace fb_usetime2=4 if strpos(fb_usetime, "evening")>0
replace fb_usetime2=5 if strpos(fb_usetime, "night (9")>0
replace fb_usetime2=6 if strpos(fb_usetime, "late night")>0
labmask fb_usetime2, values(fb_usetime)
drop fb_usetime
ren fb_usetime2 fb_usetime

gen fb_usetime_min=.
replace fb_usetime_min=6 if fb_usetime==2
replace fb_usetime_min=12 if fb_usetime==3
replace fb_usetime_min=17 if fb_usetime==4
replace fb_usetime_min=21 if fb_usetime==5
replace fb_usetime_min=0 if fb_usetime==6
gen fb_usetime_max=.
replace fb_usetime_max=12 if fb_usetime==2
replace fb_usetime_max=17 if fb_usetime==3
replace fb_usetime_max=21 if fb_usetime==4
replace fb_usetime_max=24 if fb_usetime==5
replace fb_usetime_max=6 if fb_usetime==6

* remove all suffixes from slider variables, and normalize such that 0 means neutral
foreach var in fb_soclife fb_goodbad fb_society fb_happy fb_betterfollownews fb_morefakenews fb_habit fb_polarize {
	ren `var'* `var'
	replace `var'=`var'-5
}

* Create dummy for whether people have positive or negative opinions about FB
gen opinion = .
replace opinion = 0 if fb_goodbad < 0 & fb_goodbad!=.
replace opinion = 1 if fb_goodbad >= 0 & fb_goodbad!=.

*** Opinions about Facebook
gen fb_deact_good=.
replace fb_deact_good = (deact_24h_goodbad - 5) * (-1) if deact_24h_goodbad !=.
replace fb_deact_good = (deact_4wk_goodbad - 5) * (-1) if deact_4wk_goodbad !=.

* adjust vars such that positive is "good"
foreach var in fb_morefakenews fb_habit fb_polarize fb_negimp_length fb_negimp_log {
	replace `var'=`var'*(-1)
}


*** Post experiment use
* Planned use
gen planned_use=.
replace planned_use=-1 if strpos(fb_use_plan, "stop")>0
replace planned_use=-.75 if strpos(fb_use_plan, "decrease the amount of time i spend on facebook by 50%")>0
replace planned_use=-0.25 if strpos(fb_use_plan, "decrease the amount of time i spend on facebook by 1%")>0
replace planned_use=0 if strpos(fb_use_plan, "neither")>0
replace planned_use=0.25 if strpos(fb_use_plan, "increase the amount of time i spend on facebook by 1%")>0
replace planned_use=0.75 if strpos(fb_use_plan, "increase the amount of time i spend on facebook by 50%")>0


*** final questions on researcher agenda
ren agenda agenda2
gen agenda = .
replace agenda = 0 if strpos(agenda2, "no, i don't")>0
replace agenda = 0 if strpos(agenda2, "i am not sure")>0
replace agenda = 0 if strpos(agenda2, "wanted to show that facebook is good for people")>0
replace agenda = 1 if strpos(agenda2, "wanted to show that facebook is bad for people")>0
*labmask agenda2, values(agenda)
*drop agenda


*** FB app use
* extract days of battery report
egen app_timespan=sieve(battery_days), keep(numeric)
replace app_timespan="1" if battery_days=="last day"
destring app_timespan, replace
labmask app_timespan, values(battery_days)
drop battery_days

replace fbhrs_battery = subinstr(fbhrs_battery, `"""',  "", .)
*extract minutes of battery report
gen min =  substr(fbhrs_battery, 1, strpos(fbhrs_battery, "m")-1)
replace min=strrtrim(min)
split min, p(" ")
gen m=""
replace m=min5
replace m=min4 if m==""
replace m=min3 if m==""
replace m=min2 if m==""
replace m=min1 if m==""
drop min min1 min2 min3 min4 min5
ren m min
replace min = "0" if min=="0h0"
replace min = "43" if min=="24h43"
replace min = "51" if min=="2h51"
replace min = "20" if min=="3hr20"
replace min = "22" if min=="6h22"
replace min = "44" if fbhrs_battery=="9hrs and 44hmin"
replace min = "" if min=="and"
replace min = "" if min=="hours,"
destring min, replace

* extract hours of battery report
gen hrs = substr(fbhrs_battery, 1, strpos(fbhrs_battery, "h")-1)
replace hrs="0" if hrs=="0 (i "
replace hrs="" if strpos(fbhrs_battery, "38 mins 1.2 hrs background")!=0
replace hrs=strtrim(hrs)
replace min = 48 if  hrs=="2:48"
replace hrs="2" if hrs=="2:48"
destring hrs, replace

replace hrs=0 if fbhrs_battery=="0" | fbhrs_battery=="0 / did not use " | fbhrs_battery=="0.0"
replace min=0 if fbhrs_battery=="0" | fbhrs_battery=="0 / did not use " | fbhrs_battery=="0.0"
replace hrs=1.7 if fbhrs_battery=="hours 1.7"
replace hrs=17 if strpos(fbhrs_battery, "17hours 25minutes")!=0
replace min=25 if strpos(fbhrs_battery, "17hours 25minutes")!=0
replace hrs=2 if strpos(fbhrs_battery, "2h 18m screen")!=0
replace min=18 if strpos(fbhrs_battery, "2h 18m screen")!=0
replace hrs=4 if strpos(fbhrs_battery, " 4h 43m")!=0
replace min=43 if strpos(fbhrs_battery, " 4h 43m")!=0
replace hrs=0 if strpos(fbhrs_battery, "38 mins 1.2 hrs background")!=0
replace hrs=10.5 if strpos(fbhrs_battery, "hours and minutes 10.5 hours ")!=0
replace hrs=3 if fbhrs_battery=="3:28"
replace min=28 if fbhrs_battery=="3:28"

*set hrs to 0 if only minutes reported
replace hrs=0 if hrs==. & min!=.
*set minutes to 0 if only hours reported
replace min=0 if min==. & hrs!=.

tab fbhrs_battery if hrs==. & min==. // some observations left with ambiguous answers (because hours/minutes was not specified)
* among those that don't report whether it's hours or minutes, we know that if there is a decimal point in their answer, it's hours
gen hrs2 = ""
replace hrs2 = fbhrs_battery if hrs==. & strpos(fbhrs_battery, ".")
replace hrs2=strtrim(hrs2)
destring hrs2, replace
replace hrs=hrs2 if hrs==.
drop hrs2
* we also know that if the number reported > 60, it must be in minutes
gen hrs2 = fbhrs_battery if hrs==. & strpos(fbhrs_battery, "%")==0
egen hrs3=sieve(hrs2), keep(numeric)
destring hrs3, replace
replace hrs=hrs3 if hrs3>60 & hrs==.
drop hrs2 hrs3

* aggregate to minutes per day
replace min = min/60
gen mobile_minutes = ((hrs+min)/app_timespan)*60
drop hrs min

* create indicator for whether mobile app minutes report is from battery or self-report
gen mm_from_battery=.
replace mm_from_battery = . if mobile_minutes==. & fbhrs_nonbattery==.
replace mm_from_battery = 1 if mobile_minutes!=.
replace mm_from_battery = 0 if mobile_minutes==. & fbhrs_nonbattery!=.
drop fbhrs_iphone_success

replace mobile_minutes=(fbhrs_nonbattery/7)*60 if mobile_minutes==. // replace mobile minutes with non-battery self-report in case person doesn't have iPhone or couldn't find battery report (but has fb app)

tsset id survey


*************************************
** Reduce to 1 obs per participant **
*************************************

foreach var of varlist wta_understanding1 wta_understanding2 startdate enddate progress finished durationinseconds rep_angry* dem_angry* rep_pov dem_pov dem_feeling rep_feeling trump_feeling follow_politics follow_trump read_news news_* fake_news_score strictly_correct_score weakly_correct_score true_* false_* unsure_* correct* time_news* swb_* friends_met_number leisure_* fb_minutes fb_soclife fb_goodbad fb_society fb_happy fb_betterfollownews fb_morefakenews fb_habit fb_polarize fb_posimp* fb_negimp* attitude* vote_yesno vote_repdem vote_convinced vote_conv_resc mobile_minutes app_timespan fbhrs_noapp mm_from_battery opinion {
		bysort id (survey): gen end_`var' = `var'[_n+1] if survey[_n+1]==3
}

foreach var of varlist wta_understanding1 wta_understanding2 startdate enddate progress finished durationinseconds rep_angry* dem_angry* rep_pov dem_pov dem_feeling rep_feeling trump_feeling follow_politics follow_trump read_news news_* fake_news_score strictly_correct_score weakly_correct_score true_* false_* unsure_* correct* swb_* time_news* friends_met_number leisure_* fb_minutes fb_soclife fb_goodbad fb_society fb_happy fb_betterfollownews fb_morefakenews fb_habit fb_polarize fb_posimp* fb_negimp* attitude* vote_yesno vote_repdem vote_convinced vote_conv_resc mobile_minutes app_timespan fbhrs_noapp mm_from_battery opinion {
		ren `var' `var'_b
}

ren end_* *

drop if survey==3
drop survey

**************************
*** treatment status
**************************
gen T=.
replace T=1 if price==102 & wta1!=. & randomized==1
replace T=0 if price==0 & wta1!=. & randomized==1
replace T=. if lowqual==1
lab def treat 0 "Control" 1 "Treatment"
lab val T treat

*****************************
*** Merge in SMS responses **
*****************************

merge m:1 id using `tempsms'
ren _merge merge_sms

*****************************
* Merge in FB activ. status *
*****************************

merge m:1 id using `tempfb'
ren _merge merge_fbactstat

*****************************
* Merge in Twitter data *
*****************************

merge m:1 id using `temptwitter'
ren _merge merge_twitter

*****************************
* Merge in post-endline email data *
*****************************

merge m:1 id using "$confidential_main/timeuse_anonymous.dta"
ren _merge merge_timeuseemail

merge m:1 id using "$confidential_main/politics_anonymous.dta"
ren _merge merge_politicalemail

*****************************
* Merge in voting data *
*****************************
merge m:1 id using `tempvoting'
keep if _merge==1 | _merge==3

* record whether individual was found in L2 data
gen found_inL2=0
replace found_inL2=1 if _merge==3
gen found_inL2_bestmergequality=0
replace found_inL2_bestmergequality=1 if _merge==3 & merge_quality==1

ren _merge merge_voting

* impute voting and donations for those who didn't merge
foreach year in 2016 2018 {
	replace vote_`year'=0 if missing(vote_`year') // assume who didn't merge didn't vote
	replace vote_`year'=. if found_inL2==0 // keep only those we matched
}
ren vote_2016 vote_2018_b

replace donated=0 if missing(donated) // assume who didn't merge didn't donate

*****************************
* Drop irrelevant Qualtrics variables *
*****************************
drop status responsei* recordedd* externalr* distributionch* userlan*

**********************************************************
******** re-orient variables						 *****
**********************************************************
* such that more positive values have the same meaning
foreach x in 4 5 7 {
	foreach type in "" _b {
		replace swb_eurhappsvy_`x'`type' = (-1)*(swb_eurhappsvy_`x'`type'-5)
	}
}


**********************************************************
******** rename variables						 *
**********************************************************

* valuation
ren wta1 v

******************************
*** Political polarization ***
******************************

foreach time in "" _b {

	* Party affective polarization
	gen party_thermo`time'=.
	replace party_thermo`time' = dem_feeling`time' - rep_feeling`time' if democrat_polar==1
	replace party_thermo`time' = rep_feeling`time' - dem_feeling`time' if republican_polar==1

	* Trump affective polarization
	gen trump_thermo`time'=.
	gen rescale_trump_feeling`time'=.
	replace rescale_trump_feeling`time' = trump_feeling`time' - 50
	replace trump_thermo`time' = rescale_trump_feeling`time' * (-1) if democrat_polar==1
	replace trump_thermo`time' = rescale_trump_feeling`time' if republican_polar==1

	* Party anger
	gen party_anger`time'=.
	replace party_anger`time' = log(dem_angry_length`time' + 1) - log(rep_angry_length`time' + 1) if republican_polar==1
	replace party_anger`time' = log(rep_angry_length`time' + 1) - log(dem_angry_length`time' + 1) if democrat_polar==1

	* Party understanding
	gen party_understand`time'=.
	replace party_understand`time' = dem_pov`time' -rep_pov`time' if democrat_polar==1
	replace party_understand`time' = rep_pov`time' - dem_pov`time' if republican_polar==1

	* Vote polarization
	gen vote_polar`time'=.
	replace vote_polar`time' = vote_conv_resc`time' if republican_polar==1
	replace vote_polar`time' = (-1) * vote_conv_resc`time' if democrat_polar==1
	replace vote_polar`time' = 0 if vote_repdem`time'==0.5 & (democrat_polar==1 | republican_polar==1)

}

* Issue polarization
foreach t in trade race metoo kavanaugh immigration mueller gun media trump {
	foreach time in "" _b {
		gen resc_att_`t'`time'=attitude_`t'`time'
		sum resc_att_`t'`time' if T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
		local sigma = r(sd)
		replace resc_att_`t'`time'=resc_att_`t'`time'/`sigma'
		summarize resc_att_`t'`time' if democrat_polar==1 & T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
		local D = r(mean)
		summarize resc_att_`t'`time' if republican_polar==1 & T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
		local R = r(mean)
		local recenter = (`D' + `R') * (-0.5)
		replace resc_att_`t'`time' = resc_att_`t'`time' + `recenter'
		if `D' > `R' {
			replace resc_att_`t'`time' = resc_att_`t'`time' * (-1)
		}
		gen temp_`t'`time'=resc_att_`t'`time'
		replace resc_att_`t'`time' = (-1) * resc_att_`t'`time' if democrat_polar==1
		replace resc_att_`t'`time' = . if !(republican_polar==1 | democrat_polar==1)
	}

	gen diff = resc_att_`t'_b - resc_att_`t'
	summarize diff if T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
	local sigma_`t' = r(sd)
	drop diff
}

*weight attitudes
foreach time in "" _b {
	foreach t in trade race metoo kavanaugh immigration mueller gun media trump {
		gen forsum_`t'`time' = resc_att_`t'`time' * `sigma_`t''
		replace forsum_`t'`time'=. if !(republican_polar==1 | democrat_polar==1)
		gen temp2_`t'`time'=temp_`t'`time'*`sigma_`t''
		drop temp_`t'`time'
	}
	gen issue_polar`time'=forsum_trade`time' + forsum_race`time' + forsum_metoo`time' + forsum_kavanaugh`time' + forsum_immigration`time' + forsum_mueller`time' + forsum_gun`time' + forsum_media`time' + forsum_trump`time'
	gen issue_polar_sigma1`time' = resc_att_trade`time' + resc_att_race`time' + resc_att_metoo`time' + resc_att_kavanaugh`time' + resc_att_immigration`time' + resc_att_mueller`time' + resc_att_gun`time' + resc_att_media`time' + resc_att_trump`time'
	gen iss_pol_all`time' = temp2_trade`time' + temp2_race`time' + temp2_metoo`time' + temp2_kavanaugh`time' + temp2_immigration`time' + temp2_mueller`time' + temp2_gun`time' + temp2_media`time' + temp2_trump`time'
}

drop forsum* temp2_*

* Belief polarization
foreach time in "" _b {
	forvalues x=5/19 {
		gen polar_news_`x'`time'=.
		replace polar_news_`x'`time' = 1 if news_knowledge_`x'`time'==1
		replace polar_news_`x'`time' = 0 if news_knowledge_`x'`time' == 0.5
		replace polar_news_`x'`time' = -1 if news_knowledge_`x'`time'==0
		summarize polar_news_`x'`time' if T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0) & (republican_polar==1 | democrat_polar==1)
		local sigma_`t' = r(sd)
		replace polar_news_`x'`time' = polar_news_`x'`time'/`sigma_`t''
		summarize polar_news_`x'`time' if democrat_polar==1 & T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
		local D = r(mean)
		summarize polar_news_`x'`time' if republican_polar==1 & T==0 & v<102 & v!=. & (price==102|price==0) & lowqual==0 & randomized==1 & !(price2!=0 & first_question_counts==1) & !(price2_2!=0 & first_question_counts==0)
		local R = r(mean)
		local recenter = (`D' + `R') * (-0.5)
		replace polar_news_`x'`time' = polar_news_`x'`time' + `recenter'
		if `D' > `R' {
			replace polar_news_`x'`time' = polar_news_`x'`time' * (-1)
		}
		replace polar_news_`x'`time' = (-1) * polar_news_`x'`time' if democrat_polar==1
	}
	gen belief_polar`time' = polar_news_5`time' + polar_news_6`time' + polar_news_7`time' + polar_news_8`time' + polar_news_9`time' + polar_news_10`time' + polar_news_11`time' + polar_news_12`time' + polar_news_13`time' + polar_news_14`time' + polar_news_15`time' + polar_news_16`time' + polar_news_17`time' + polar_news_18`time' + polar_news_19`time'
	replace belief_polar`time'=. if !(republican_polar==1 | democrat_polar==1)
}

drop polar_news*


*****************************
*** Subjective well-being ***
*****************************

foreach time in "" _b {
	gen swb_happy`time' = (swb_happiness`time' + swb_relhappiness`time')/2
	gen swb_swl`time' = swb_swl1`time' + swb_swl2`time' + swb_swl3`time'
	gen swb_lns`time' = (-1) * (swb_lnlns1`time' + swb_lnlns2`time' + swb_lnlns3`time')
}

*************************
** Post-experiment use **
*************************

* get days until reactivation
format enddate %tc
gen endline_day = dofc(enddate) - $start_experiment
replace endline_day = 42 if endline_day==.

gen days_until_react =.
forvalues j = 42/47 {
	forvalues i = `current_day' (-1) 42 {
		replace days_until_react = `i' - (`j'+1) if endline_day == `j' & enabled_some`i' == 1  & (`i'>`j')
	}
	replace days_until_react = `current_day' - (`j' + 1) if days_until_react==. & endline_day == `j'
}

gen time_to_reactivation = (-1) * log(days_until_react + 1)
gen clicked_timelimit_email=.
replace clicked_timelimit_email= -1 if clicked_timeuse == 1 & clicked_timeuse !=.
replace clicked_timelimit_email= 0 if clicked_timeuse == 0 & clicked_timeuse !=.
gen clicked_politics_email =.
replace clicked_politics_email =1 if clicked_politics ==1 & clicked_politics !=.
replace clicked_politics_email = 0 if clicked_politics==0 & clicked_politics !=.


*************************
** WTA **
*************************

gen v_bin = .
replace v_bin = 13 if v>=0&v<=25
replace v_bin = 38 if v>25&v<=50
replace v_bin = 63 if v>50&v<=75
replace v_bin = 88 if v>75&v<102
replace v_bin = 150 if v>=102&v<=200
replace v_bin = 250 if v>200&v<=300
replace v_bin = 400 if v>300&v<=500
replace v_bin = 550 if v>500 & v!=.
*replace v_bin = 350 if v>300&v<=400
*replace v_bin = 450 if v>400&v<=500

*********************************
** Define sample **
*********************************
gen sample_main=0
replace sample_main=1 if v<102 & v!=. // restrict to those with valuation less than $102
replace sample_main=0 if price!=102 & price!=0 // exclude those who got a price draw other than $0 or $102
replace sample_main=0 if (price2!=0 & first_question_counts==1) | (price2_2!=0 & first_question_counts==0) // exclude those with non-zero price draw that counts for endline
replace sample_main=0 if lowqual==1 // exclude low quality respondents
replace sample_main=0 if randomized==0 // exclude those who didn't make it to randomization stage in Midline

gen sample_wta=0
replace sample_wta=1 if v!=. // do NOT restrict to those with valuation less than $102
replace sample_wta=0 if price!=102 & price!=0 // exclude those who got a price draw other than $0 or $102
replace sample_wta=0 if (price2!=0 & first_question_counts==1) | (price2_2!=0 & first_question_counts==0) // exclude those with non-zero price draw that counts for endline
replace sample_wta=0 if lowqual==1 // exclude low quality respondents
replace sample_wta=0 if randomized==0 // exclude those who didn't make it to randomization stage in Midline

* indicate wave 1 and wave 2
		* wave 1 started with text messages on Sep 28th, 2018
		* wave 2 started with text messages on Oct 4th, 2018
		* wave 2 individuals all finished Baseline on or after Sep 28th, 2018
ren wave wave2
gen wave=.
replace wave=1 if endday_b<21455 & endday_b!=.
replace wave=2 if endday_b>=21455 & endday_b!=.
drop wave2

*********************************
** Set up endogenous vars **
*********************************
foreach time in "" _b {
	* winsorize minutes of FB use
	gen fb_min_wins`time' = fb_minutes`time'
	replace fb_min_wins`time' = 120 if fb_minutes`time' > 120 & fb_minutes`time'!=.
	** mobile minutes
	gen mobile_minutes_wins`time' = mobile_minutes`time'
	replace mobile_minutes_wins`time' = 120 if mobile_minutes`time' > 120 & mobile_minutes`time' !=.
	** news minutes
	gen read_news_wins`time' = read_news`time'
	replace read_news_wins`time' = 120 if read_news`time' > 120 & read_news`time' !=.
	* leisure activities
	foreach activity in sm oo tvm nns frf {
		gen leisure_`activity'_wins`time' = leisure_`activity'`time'
		if "`time'"== "_b" {
			replace leisure_`activity'_wins`time' = 120 if leisure_`activity'`time' == 150 | leisure_`activity'`time' == 240
		}
	}
}

* winsorize valuations of FB
local maxWTA = 1000
gen v_wins = v
replace v_wins = `maxWTA' if v>`maxWTA' & v!=.

* winsorize wta2 and wta3
gen wta3_wins=wta3
replace wta3_wins=`maxWTA' if wta3>`maxWTA' & wta3!=.
gen wta2_wins=wta2
replace wta2_wins=`maxWTA' if wta2>`maxWTA' & wta2!=.

gen H= fb_minutes_b/60
*replace H = 10/60 if H<10/60 & H!=. // winsorize at 10 minutes
replace H = 120/60 if H>120/60 & H!=. // winsorize at 120 minutes
gen D_H= H*D
gen T_H = T*H

* construct binary deactivation indicator for binary IV specification
gen D_binary=.
replace D_binary=1 if D>=0.9
replace D_binary=0 if D<0.9
replace D_binary=. if D==.

* replace "decide not to respond" income with median income ($)
gen hhld_inc_orig = hhld_inc
replace hhld_inc=. if T==.
egen hinc_median = median(hhld_inc)
replace hhld_inc = hinc_median if hhld_inc ==.n
replace hhld_inc = hhld_inc_orig if T==.
replace hhld_inc = hinc_median if hhld_inc_orig ==.n

*********************************
** Get sample weights **
*********************************
gen ageunder30 = cond(age<30&age!=.,1,0)
gen college = cond(educyears>=16,1,0)
gen hhld_inc_under50 = cond(hhld_inc < 50, 1, 0)
global WeightingVars = "hhld_inc_under50 college male white ageunder30 fb_min_wins_b"
local WeightingVarsMeans = "0.41 0.3312 0.4431 0.73 0.2552 45"
foreach sample in main wta {
	ebalance $WeightingVars, manualtargets(`WeightingVarsMeans') gen(weight_`sample'), if sample_`sample'==1
}

*********************************
** Get control variables for regressions **
*********************************

* strata based on everyone who is lowqual=0 and qualified=1
                *Above-median active browser,
                *(Fairly Often/Very Often get news from FB) vs. (Never/Rarely/Sometimes),
                *Fb_minutes (below median, above median),
                *Age (median),
                *Party affiliation (D,Ind,R)

gen medianvar= .
replace medianvar = fb_activepassive if lowqual==0 & qualified==1
egen fbactpass_median = median(medianvar)
gen fbactpass_above_median =.
replace fbactpass_above_median=0 if fb_activepassive<=fbactpass_median & fb_activepassive!=.
replace fbactpass_above_median=1 if fb_activepassive>fbactpass_median & fb_activepassive!=.
tab fbactpass_above_median if lowqual==0 & qualified==1
drop medianvar

gen medianvar= .
replace medianvar = fb_minutes_b if lowqual==0 & qualified==1
egen fbmin_median = median(medianvar)
gen fbmin_above_median =.
replace fbmin_above_median=0 if fb_minutes_b<=fbmin_median & fb_minutes_b!=.
replace fbmin_above_median=1 if fb_minutes_b>fbmin_median & fb_minutes_b!=.
tab fbmin_above_median if lowqual==0 & qualified==1
drop medianvar

gen news_fb_often = .
replace news_fb_often = 0 if news_source_6_b ==1 | news_source_6_b ==2  | news_source_6_b ==3
replace news_fb_often = 1 if news_source_6_b ==4 | news_source_6_b ==5
tab news_fb_often if lowqual==0 & qualified==1

gen medianvar= .
replace medianvar = yob_prescreen if lowqual==0 & qualified==1
egen medage = median(medianvar)
gen young=.
replace young = 0 if yob_prescreen<=medage & yob_prescreen!=.
replace young = 1 if yob_prescreen>medage & yob_prescreen!=.
tab young if lowqual==0 & qualified==1
drop medianvar

gen partyaffil=.
replace partyaffil=1 if repdem==3 | repdem==4 | repdem==5
replace partyaffil=2 if democrat==1
replace partyaffil=3 if republican==1
tab partyaffil if lowqual==0 & qualified==1

count if (young==. | fbactpass_above_median==. | fbmin_above_median==. | news_fb_often==. | partyaffil==.) & (lowqual==0 & qualified==1)

egen stratum = group(young fbactpass_above_median fbmin_above_median news_fb_often partyaffil)

ren young age_dummy
ren fbactpass_above_median activepassive_fb
ren fbmin_above_median minutes_fb
ren news_fb_often news_fb
ren partyaffil party_dummy

*****************************
******** Get HTE vars *******
*****************************

gen fb_active = fb_activepassive + fb_activepassive2 if sample_main==1
sum fb_active if sample_main==1, detail
local median_active = r(p50)
replace fb_active= fb_active>`median_active' if sample_main==1

sum v if v < 102 & sample_main==1, detail
local medianv = r(p50)
gen wta_median = .
replace wta_median = 1 if v < 102 & v > `medianv' & sample_main==1
replace wta_median = 0 if v < 102 & v < `medianv' & sample_main==1

sum weight_main if sample_main==1, detail
local medianweight = r(p50)
gen weight_more = .
replace weight_more = 1 if weight_main > `medianweight' & sample_main==1
replace weight_more = 0 if weight_main < `medianweight' & sample_main==1

tempfile temp
save `temp'

*******************************************************
******** Impute some baseline control variables *******
*******************************************************

** Create local lists of outcome variabels
include "../lib/stata/DefineVarsets.do"

* create indicator for whether variable is imputed
foreach vset in $vset_list {
	local vs_`vset'="`varset_`vset''"
	foreach yvar in `vs_`vset'' {
		foreach time in "" _b {
			gen `yvar'`time'_imp=0
		}
	}
}

** impute summary SMS variables at baseline for those who are missing
foreach v in happy lonely pos_emotion {
	replace `v'_sms_summary_b_imp=1 if `v'_sms_summary_b ==.
	summ `v'_sms_summary_b if sample_main==1
	replace `v'_sms_summary_b = r(mean) if `v'_sms_summary_b ==.
}

** impute swb_eurhappy_b for those that are missing in baseline (5 individuals)
forvalues x=4/7 {
	replace swb_eurhappsvy_`x'_b_imp=1 if swb_eurhappsvy_`x'_b ==.
	summ swb_eurhappsvy_`x'_b if sample_main==1
	replace swb_eurhappsvy_`x'_b = r(mean) if swb_eurhappsvy_`x'_b ==.
}

** impute mobile_minutes_b for those that are missing in baseline
foreach v in mobile_minutes_wins_b {
	replace `v'_imp=1 if `v' ==. & fb_app=="yes"
	summ `v' if sample_main==1
	replace `v' = r(mean) if `v' ==. & fb_app=="yes"
}

sum news_knowledge_score_b if T==0 & sample_main==1
gen news_score_norm_b = (news_knowledge_score_b-r(mean))/r(sd)
sum fake_news_score_b if T==0 & sample_main==1
gen fake_score_norm_b = (fake_news_score_b-r(mean))/r(sd)


***************************************
******** Normalize variables **********
***************************************

** Create local lists of outcome variabels
include "../lib/stata/DefineVarsets.do"

foreach vset in $vset_list {
		local vs_`vset'="`varset_`vset''"
		foreach yvar in `vs_`vset'' {
			foreach time in "" _b {
				capture gen `yvar'`time'=.
				capture gen `yvar'`time'_nn = `yvar'`time' // record original, non-normalized value
			}
			* Normalize variables to mean=0, sd=1 based on Endline control
			if ("`vset'" == "sub_news" & "`yvar'" != "tweets") | ("`vset'" == "sub_time" & "`yvar'" != "fb_min_wins") { // variables asked differently in BL and EL
				sum `yvar'_b if T==0 & sample_main==1
				replace `yvar'_b = (`yvar'_b-r(mean))/r(sd)
				sum `yvar' if T==0 & sample_main==1
				replace `yvar' = (`yvar'-r(mean))/r(sd)
			}
			else if "`yvar'" == "fb_deact_good" | "`yvar'" == "planned_use" | "`yvar'" == "clicked_timelimit_email" | "`yvar'" == "time_to_reactivation" | "`yvar'" == "clicked_politics_email" { // variables asked in EL only
				sum `yvar' if T==0 & sample_main==1
				replace `yvar' = (`yvar'-r(mean))/r(sd)
			}
			else { // variables asked identically in BL and EL
				sum `yvar' if T ==0 & sample_main==1
				replace `yvar'_b = (`yvar'_b-r(mean))/r(sd)
				replace `yvar' = (`yvar'-r(mean))/r(sd)
			}

		}
}

**********************************
******** Create indices **********
**********************************

include "code/MakeIndices.do"

******************************
****** SMS construction ******
******************************

tempfile maindata
save `maindata'

keep if sample_main==1
keep *_sms* id T fb_useti* weight* sample* D D_H D_binary T_H H stratum hour_sent* day_sent* fb_active

* Obtain 4-week average mean and sd in control for standardization later
foreach v in happy lonely pos_emotion {
	sum `v'_sms_summary if T==0
	local `v'_mean = r(mean)
	local `v'_sd = r(sd)
}

reshape long happy_sms lonely_sms mood_sms pos_emotion_sms hour_sent, i(id) j(day_numeric_sent)

replace lonely_sms=. if day_numeric_sent<10 // drop those days with erroneous loneliness question

gen answer_sms=0
replace answer_sms=1 if happy_sms!=. | lonely_sms!=. | mood_sms!=.

* Generate pre and post periods
gen pre=.
replace pre=1 if day_numeric_sent<14
gen post=.
replace post=1 if day_numeric_sent>15 & day_numeric_sent<42 & day_numeric_sent!=.

gen peak =.
gen offpeak =.
replace peak = 1 if hour_sent>=fb_usetime_min & hour_sent<fb_usetime_max & fb_usetime!=1 & hour_sent!=.
replace offpeak = 1 if (hour_sent<fb_usetime_min | hour_sent>=fb_usetime_max) & fb_usetime!=1 & hour_sent!=.

* Define weeks
gen week=.
forvalues x = 0/4 {
    replace week=`x' if day_numeric_sent >= 7 & day_numeric_sent < 7 * (`x' + 2) & week==.
}

* Compute averages
foreach v in happy lonely pos_emotion {
	gen temp1 = `v'_sms*peak
	gen temp2 = `v'_sms*offpeak

	forvalues x = 1/4 {
		bysort id: egen `v'_sms`x'=mean(`v'_sms) if week==`x'
		bysort id: egen `v'_sms_peak`x'=mean(temp1) if week==`x'
		bysort id: egen `v'_sms_offpeak`x'=mean(temp2) if week==`x'

		bysort id: egen `v'_sms`x'_b=mean(`v'_sms) if week==0
		bysort id: egen `v'_sms_peak`x'_b=mean(temp1) if week==0
		bysort id: egen `v'_sms_offpeak`x'_b=mean(temp1) if week==0
	}

	gen temp3 = temp1*pre
	gen temp4 = temp1*post
	gen temp5 = temp2*pre
	gen temp6 = temp2*post

	* compute means
	bysort id: egen `v'_sms_peak_b=mean(temp3)
	bysort id: egen `v'_sms_peak=mean(temp4)
	bysort id: egen `v'_sms_offpeak_b=mean(temp5)
	bysort id: egen `v'_sms_offpeak=mean(temp6)

	drop temp*
}

* reduce back to 1 observation per participant
drop *day*
keep id *_peak* *_offpeak* *1* *2* *3* *4*
collapse (firstnm) *sms*, by(id)

*** impute baseline with sample baseline mean if missing and
*** normalize outcome variables based on 4-week average in control
foreach v in happy lonely pos_emotion {
	foreach ptime in "" _peak _offpeak {
		foreach week in "" 1 2 3 4 {
			capture sum `v'_sms`ptime'`week'_b
			capture replace `v'_sms`ptime'`week'_b = r(mean) if `v'_sms`ptime'`week'_b==.
			foreach time in "" _b {
				capture replace `v'_sms`ptime'`week'`time' = (`v'_sms`ptime'`week'`time'-``v'_mean')/``v'_sd'
			}
		}
	}
}

* merge into main dataset
merge 1:1 id using `maindata'
drop _merge

* Normalize variables to mean=0, sd=1 based on Endline control
foreach v in happy lonely pos_emotion {
	foreach ptime in "" _peak _offpeak {
		foreach week in "" 1 2 3 4 {
			foreach time in "" _b {
				capture sum `v'_sms`ptime'`week'`time'_b if T==0 & sample_main==1
				capture replace `v'_sms`ptime'`week'`time'_b = (`v'_sms`ptime'`week'`time'_b-r(mean))/r(sd)
				capture replace `v'_sms`ptime'`week'`time' = (`v'_sms`ptime'`week'`time'-r(mean))/r(sd)
			}
		}
	}
}

*****************************
******** Save data  *********
*****************************

foreach var of varlist _all {
	cap _strip_labels `var'
}

include "../lib/stata/LabelVariables.do"

*** Prepare and save dataset with un-normalized variables and with indices
foreach vset in $vset_list {
	local vs_`vset'="`varset_`vset''"
	foreach yvar in `vs_`vset'' {
		foreach time in "" _b {
			ren `yvar'`time' `yvar'`time'_tmp
			ren `yvar'`time'_nn `yvar'`time' // rename non-normalized variables
			ren `yvar'`time'_tmp `yvar'`time'_nm // rename normalize variables
		}
	}
}

include "../lib/stata/LabelVariables.do"

ds, not(varlabel)
drop `r(varlist)'

compress

$save "$output/final_data.dta", replace
