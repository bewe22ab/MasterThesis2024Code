************* Data preparation by excluding highly illiquid stocks ********************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000.dta", clear
sort PERMNO date
gen illiqui_d = abs(RET)*1000000/(VOL*PRC)
bys month PERMNO: egen illiqui = mean(illiqui_d)
by PERMNO month, sort: keep if _n == _N 
sum illiqui, detail
bys month: egen illiqui_bp = pctile(illiqui), p(90) //monthly breakpoint for percentile 90
gen illiquid = 1 if illiqui > illiqui_bp
tsset PERMNO month
by PERMNO: gen marked_del = 1 if L.PRC <5 | L.illiquid ==1 // exclude penny stocks and illiquid stocks
keep PERMNO month marked_del
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily_5dollarILLIQUI", replace
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000.dta", clear
merge m:m PERMNO month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily_5dollarILLIQUI"
drop if marked_del ==1
drop marked_del _merge
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta", replace


// only include common shares with SHRCD 10 or 11
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",clear
drop if date ==.
merge m:m PERMNO date using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/SHRCD.dta"
drop if _merge != 3
keep if SHRCD == 10 | SHRCD ==11 // include only common shares with code 10 or 11
drop _merge SHRCD
tab EXCHCD // we don't limit analysis to NYSE,AMEX & NASDAQ but include all exchanges

// include risk factors etc in the data set for later analyses
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",clear
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta", force
rename USA BAB
drop if _merge !=3
drop _merge
gen MKTEX=mktrf/100
gen ExcessRet=RET-(rf/100)
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",replace


// generate ex-ante 60-m CAPM-Betas
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta", clear
bys PERMNO: asreg ExcessRet MKTEX, wind(month -60 0) se newey(6) //60 month rolling regression
rename _b_MKTEX CAPM_beta
rename _Nobs _Nobs1
drop _R2 _adjR2 _b_cons _se_MKTEX _se_cons
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",replace

// MAX with 5 largest returns per month
sort PERMNO month ExcessRet
by PERMNO month: gen MAX=ExcessRet if _n > (_N-5)
by PERMNO month, sort: egen MAX2 = mean(MAX)
drop MAX
rename MAX2 MAX5


egen nobs_M=rank(fakedate), by(PERMNO month)
egen total_obs = max(nobs_M), by (PERMNO month)
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",replace

// constraint minimum of observations for MAX and Beta analyses
drop if total_obs<15
drop if _Nobs < 200 | _Nobs ==. 
drop if _Nobs1 < 200 | _Nobs1 ==. 
drop TICKER vwretd Noise_bp tedrate BAB _Nobs1 _Nobs nobs_M total_obs

save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",replace

