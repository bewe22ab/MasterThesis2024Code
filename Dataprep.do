******Creation of data files for daily stock returns and factor returns and macro-variables *********************************

************** FACTOR RETURNS, MACRO-VARIABLES ETC **************************************

// FF 5-factors:
todate v1, generate(date) pattern(YYYYMMDD)
drop v1
gen fakedate=date
format fakedate %11.0g

// BAB factor:
keep DATE USA
gen fakedate = date(DATE,"MDY")
format fakedate %11.0g
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta"
drop DATE _merge v1 date
rename USA BAB

// Momentum
gen fakedate = date(v1,"YMD")
drop v1
format fakedate %11.0g
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta"
drop if _merge == 1
drop _merge

// TED-Spread
gen fakedate = date(date,"YMD")
format fakedate %11.0g
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta"
drop _merge


// Noise
tostring Date, replace
gen fakedate = Date
destring fakedate, replace
format fakedate %11.0g
drop if fakedate ==.
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta"
sort fakedate
drop _merge

save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta", replace

// US-REC monthly:
gen fakedate = date(date,"YMD")
format fakedate %11.0g
gen month =mofd(fakedate)

// CPI:
gen fakedate = date(date,"YMD")
format fakedate %11.0g
gen month =mofd(fakedate)
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop _merge

//BAB monthly:
keep DATE USA
gen fakedate = date(DATE,"MDY")
format fakedate %11.0g
gen month =mofd(fakedate)
rename USA BAB
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop _merge

//FF-5 monthly:
gen date = date(v1,"YM")
drop v1
gen month =mofd(date)
drop date
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop DATE _merge
sort month
drop if _n > 1117

//mom monthly:
gen date = date(v1,"YM")
drop v1
gen month =mofd(date)
drop date
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop _merge
sort month
drop if month < -349 | month ==.
drop fakedate
destring mktrf smb hml rmw cma rf mom, replace

// sentiment monthly:

todate yearmo, generate(date) pattern(YYYYMM)
format date %11.0g
gen month =date
drop date yearmo
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop _merge
sort month

//term spread monthly
gen fakedate = date(date,"YMD")
gen month =mofd(fakedate)
drop date fakedate
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
drop _merge

save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", replace


// Goyal Macro-data monthly:
todate yyyymm, generate(date) pattern(YYYYMM)
format date %11.0g
gen month =date
drop date yyyymm
drop if month < 100
destring bm tbl AAA BAA lty ntis Rfree infl ltr corpr svar csp CRSP_SPvw CRSP_SPvwx,replace
drop if month ==.
drop csp
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/GoyalMonthlyMacro.dta", replace



********** DAILY STOCK RETURNS ******************************************
// basic clean-up of daily return data set from CRSP
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000.dta", clear

duplicates report PERMNO date
replace PRC = abs(PRC)	// mid-quotes
drop if RET == .b 
drop if RET == .c 
drop if RET ==. 

gen fakedate=date
format fakedate %11.0g
gen month =mofd(fakedate)

gen MktCap=SHROUT*PRC

egen numobs=rank(fakedate), by(PERMNO)

save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000.dta", replace
 
 // deletion of penny stocks where prior month price is below $5
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
tsset PERMNO month
by PERMNO: gen marked_del = 1 if L.PRC <5
keep PERMNO month marked_del
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily_5dollar", replace
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000.dta", clear
merge m:m PERMNO month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily_5dollar"
drop if marked_del ==1
drop marked_del _merge
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", replace

// only include common shares with SHRCD 10 or 11
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
drop if date ==.
merge m:m PERMNO date using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/SHRCD.dta"
drop if _merge != 3
keep if SHRCD == 10 | SHRCD ==11 // include only common shares with code 10 or 11
drop _merge SHRCD
tab EXCHCD // don't limit analysis to NYSE,AMEX & NASDAQ but include all exchanges

// include risk factors etc in the data set for later analyses
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
merge m:m fakedate using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta"
rename USA BAB
drop if _merge !=3
drop _merge
gen MKTEX=mktrf/100
gen ExcessRet=RET-(rf/100)
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",replace

// summary statistics:
sum month
sort PERMNO date
by PERMNO month, sort: keep if _n == _N
egen count1 = rank(PERMNO), by(month)
sort month count1
egen companies = max(count1), by(month)
by month, sort: keep if _n == _N
sum companies // monthly average of number of companies
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
by PERMNO, sort: keep if _n == _N //total number of companies

// generate ex-ante CAPM-Betas
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
bys PERMNO: asreg ExcessRet MKTEX, wind(month -60 0) se newey(6) //60 month rolling regression
rename _b_MKTEX CAPM_beta
rename _Nobs _Nobs1
drop _R2 _adjR2 _b_cons _se_MKTEX _se_cons
bys PERMNO: asreg ExcessRet MKTEX, wind(month -12 0) se newey(6) //12 month rolling regression
rename _b_MKTEX CAPM_beta2
drop _R2 _adjR2 _b_cons _se_MKTEX _se_cons
gen CAPM_beta_sh = 0.6*CAPM_beta+0.4 //shrunk 60-month betas
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",replace

// MAX with 5 largest returns per month
sort PERMNO month ExcessRet
by PERMNO month: gen MAX=ExcessRet if _n > (_N-5)
by PERMNO month, sort: egen MAX2 = mean(MAX)
drop MAX
rename MAX2 MAX5

egen nobs_M=rank(fakedate), by(PERMNO month)
egen total_obs = max(nobs_M), by (PERMNO month)
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",replace

// constraint minimum of observations for MAX and Beta analyses
drop if total_obs<15
drop if _Nobs < 200 | _Nobs ==. 
drop if _Nobs1 < 200 | _Nobs1 ==. 
drop TICKER vwretd Noise_bp tedrate BAB _Nobs1 _Nobs nobs_M total_obs
save  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",replace

// summary statistics after imposing requirements:
sum month
sort PERMNO date
by PERMNO month, sort: keep if _n == _N
egen count1 = rank(PERMNO), by(month)
sort month count1
egen companies = max(count1), by(month)
by month, sort: keep if _n == _N
sum companies
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
by PERMNO, sort: keep if _n == _N
