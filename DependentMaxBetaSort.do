***************** Dependent bivariate portfolio sort analyses ****************************************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Bivariate sorts"

************* Control for Beta (60m rolling window) *******************************************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// find NYSE beta breakpoints to look at mid range between p33 and p67
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Beta45 = CAPM_beta (p66) Beta55 = CAPM_beta, by(month)
save Dep_CAPM_beta_breakpoints,replace

// assign to beta portfolios based on prior month and only keep mid range stocks
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_CAPM_beta_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta >= Beta45 & nvals==1 & CAPM_beta <= Beta55

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
save Double_sorted_MB_portfolios, replace

// determine dependent decile MAX breakpoints
keep if EXCHCD == 1 & CAPM_beta >= Beta45 & CAPM_beta <= Beta55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Max5_1 = MAX5 (p20) Max5_2 = MAX5 (p30) Max5_3 = MAX5 (p40) Max5_4 = MAX5 (p50) Max5_5 = MAX5 (p60) Max5_6 = MAX5 (p70) Max5_7 = MAX5 (p80) Max5_8 = MAX5 (p90) Max5_9 = MAX5, by(month)
save Dep_MAX5_breakpoints,replace

// assign to MAX decile portfolios
use Double_sorted_MB_portfolios, clear
merge m:1 month using Dep_MAX5_breakpoints
drop _merge

sort PERMNO date
gen byte pf_M = .
replace pf_M = 1 if MAX5 <= Max5_1 & nvals==1 
replace pf_M = 10 if MAX5 < . & MAX5 > Max5_9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_M = `p' if MAX5 > Max5_`pl' & MAX5 <= Max5_`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
keep if pf_B==1 & pf_M !=.
drop Max5_1 Max5_3 Max5_2 Max5_4 Max5_5 Max5_6 Max5_7 Max5_8 Max5_9 Beta45 Beta55 _Nobs1 _Nobs nobs_M
save Double_sorted_MB_portfolios, replace

// Calculate returns for decile portfolios
use Double_sorted_MB_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta MAX5, by(date pf_M)

rename RET ret_PF
rename CAPM_beta beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_M)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_M month) //average ex-ante beta
egen max_pf_month = mean(av_MAX5_pf), by(pf_M month) //average ex-ante MAX
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_M date
by pf_M month, sort: keep if _n == _N

// Regressions for returns
tsset pf_M month 
bys pf_M: newey exret_pf_month exret_m_month,lag(6) //CAPM
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6) //FFC-4
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6) //FFC-6

by pf_M, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_M: ttest exret_pf_month==0


// Low-High MAX

gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N
save Dep_sort_max, replace

drop if month > 635
tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0


************* Control for Beta (12m rolling window) *******************************************************

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// find NYSE beta breakpoints to look at mid range between p33 and p67
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Beta45 = CAPM_beta2 (p66) Beta55 = CAPM_beta2, by(month)
save Dep_CAPM_beta2_breakpoints,replace

// assign to beta portfolios based on prior month and only keep mid range stocks
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_CAPM_beta2_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta2 >= Beta45 & nvals==1 & CAPM_beta2 <= Beta55

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
save Double_sorted_MB_portfolios, replace

// determine dependent decile MAX breakpoints
keep if EXCHCD == 1 & CAPM_beta2 >= Beta45 & CAPM_beta2 <= Beta55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Max5_1 = MAX5 (p20) Max5_2 = MAX5 (p30) Max5_3 = MAX5 (p40) Max5_4 = MAX5 (p50) Max5_5 = MAX5 (p60) Max5_6 = MAX5 (p70) Max5_7 = MAX5 (p80) Max5_8 = MAX5 (p90) Max5_9 = MAX5, by(month)
save Dep_MAX5_breakpoints,replace

// Assign to portfolios
use Double_sorted_MB_portfolios, clear
merge m:1 month using Dep_MAX5_breakpoints
drop _merge

sort PERMNO date
gen byte pf_M = .
replace pf_M = 1 if MAX5 <= Max5_1 & nvals==1 
replace pf_M = 10 if MAX5 < . & MAX5 > Max5_9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_M = `p' if MAX5 > Max5_`pl' & MAX5 <= Max5_`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
keep if pf_B==1 & pf_M !=.
drop Max5_1 Max5_3 Max5_2 Max5_4 Max5_5 Max5_6 Max5_7 Max5_8 Max5_9 Beta45 Beta55 _Nobs1 _Nobs nobs_M
save Double_sorted_MB_portfolios, replace

// Calculate returns for decile portfolios
use Double_sorted_MB_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta2 MAX5, by(date pf_M)

rename RET ret_PF
rename CAPM_beta2 beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_M)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_M month)
egen max_pf_month = mean(av_MAX5_pf), by(pf_M month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_M date
by pf_M month, sort: keep if _n == _N

// Return regressions
tsset pf_M month 
bys pf_M: newey exret_pf_month exret_m_month,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6) 
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)

by pf_M, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_M: ttest exret_pf_month==0


// Low-High MAX
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N
save Dep_sort_max, replace

drop if month > 635
tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0

************* Control for Beta (60m shrunk) *******************************************************

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// find NYSE beta breakpoints to look at mid range between p33 and p67
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Beta45 = CAPM_beta_sh (p66) Beta55 = CAPM_beta_sh, by(month)
save Dep_CAPM_beta_sh_breakpoints,replace

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_CAPM_beta_sh_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta_sh >= Beta45 & nvals==1 & CAPM_beta_sh <= Beta55

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
save Double_sorted_MB_portfolios, replace

// MAX breakpoints
keep if EXCHCD == 1 & CAPM_beta_sh >= Beta45 & CAPM_beta_sh <= Beta55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Max5_1 = MAX5 (p20) Max5_2 = MAX5 (p30) Max5_3 = MAX5 (p40) Max5_4 = MAX5 (p50) Max5_5 = MAX5 (p60) Max5_6 = MAX5 (p70) Max5_7 = MAX5 (p80) Max5_8 = MAX5 (p90) Max5_9 = MAX5, by(month)
save Dep_MAX5_breakpoints,replace

// Portfolio assignment
use Double_sorted_MB_portfolios, clear
merge m:1 month using Dep_MAX5_breakpoints
drop _merge

sort PERMNO date
gen byte pf_M = .
replace pf_M = 1 if MAX5 <= Max5_1 & nvals==1 
replace pf_M = 10 if MAX5 < . & MAX5 > Max5_9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_M = `p' if MAX5 > Max5_`pl' & MAX5 <= Max5_`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
keep if pf_B==1 & pf_M !=.
drop Max5_1 Max5_3 Max5_2 Max5_4 Max5_5 Max5_6 Max5_7 Max5_8 Max5_9 Beta45 Beta55 _Nobs1 _Nobs nobs_M
save Double_sorted_MB_portfolios, replace

//Portfolio returns
use Double_sorted_MB_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta_sh MAX5, by(date pf_M)

rename RET ret_PF
rename CAPM_beta_sh beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_M)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_M month)
egen max_pf_month = mean(av_MAX5_pf), by(pf_M month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_M date
by pf_M month, sort: keep if _n == _N

// Return regressions
tsset pf_M month 
bys pf_M: newey exret_pf_month exret_m_month,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6) 
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)

by pf_M, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_M: ttest exret_pf_month==0


// Low-High MAX
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N
save Dep_sort_max, replace
drop if month > 635

tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0



************* Control for MAX (with 60m rolling window betas) *******************************************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// NYSE MAX breakpoints to find mid between p33 and p67
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Max45 = MAX5 (p66) Max55 = MAX5, by(month)
save Dep_max_beta_breakpoints,replace

// only keep mid MAX stocks
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_max_beta_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_M = .
replace pf_M = 1 if MAX5 >= Max45 & nvals==1 & MAX5 <= Max55

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
save Double_sorted_BM_portfolios, replace

// Find dependent beta breakpoints
keep if EXCHCD == 1 & MAX5 >= Max45 &  MAX5 <= Max55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Beta1 = CAPM_beta (p20) Beta2 = CAPM_beta (p30) Beta3 = CAPM_beta (p40) Beta4 = CAPM_beta (p50) Beta5 = CAPM_beta (p60) Beta6 = CAPM_beta (p70) Beta7 = CAPM_beta (p80) Beta8 = CAPM_beta (p90) Beta9 = CAPM_beta, by(month)
save Dep_Beta_breakpoints,replace

// Portfolio assignment
use Double_sorted_BM_portfolios, clear
merge m:1 month using Dep_Beta_breakpoints
drop _merge

sort PERMNO date
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta <= Beta1 & nvals==1 
replace pf_B = 10 if CAPM_beta < . & CAPM_beta > Beta9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_B = `p' if CAPM_beta > Beta`pl' & CAPM_beta <= Beta`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
keep if pf_M==1 & pf_B !=.
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9 Max45 Max55 _Nobs1 _Nobs nobs_M
save Double_sorted_BM_portfolios, replace

// Calculate portfolio returns
use Double_sorted_BM_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta MAX5, by(date pf_B)

rename RET ret_PF
rename CAPM_beta beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_B month)
egen max_pf_month = mean(av_MAX5_pf), by(pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_B date
by pf_B month, sort: keep if _n == _N

// return regressions
tsset pf_B month 
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6) 
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)

by pf_B, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_B: ttest exret_pf_month==0

// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N

tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0


************* Control for MAX (with 12m rolling window betas) *******************************************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// MAX breakpoints
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Max45 = MAX5 (p66) Max55 = MAX5, by(month)
save Dep_max_beta_breakpoints,replace

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_max_beta_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_M = .
replace pf_M = 1 if MAX5 >= Max45 & nvals==1 & MAX5 <= Max55

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
save Double_sorted_BM_portfolios, replace

// dependent beta breakpoints
keep if EXCHCD == 1 & MAX5 >= Max45 &  MAX5 <= Max55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Beta1 = CAPM_beta2 (p20) Beta2 = CAPM_beta2 (p30) Beta3 = CAPM_beta2 (p40) Beta4 = CAPM_beta2 (p50) Beta5 = CAPM_beta2 (p60) Beta6 = CAPM_beta2 (p70) Beta7 = CAPM_beta2 (p80) Beta8 = CAPM_beta2 (p90) Beta9 = CAPM_beta2, by(month)
save Dep_Beta2_breakpoints,replace

// portfolio assignment
use Double_sorted_BM_portfolios, clear
merge m:1 month using Dep_Beta2_breakpoints
drop _merge

sort PERMNO date
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta2 <= Beta1 & nvals==1 
replace pf_B = 10 if CAPM_beta2 < . & CAPM_beta2 > Beta9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_B = `p' if CAPM_beta2 > Beta`pl' & CAPM_beta2 <= Beta`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
keep if pf_M==1 & pf_B !=.
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9 Max45 Max55 _Nobs1 _Nobs nobs_M
save Double_sorted_BM_portfolios, replace

//Double sorted portfolio returns
use Double_sorted_BM_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta2 MAX5, by(date pf_B)

rename RET ret_PF
rename CAPM_beta2 beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_B month)
egen max_pf_month = mean(av_MAX5_pf), by(pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_B date
by pf_B month, sort: keep if _n == _N

// Return regressions
tsset pf_B month 
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6) 
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)

by pf_B, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_B: ttest exret_pf_month==0

// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N

tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0



************* Control for MAX (with 60m rolling window shrunk betas) *******************************************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear

// MAX Breakpoints
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse  (p33) Max45 = MAX5 (p66) Max55 = MAX5, by(month)
save Dep_max_beta_breakpoints,replace

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta", clear
merge m:1 month using Dep_max_beta_breakpoints
drop _merge

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_M = .
replace pf_M = 1 if MAX5 >= Max45 & nvals==1 & MAX5 <= Max55

xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
save Double_sorted_BM_portfolios, replace

// Dependent beta breakpoints
keep if EXCHCD == 1 & MAX5 >= Max45 &  MAX5 <= Max55
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p10) Beta1 = CAPM_beta_sh (p20) Beta2 = CAPM_beta_sh (p30) Beta3 = CAPM_beta_sh (p40) Beta4 = CAPM_beta_sh (p50) Beta5 = CAPM_beta_sh (p60) Beta6 = CAPM_beta_sh (p70) Beta7 = CAPM_beta_sh (p80) Beta8 = CAPM_beta_sh (p90) Beta9 = CAPM_beta_sh, by(month)
save Dep_Beta_breakpoints,replace

// Portfolio assignment
use Double_sorted_BM_portfolios, clear
merge m:1 month using Dep_Beta_breakpoints
drop _merge

sort PERMNO date
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta_sh <= Beta1 & nvals==1 
replace pf_B = 10 if CAPM_beta_sh < . & CAPM_beta_sh > Beta9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_B = `p' if CAPM_beta_sh > Beta`pl' & CAPM_beta_sh <= Beta`p' & nvals==1 
}

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
keep if pf_M==1 & pf_B !=.
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9 Max45 Max55 _Nobs1 _Nobs nobs_M
save Double_sorted_BM_portfolios, replace

//Double sorted portfolio returns
use Double_sorted_BM_portfolios, clear
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta_sh MAX5, by(date pf_B)

rename RET ret_PF
rename CAPM_beta_sh beta_PF
rename MAX5 av_MAX5_pf

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B)
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_pf_month = exp(ret_pf_month)-1-(rf/100)
gen exret_m_month = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100
replace rf = rf/100

egen beta_pf_month = mean(beta_PF), by(pf_B month)
egen max_pf_month = mean(av_MAX5_pf), by(pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF av_MAX5_pf

sort pf_B date
by pf_B month, sort: keep if _n == _N

// return regressions
tsset pf_B month 
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6) 
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)

by pf_B, sort: sum exret_pf_month beta_pf_month max_pf_month
bysort  pf_B: ttest exret_pf_month==0

// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N

tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0
