*************** FMAX factor construction *******************************************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Time-series"

// monthly NYSE size breakpoints (median)
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (median) MktCap_Median = MktCap , by(month)
save MktCap_breakpoints,replace

// monthly (independent) MAX breakpoints
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 
collapse (p33) MAX_1 = MAX5 (p66) MAX_2 = MAX5, by(month)
save MAX_breakpoints,replace

// Form 6 portfolios based on breakpoints
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
merge m:1 month using MktCap_breakpoints
drop _merge
merge m:1 month using MAX_breakpoints
drop _merge
drop ExcessRet _Nobs1 _Nobs nobs_M total_obs Noise_bp tedrate mom BAB rmw cma smb hml

sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_S = .
replace pf_S = 1 if MktCap <= MktCap_Median & MAX5 <= MAX_1 & nvals==1 //small & lowest MAX
replace pf_S = 2 if MktCap > MktCap_Median & MAX5 <= MAX_1 & nvals==1 //large & lowest MAX
replace pf_S = 3 if MktCap <= MktCap_Median &  MAX5 > MAX_2 & nvals==1 //small & largest MAX
replace pf_S = 4 if MktCap > MktCap_Median &  MAX5 > MAX_2 & nvals==1 //large & largest MAX
replace pf_S=5 if pf_S==.

// portfolio assignment based on prior month
xtset PERMNO numobs
by PERMNO: replace pf_S = L.pf_S if  nvals ==0
by PERMNO: replace pf_S=L.pf_S if  nvals==1

// generate value-weighted pf returns
by month pf_S, sort: egen total_PF_mcap = total(MktCap) if nvals ==1
sort PERMNO date
gen weight = MktCap/total_PF_mcap if nvals ==1
xtset PERMNO numobs
by PERMNO: replace weight = L.weight if  nvals ==0
by PERMNO: replace weight = L.weight if  nvals==1

gen ret_vw = RET * weight
bys date pf_S: egen ret_pf = total(ret_vw)

collapse ret_pf month, by(date pf_S)
drop if pf_S==.|pf_S==5
gen ret_weights = 0.5*ret_pf if pf_S ==1 | pf_S == 2 // long position in low MAX
replace ret_weights = -0.5*ret_pf if pf_S ==3 | pf_S == 4 //short position in high MAX 
bys date: egen ret_FMAX = total(ret_weights) 
collapse ret_FMAX month, by(date)

gen log_ret_pf = log(1+ret_FMAX)
egen ret_FMAX_month = total(log_ret_pf), by(month) //monthly FMAX returns

collapse ret_FMAX_month, by(month)
gen ret_FMAX = exp(ret_FMAX_month)-1
drop ret_FMAX_month

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta"
rename ret_FMAX FMAX
drop _merge

save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", replace
