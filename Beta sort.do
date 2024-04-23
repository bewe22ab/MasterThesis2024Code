****************** General preparation for univariate portfolio sorts & beta-decile analysis **********************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts"

****** Generate NYSE breakpoints for three ex-ante beta measures and MAX

// 60-m beta
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 

collapse (p10) Beta1 = CAPM_beta (p20) Beta2 = CAPM_beta (p30) Beta3 = CAPM_beta (p40) Beta4 = CAPM_beta (p50) Beta5 = CAPM_beta (p60) Beta6 = CAPM_beta (p70) Beta7 = CAPM_beta (p80) Beta8 = CAPM_beta (p90) Beta9 = CAPM_beta, by(month)

save CAPM_beta_breakpoints,replace

// 60-m shrunk beta
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 

collapse (p10) Beta1 = CAPM_beta_sh (p20) Beta2 = CAPM_beta_sh (p30) Beta3 = CAPM_beta_sh (p40) Beta4 = CAPM_beta_sh (p50) Beta5 = CAPM_beta_sh (p60) Beta6 = CAPM_beta_sh (p70) Beta7 = CAPM_beta_sh (p80) Beta8 = CAPM_beta_sh (p90) Beta9 = CAPM_beta_sh, by(month)

save CAPM_beta_sh_breakpoints,replace

// 12-m beta
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 

collapse (p10) Beta1 = CAPM_beta2 (p20) Beta2 = CAPM_beta2 (p30) Beta3 = CAPM_beta2 (p40) Beta4 = CAPM_beta2 (p50) Beta5 = CAPM_beta2 (p60) Beta6 = CAPM_beta2 (p70) Beta7 = CAPM_beta2 (p80) Beta8 = CAPM_beta2 (p90) Beta9 = CAPM_beta2, by(month)

save CAPM_beta2_breakpoints,replace

// MAX
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 

collapse (p10) Max5_1 = MAX5 (p20) Max5_2 = MAX5 (p30) Max5_3 = MAX5 (p40) Max5_4 = MAX5 (p50) Max5_5 = MAX5 (p60) Max5_6 = MAX5 (p70) Max5_7 = MAX5 (p80) Max5_8 = MAX5 (p90) Max5_9 = MAX5, by(month)

save MAX5_breakpoints,replace

************** Beta decile sort analysis **********************************************************
// 60-month beta without shrinkage
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

merge m:1 month using CAPM_beta_breakpoints
drop _merge

//Assign to portfolios
sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_B = .
replace pf_B = 1 if CAPM_beta <= Beta1 & nvals==1 
replace pf_B = 10 if CAPM_beta < . & CAPM_beta > Beta9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_B = `p' if CAPM_beta > Beta`pl' & CAPM_beta <= Beta`p' & nvals==1 
}
// based on end of prior month
xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
drop if pf_B==.
save Beta_sort_portfolios, replace

// generate daily portfolio returns (equal-weighted)
use Beta_sort_portfolios,clear
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta, by(date pf_B)

rename RET ret_PF
rename CAPM_beta beta_PF

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B) // monthly portfolio returns
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom) // monthly factor returns
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

egen beta_pf_month = mean(beta_PF), by(pf_B month) // average portfolio ex-ante beta
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B date
by pf_B month, sort: keep if _n == _N
save Beta_sort_portfolios_subperiods, replace

tsset pf_B month
bys pf_B: newey exret_pf_month exret_m_month,lag(6) //CAPM regression
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6) //FFC-4 regression
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression
by pf_B: sum exret_pf_month beta_pf_month
bysort pf_B: ttest exret_pf_month==0

// Low-High Beta portfolio
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N

tsset month
newey Ret_LS_m exret_m_month,lag(6) //CAPM regression
newey Ret_LS_m exret_m_month smb hml mom,lag(6) //FFC-4 regression
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression
ttest Ret_LS_m==0


//Implement sub-period split:
******* Jan 2000 - Dec 2012 ******
use Beta_sort_portfolios_subperiods, clear
drop if month> 635
tsset pf_B month
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_B: sum exret_pf_month beta_pf_month
bysort pf_B: ttest exret_pf_month==0
// Low-High Beta portfolio
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

******* Jan 2013 - Dec 2023 ******
use Beta_sort_portfolios_subperiods, clear
tsset pf_B month
drop if month <= 635
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_B: sum exret_pf_month beta_pf_month
bysort pf_B: ttest exret_pf_month==0
// Low-High Beta portfolio
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



************* 60-month beta with shrinkage ****************************************************************
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

merge m:1 month using CAPM_beta_sh_breakpoints
drop _merge

//Assign to portfolios
sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
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
drop if pf_B==.
save Beta_sh_sort_portfolios, replace

use Beta_sh_sort_portfolios,clear
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta_sh, by(date pf_B)

rename RET ret_PF
rename CAPM_beta_sh beta_PF

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
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B date
by pf_B month, sort: keep if _n == _N
tsset pf_B month
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_B: sum exret_pf_month beta_pf_month
bysort pf_B: ttest exret_pf_month==0


// Low-High Beta portfolio
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


************** 12-month beta without shrinkage **********************************
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

merge m:1 month using CAPM_beta2_breakpoints
drop _merge

//Assign to portfolios
sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
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
drop if pf_B==.
save Beta2_sort_portfolios, replace

use Beta2_sort_portfolios,clear
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta2, by(date pf_B)

rename RET ret_PF
rename CAPM_beta2 beta_PF

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
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B date
by pf_B month, sort: keep if _n == _N
tsset pf_B month
bys pf_B: newey exret_pf_month exret_m_month,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_B: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_B: sum exret_pf_month beta_pf_month
bysort pf_B: ttest exret_pf_month==0

// Low-High Beta portfolio
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


******* descriptives for beta deciles based on 60-m ex-ante beta ***************************
use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CompanyDescriptives.dta", clear
gen month = mofd(public_date)
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CompanyDescriptives.dta", replace

use Beta_sort_portfolios,clear

merge m:m month PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CompanyDescriptives.dta"
drop if _merge == 2
drop _merge
merge m:m date PERMNO using  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep2.dta", keepusing(idiovol)
drop if _merge != 3
drop _merge
gen illiqui_d = abs(RET)*1000000/(VOL*PRC)
bys month PERMNO: egen illiqui = mean(illiqui_d)
collapse MktCap illiqui idiovol bm pe_exi MAX5 pf_B, by(PERMNO month)
collapse MktCap idiovol bm pe_exi illiqui MAX5, by(month pf_B)
replace MktCap=MktCap/1000 // in 1,000,000

bys pf_B: sum MktCap illiqui bm pe_exi MAX5 idiovol
