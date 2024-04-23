************** Beta decile sort analysis 60-m betas without ILLIQUI stocks *************************************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts"

****** Generate NYSE breakpoints 
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",clear
keep if EXCHCD == 1
sort PERMNO date
by PERMNO month, sort: keep if _n == _N 

collapse (p10) Beta1 = CAPM_beta (p20) Beta2 = CAPM_beta (p30) Beta3 = CAPM_beta (p40) Beta4 = CAPM_beta (p50) Beta5 = CAPM_beta (p60) Beta6 = CAPM_beta (p70) Beta7 = CAPM_beta (p80) Beta8 = CAPM_beta (p90) Beta9 = CAPM_beta, by(month)

save CAPM_beta_breakpoints_ILLIQUI,replace

************* Beta decile portfolios *******************
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep_ILLIQUI.dta",clear

merge m:1 month using CAPM_beta_breakpoints_ILLIQUI
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

xtset PERMNO numobs
by PERMNO: replace pf_B = L.pf_B if  nvals ==0
by PERMNO: replace pf_B=L.pf_B if  nvals==1
drop if pf_B==.
save Beta_sort_portfolios_ILLIQUI, replace

// generate daily portfolio returns (equal-weighted)
use Beta_sort_portfolios_ILLIQUI,clear
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta, by(date pf_B)

rename RET ret_PF
rename CAPM_beta beta_PF

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


******* descriptives for beta deciles based on 60-m ex-ante beta **********
use Beta_sort_portfolios_ILLIQUI,clear

merge m:m month PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CompanyDescriptives.dta"
drop if _merge == 2
drop _merge
merge m:m date PERMNO using  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep2.dta", keepusing(idiovol)
drop if _merge != 3
drop _merge
gen illiqui_d = abs(RET)*1000000/(VOL*PRC)
bys month PERMNO: egen illiqui = mean(illiqui_d)
collapse MktCap illiqui idiovol bm MAX5 pf_B, by(PERMNO month)
collapse MktCap idiovol bm illiqui MAX5, by(month pf_B)

replace MktCap=MktCap/1000 // in 1,000,000

bys pf_B: sum MktCap illiqui bm MAX5 idiovol
