************** MAX decile sort analysis **********************************************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts"

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

merge m:1 month using MAX5_breakpoints
drop _merge

//Assign to portfolios
sort PERMNO date
by PERMNO month, sort: gen nvals=_n==_N
gen byte pf_M = .
replace pf_M = 1 if MAX5 <= Max5_1 & nvals==1 
replace pf_M = 10 if MAX5 < . & MAX5 > Max5_9 & nvals==1 
forvalues p = 2/9 {
	local pl = `p'-1
	replace pf_M = `p' if MAX5 > Max5_`pl' & MAX5 <= Max5_`p' & nvals==1 
}
// based on end of prior month
xtset PERMNO numobs
by PERMNO: replace pf_M = L.pf_M if  nvals ==0
by PERMNO: replace pf_M=L.pf_M if  nvals==1
drop if pf_M==.
save MAX5_sort_portfolios, replace

// generate daily portfolio returns (equal-weighted)
use MAX5_sort_portfolios,clear
drop Max5_1 Max5_3 Max5_2 Max5_4 Max5_5 Max5_6 Max5_7 Max5_8 Max5_9
collapse RET mktrf month rf smb hml rmw cma mom MAX5, by(date pf_M)

rename RET ret_PF
rename MAX5 MAX_PF

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_M) // monthly portfolio returns
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

egen MAX_pf_month = mean(MAX_PF), by(pf_M month)
drop ret_PF mktrf ret_pf_month log_ret_pf MAX_PF

sort pf_M date
by pf_M month, sort: keep if _n == _N
save Max_sort_portfolios_subperiods, replace

tsset pf_M month
bys pf_M: newey exret_pf_month exret_m_month,lag(6) //CAPM regression
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6) //FFC-4 regression
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression
by pf_M: sum exret_pf_month MAX_pf_month
bysort pf_M: ttest exret_pf_month==0

// Low-High MAX portfolio
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
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
use Max_sort_portfolios_subperiods, clear
drop if month> 635
tsset pf_M month
bys pf_M: newey exret_pf_month exret_m_month,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_M: sum exret_pf_month MAX_pf_month
bysort pf_M: ttest exret_pf_month==0
// Low-High MAX portfolio
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N
tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0

******* Jan 2013 - Dec 2023 ******
use Max_sort_portfolios_subperiods, clear
drop if month <= 635
tsset pf_M month
bys pf_M: newey exret_pf_month exret_m_month,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml mom,lag(6)
bys pf_M: newey exret_pf_month exret_m_month smb hml rmw cma mom,lag(6)
by pf_M: sum exret_pf_month MAX_pf_month
bysort pf_M: ttest exret_pf_month==0
// Low-High MAX portfolio
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==10
gen Ret_LS = exret_pf_month * LS_Ind
by month, sort: egen Ret_LS_m = total(Ret_LS)
by month, sort: keep if _n == _N
tsset month
newey Ret_LS_m exret_m_month,lag(6)
newey Ret_LS_m exret_m_month smb hml mom,lag(6)
newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6)
ttest Ret_LS_m==0
