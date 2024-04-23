************ Calculate monthly beta spread to be used as control variable in funding liquidity regression **************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Time-series"

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

****** BAB factor construction following Frazzini&Pedersen (2014) utilizing 60-m rolling beta estimates ***************
egen beta_month = mean(CAPM_beta), by(PERMNO month) // use average monthly beta estimates
gen log_ret = log(1+RET)
egen ret_month = total(log_ret), by(month PERMNO) //monthly returns
drop rf mktrf smb hml rmw cma mom

merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", keepusing(rf mktrf smb hml rmw cma mom)
drop if _merge != 3
drop _merge
gen exret_month = exp(ret_month)-1-(rf/100)
gen exret_m_month = mktrf/100
sort PERMNO date
by PERMNO month, sort: keep if _n == _N

egen median2=median(beta_month), by(month) //median average firm beta each month

// assign stocks to high or low beta portfolio depending on their average monthly beta relative to monthly median breakpoint
gen lowhigh="."
replace lowhigh="High" if beta_month>=median2 //short leg BAB portfolio
replace lowhigh="Low" if beta_month<median2 //long leg BAB portfolio
drop if lowhigh=="."

// weights using rank-weighting procedure
egen rank=rank(beta_month), by(month)
sort month rank 
egen zbarnum=total(rank), by(month) //higher rank, higher beta
egen zbardiv=max(rank), by(month)
gen zbar=zbarnum/zbardiv
	
//k and weights
gen z_zbar=rank-zbar
egen k=total(abs(z_zbar)), by(month)
replace k=k/2 
replace k=(1/k) 
gen weight=abs(z_zbar)*k
egen sumweightcheck=total(weight), by(month)
sum sumweightcheck  //needs to be 2 each month
keep PERMNO month rank weight lowhigh

// assignments to portfolio based on prior month
xtset PERMNO month
gen lagged_weight = L.weight 
bysort PERMNO (month): generate lagged_PF = lowhigh[_n-1] if month[_n-1]==month-1

save PFs_BAB_weighted, replace

// use portfolio assignments & weights to find beta spread
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear
 merge m:m PERMNO month using PFs_BAB_weighted
 drop if _merge !=3
 drop _merge
 
xtset PERMNO date
// calculate beta spread at portfolio formation (i.e. not the lagged values)
gen pfbetalow_pre=weight*CAPM_beta if lowhigh=="Low"
gen pfbetahigh_pre=weight*CAPM_beta if lowhigh=="High"
egen lowbeta_pre=total(pfbetalow_pre), by(date)
egen highbeta_pre=total(pfbetahigh_pre), by(date)
by lowhigh date, sort: keep if _n == _N
collapse lowbeta_pre highbeta_pre , by(month lowhigh)
gen beta_spread = (highbeta_pre-lowbeta_pre)/(highbeta_pre*lowbeta_pre)

by month, sort: keep if _n == _N
keep beta_spread month
save beta_spread_monthly, replace // to be used as control variable in funding liquidity regressions
