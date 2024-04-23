*********** Collection of various time-series analyses using BAB factor returns *******************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Time-series"

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000monthly.dta", clear
drop if month == 768

gen monthly_date = month
format monthly_date %tm
drop date
drop if month < 42
tsset monthly_date
twoway (tsline BAB), ytitle(`"Monthly BAB return"') ttitle(`"Month"') // time-series graph of BAB returns
twoway (tsline FMAX) if month >= 480, ytitle(`"Monthly FMAX return"') ttitle(`"YearMonth"') // time-series graph of FMAX returns

******* cumulative returns *********************

tsset monthly_date
gen cum_ret = 1 if month == 480
replace cum_ret = L.cum_ret*(1+BAB) if month > 480
gen cum_ret_smb = 1 if month == 480
replace cum_ret_smb = L.cum_ret_smb*(1+smb/100) if month > 480
gen cum_ret_hml = 1 if month == 480
replace cum_ret_hml = L.cum_ret_hml*(1+hml/100) if month > 480
gen cum_ret_cma = 1 if month == 480
replace cum_ret_cma = L.cum_ret_cma*(1+cma/100) if month > 480
gen cum_ret_rmw = 1 if month == 480
replace cum_ret_rmw = L.cum_ret_rmw*(1+rmw/100) if month > 480
gen cum_ret_mkt = 1 if month == 480
replace cum_ret_mkt = L.cum_ret_mkt*(1+mkt/100+rf/100) if month > 480
gen cum_ret_mom = 1 if month == 480
replace cum_ret_mom = L.cum_ret_mom*(1+mom/100) if month > 480

twoway (tsline cum_ret cum_ret_hml cum_ret_smb cum_ret_cma cum_ret_rmw cum_ret_mkt cum_ret_mom) if month >= 480 // graph for cumulative factor returns

gen MKTEX = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
replace mom = mom/100

***** BAB factor return time-series regressions ************
tsset month 
newey BAB MKTEX, lag(6) //CAPM
newey BAB MKTEX smb hml mom, lag(6) //FFC-4
newey BAB MKTEX smb hml mom rmw cma, lag(6) //FFC-6

correlate BAB FMAX smb hml mom rmw cma MKTEX // factor correlation


****** Summary statistics *******************************
sum BAB, detail
sum FMAX, detail
sum BAB MKTEX FMAX smb hml mom rmw cma if month >= 480, detail
tsset month 
newey BAB MKTEX if month >= 480 , lag(6)
newey smb MKTEX if month >= 480 , lag(6)
newey hml MKTEX if month >= 480 , lag(6)
newey mom MKTEX if month >= 480 , lag(6)
newey rmw MKTEX if month >= 480 , lag(6)
newey cma MKTEX if month >= 480 , lag(6)


******* Stanhope (2016) - Recession classification ********************
drop if usrec ==.
tsset month
replace usrec =2 if usrec==1
gen post_rec = 3 if L.usrec ==2 | L2.usrec ==2 | L3.usrec ==2 | L4.usrec ==2 | L5.usrec ==2 | L6.usrec ==2 | L7.usrec ==2 | L8.usrec ==2 | L9.usrec ==2 | L10.usrec ==2 | L11.usrec ==2 | L12.usrec ==2 
replace post_rec = 0 if post_rec ==.
gen pre_rec = 1 if F.usrec ==2 | F2.usrec ==2 | F3.usrec ==2 | F4.usrec ==2 | F5.usrec ==2 | F6.usrec ==2 | F7.usrec ==2 | F8.usrec ==2 | F9.usrec ==2 | F10.usrec ==2 | F11.usrec ==2 | F12.usrec ==2 
replace pre_rec = 0 if pre_rec ==.
gen us_rec_levels = post_rec + pre_rec if usrec ==0
replace us_rec_levels = usrec if usrec ==2
drop post_rec pre_rec 
bys us_rec_levels: sum BAB FMAX
bys us_rec_levels: correlate BAB FMAX smb hml mom rmw cma MKTEX

******* Cross-sectional MAX-Beta correlation ***************************
merge m:1 month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/BM_corr_months.dta"
drop _merge
drop if corr_BM ==.

bys high: sum BAB
bysort us_rec_levels high: sum BAB

*** calculate yearly inflation *******************
tsset month
gen inflation = (cpiaucsl/L12.cpiaucsl)-1

****** Analysis related to funding liquidity **************************
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/funding_liquidity_monthly.dta"
drop _merge
merge m:m month using beta_spread_monthly
drop if tedrate ==.
drop _merge
tsset month
gen tedspread_delta = (tedrate-L.tedrate)
gen noise_delta = (noise-L.noise)

drop if month < 480
reg BAB tedspread_delta L.tedrate, vce(robust)
estimates store model1
reg BAB noise_delta L.noise, vce(robust)
estimates store model2
reg BAB MKTEX L.inflation L.BAB tedspread_delta L.tedrate L.beta_spread, vce(robust)
estimates store model3
reg BAB MKTEX L.inflation L.BAB noise_delta L.noise L.beta_spread, vce(robust)
estimates store model4
reg BAB MKTEX L.inflation L.BAB tedspread_delta L.tedrate FMAX L.beta_spread, vce(robust)
estimates store model5
reg BAB MKTEX L.inflation L.BAB noise_delta L.noise FMAX L.beta_spread, vce(robust)
estimates store model6


************ beta compression ********************************

gen ted_mid = (tedSD_1==0 & tedSD_2==0)
tsset month

gen MKT_ted1 = MKTEX*tedSD_1 // group 1
gen MKT_ted3 = MKTEX*tedSD_2 //group 3
gen smb_ted1 = smb*tedSD_1 // group 1
gen smb_ted3 = smb*tedSD_2 //group 3
gen hml_ted1 = hml*tedSD_1 // group 1
gen hml_ted3 = hml*tedSD_2 //group 3
gen mom_ted1 = mom*tedSD_1 // group 1
gen mom_ted3 = mom*tedSD_2 //group 3
gen rmw_ted1 = rmw*tedSD_1 // group 1
gen rmw_ted3 = rmw*tedSD_2 //group 3
gen cma_ted1 = cma*tedSD_1 // group 1
gen cma_ted3 = cma*tedSD_2 //group 3
newey BAB MKT_ted1 MKTEX MKT_ted3 if month >= 480,lag(60) //conditional beta regression CAPM
newey BAB MKT_ted1 MKTEX MKT_ted3 smb_ted1 smb smb_ted3 hml_ted1 hml hml_ted3 mom_ted1 mom mom_ted3 if month >= 480,lag(60) //conditional beta regression FFC-4
newey BAB MKT_ted1 MKTEX MKT_ted3 smb_ted1 smb smb_ted3 hml_ted1 hml hml_ted3 mom_ted1 mom mom_ted3 rmw_ted1 rmw rmw_ted3 cma_ted1 cma cma_ted3  if month >= 480,lag(60) //conditional beta regression FFC-6

//full sample period:
gen MKT_ted1_full = MKTEX*tedSD_1_full // group 1
gen MKT_ted3_full = MKTEX*tedSD_2_full //group 3
gen smb_ted1_full = smb*tedSD_1_full // group 1
gen smb_ted3_full = smb*tedSD_2_full //group 3
gen hml_ted1_full = hml*tedSD_1_full // group 1
gen hml_ted3_full = hml*tedSD_2_full //group 3
gen mom_ted1_full = mom*tedSD_1_full // group 1
gen mom_ted3_full = mom*tedSD_2_full //group 3
gen rmw_ted1_full = rmw*tedSD_1_full // group 1
gen rmw_ted3_full = rmw*tedSD_2_full //group 3
gen cma_ted1_full = cma*tedSD_1_full // group 1
gen cma_ted3_full = cma*tedSD_2_full //group 3
newey BAB MKT_ted1_full MKTEX MKT_ted3_full if month >= 324,lag(60) //conditional beta regression CAPM
newey BAB MKT_ted1_full MKTEX MKT_ted3_full smb_ted1_full smb smb_ted3_full hml_ted1_full hml hml_ted3_full mom_ted1_full mom mom_ted3_full if month >= 324,lag(60) //conditional beta regression FFC-4
newey BAB MKT_ted1_full MKTEX MKT_ted3_full smb_ted1_full smb smb_ted3_full hml_ted1_full hml hml_ted3_full mom_ted1_full mom mom_ted3_full rmw_ted1_full rmw rmw_ted3_full cma_ted1_full cma cma_ted3_full  if month >= 324,lag(60) //conditional beta regression FFC-6

tsset monthly_date
twoway (tsline sd_tedrate) if month >= 324 // graph for TED spread change volatility


****** Time-series analysis of monthly FMAX returns *******************************
tsset month
newey FMAX MKTEX, lag(6)
newey FMAX MKTEX smb hml mom, lag(6)
newey FMAX MKTEX smb hml mom rmw cma, lag(6)
newey FMAX MKTEX smb hml mom BAB, lag(6)
newey FMAX MKTEX smb hml mom rmw cma BAB, lag(6)

// BAB regressions including FMAX as independent variable
newey BAB MKTEX smb hml mom FMAX, lag(6)
newey BAB MKTEX smb hml mom rmw cma FMAX, lag(6) 


******** Regressions on macro-environment variables ******************************
merge m:m month using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/GoyalMonthlyMacro.dta"

drop if _merge != 3
drop _merge
sort month

tsset month
gen pre_rec = (us_rec_levels==1)
gen post_rec= (us_rec_levels==3)
gen expan=(us_rec_levels==0)
rename t10y3m term_spread
gen dfy = BAA - AAA // calculate default spread

// Lagged variables
reg BAB L.infl L.pre_rec L.post_rec L.expan L.lty L.dfy L.term_spread L.sd_mktrf, vce(robust)
estimates store model1
reg BAB L.SENT L.lty L.dfy L.term_spread L.sd_mktrf, vce(robust)
estimates store model2
reg BAB  L.infl L.pre_rec L.post_rec L.expan L.SENT L.lty L.dfy L.term_spread L.sd_mktrf , vce(robust)
estimates store model3

//Contemoraneous variables
reg BAB infl pre_rec post_rec expan lty dfy term_spread sd_mktrf, vce(robust)
estimates store model1
reg BAB SENT lty dfy term_spread sd_mktrf, vce(robust)
estimates store model2
reg BAB  infl pre_rec post_rec expan SENT lty dfy term_spread sd_mktrf , vce(robust)
estimates store model3
