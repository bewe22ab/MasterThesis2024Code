********** Preparations for time-series analysis requiring daily data *****************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Time-series"

use "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/Others2000.dta", clear

drop date
gen date = fakedate
format date %td
drop if rf ==.

gen MKTEX = mktrf/100
replace smb=smb/100
replace hml = hml/100
replace rmw = rmw/100
replace cma = cma/100
rename Noise_bp noise
replace noise = noise/10

drop if fakedate < 14612
drop if fakedate > 22645 //until 31dec2021

tsset date
tsline noise tedrate // graph of funding constraint proxies

******** create monthly funding liquidity measures *****************************
gen month = mofd(fakedate)
drop if tedrate ==. | noise==.
sort fakedate
gen obs = _n
tsset obs
gen tedspread_delta = (tedrate-L.tedrate)
**** Volatility of TED spread change **************
bys month: egen sd_tedrate = sd(tedspread_delta) //monthly standard deviation of daily TED spread changes
save daily_timeseriesvariables, replace
bys month: keep if _n==_N
collapse (p33) TEDVOL1 = sd_tedrate (p66) TEDVOL2 = sd_tedrate //find breakpoints

use daily_timeseriesvariables, clear
scalar BP_tedSD1 = .01338087 //breakpoints
scalar BP_tedSD2 =  .02515563 //breakpoints
gen tedSD_1 = 1 if sd_tedrate < BP_tedSD1
replace tedSD_1 = 0 if tedSD_1==.
gen tedSD_2 = 1 if sd_tedrate > BP_tedSD2
replace tedSD_2 = 0 if tedSD_2==.
// repeat for full time period
scalar BP_tedSD1_full = .0193588  //breakpoints
scalar BP_tedSD2_full =   .046291 //breakpoints
gen tedSD_1_full = 1 if sd_tedrate < BP_tedSD1_full
replace tedSD_1_full = 0 if tedSD_1_full==.
gen tedSD_2_full = 1 if sd_tedrate > BP_tedSD2_full
replace tedSD_2_full = 0 if tedSD_2_full==.

// generate monthly market variation for time-series analysis
bys month: egen sd_mktrf = sd(MKTEX)

bys month: keep if _n==_N
keep noise tedrate month sd_tedrate tedSD_1 tedSD_2 tedSD_1_full tedSD_2_full sd_mktrf
drop if month < 480
save "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/funding_liquidity_monthly.dta",replace
