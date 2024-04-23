******************* Independent bivariate portfolio analyses ******************************************
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Bivariate sorts"

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/Beta_sort_portfolios.dta",clear

// Independent sort based on 60-month beta for heatplot
drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
merge m:m date PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/MAX5_sort_portfolios.dta", keepusing(pf_M)
drop _merge

heatplot pf_B pf_M, statistic(count) discrete ytitle(Beta decile portfolios) xtitle(MAX decile portfolios)



************* 5x5 portfolios for sort analysis using 60-m betas ******************************************

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/Beta_sort_portfolios.dta",clear

drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
merge m:m date PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/MAX5_sort_portfolios.dta", keepusing(pf_M)
drop _merge
// only 5 portfolios for each variable (instead of ten)
replace pf_B = 1 if pf_B==2
replace pf_B = 2 if pf_B==3 | pf_B==4
replace pf_B = 3 if pf_B==5 | pf_B==6
replace pf_B = 4 if pf_B==7 | pf_B==8
replace pf_B = 5 if pf_B==9 | pf_B==10
replace pf_M = 1 if pf_M==2
replace pf_M = 2 if pf_M==3 | pf_M==4
replace pf_M = 3 if pf_M==5 | pf_M==6
replace pf_M = 4 if pf_M==7 | pf_M==8
replace pf_M = 5 if pf_M==9 | pf_M==10

// calculate equal-weighted daily portfolio returns
collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta MAX5, by(date pf_B pf_M)

rename RET ret_PF
rename CAPM_beta beta_PF
rename MAX5 MAX_PF

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B pf_M) // monthly portfolio returns
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

egen beta_pf_month = mean(beta_PF), by(pf_B pf_M month)
egen MAX_pf_month = mean(MAX_PF), by(pf_M pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B pf_M date
by pf_B pf_M month, sort: keep if _n == _N

matrix ret_Ind = J(5,5,.)						
matrix rownames ret_Ind = pf1 pf2 pf3 pf4 pf5 
matrix colnames ret_Ind = pf1 pf2 pf3 pf4 pf5 

forvalues B=1/5 {
	forvalues M=1/5 {
		sum exret_pf_month if pf_M == `M' & pf_B == `B'
		scalar row=`M'
		scalar col=`B'
		matrix ret_Ind[row,col]=r(mean)
	}
}
matrix list ret_Ind


// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_M, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_M, sort: keep if _n == _N

tsset pf_M month
bys pf_M: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_B =J(2,5,.)
matrix colnames LS_B = Max_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_B = Av_exret t-stat

forvalues M=1/5 {
ttest Ret_LS_m==0 if pf_M==`M'
scalar col = `M'
matrix LS_B[1,col]=r(mu_1)
matrix LS_B[2,col]=r(t)
}
matrix list LS_B

// Low-High MAX
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_B, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_B, sort: keep if _n == _N

tsset pf_B month
bys pf_B: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_M =J(2,5,.)
matrix colnames LS_M = Beta_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_M = Av_exret t-stat

forvalues B=1/5 {
ttest Ret_LS_m==0 if pf_B==`B'
scalar col = `B'
matrix LS_M[1,col]=r(mu_1)
matrix LS_M[2,col]=r(t)
}
matrix list LS_M



************* 5x5 portfolios for sort analysis using 12-m betas ******************************************

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/Beta2_sort_portfolios.dta",clear

drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
merge m:m date PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/MAX5_sort_portfolios.dta", keepusing(pf_M)
drop _merge

replace pf_B = 1 if pf_B==2
replace pf_B = 2 if pf_B==3 | pf_B==4
replace pf_B = 3 if pf_B==5 | pf_B==6
replace pf_B = 4 if pf_B==7 | pf_B==8
replace pf_B = 5 if pf_B==9 | pf_B==10
replace pf_M = 1 if pf_M==2
replace pf_M = 2 if pf_M==3 | pf_M==4
replace pf_M = 3 if pf_M==5 | pf_M==6
replace pf_M = 4 if pf_M==7 | pf_M==8
replace pf_M = 5 if pf_M==9 | pf_M==10


collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta2 MAX5, by(date pf_B pf_M)

rename RET ret_PF
rename CAPM_beta2 beta_PF
rename MAX5 MAX_PF

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B pf_M)
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

egen beta_pf_month = mean(beta_PF), by(pf_B pf_M month)
egen MAX_pf_month = mean(MAX_PF), by(pf_M pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B pf_M date
by pf_B pf_M month, sort: keep if _n == _N

matrix ret_Ind = J(5,5,.)						
matrix rownames ret_Ind = pf1 pf2 pf3 pf4 pf5 
matrix colnames ret_Ind = pf1 pf2 pf3 pf4 pf5 

forvalues B=1/5 {
	forvalues M=1/5 {
		sum exret_pf_month if pf_M == `M' & pf_B == `B'
		scalar row=`M'
		scalar col=`B'
		matrix ret_Ind[row,col]=r(mean)
	}
}
matrix list ret_Ind


// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_M, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_M, sort: keep if _n == _N

tsset pf_M month
bys pf_M: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_B =J(2,5,.)
matrix colnames LS_B = Max_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_B = Av_exret t-stat

forvalues M=1/5 {
ttest Ret_LS_m==0 if pf_M==`M'
scalar col = `M'
matrix LS_B[1,col]=r(mu_1)
matrix LS_B[2,col]=r(t)
}
matrix list LS_B

// Low-High Max

gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_B, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_B, sort: keep if _n == _N

tsset pf_B month
bys pf_B: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_M =J(2,5,.)
matrix colnames LS_M = Beta_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_M = Av_exret t-stat

forvalues B=1/5 {
ttest Ret_LS_m==0 if pf_B==`B'
scalar col = `B'
matrix LS_M[1,col]=r(mu_1)
matrix LS_M[2,col]=r(t)
}
matrix list LS_M


************* 5x5 portfolios for sort analysis using 60-m shrunk betas ******************************************

use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/Beta_sh_sort_portfolios.dta",clear

drop Beta1 Beta3 Beta2 Beta4 Beta5 Beta6 Beta7 Beta8 Beta9
merge m:m date PERMNO using "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts/MAX5_sort_portfolios.dta", keepusing(pf_M)
drop _merge

replace pf_B = 1 if pf_B==2
replace pf_B = 2 if pf_B==3 | pf_B==4
replace pf_B = 3 if pf_B==5 | pf_B==6
replace pf_B = 4 if pf_B==7 | pf_B==8
replace pf_B = 5 if pf_B==9 | pf_B==10
replace pf_M = 1 if pf_M==2
replace pf_M = 2 if pf_M==3 | pf_M==4
replace pf_M = 3 if pf_M==5 | pf_M==6
replace pf_M = 4 if pf_M==7 | pf_M==8
replace pf_M = 5 if pf_M==9 | pf_M==10

collapse RET mktrf month rf smb hml rmw cma mom CAPM_beta_sh MAX5, by(date pf_B pf_M)

rename RET ret_PF
rename CAPM_beta_sh beta_PF
rename MAX5 MAX_PF

gen exret_m = mktrf/100
gen exret_pf = ret_PF-(rf/100)
gen log_ret_pf = log(1+ret_PF)
egen ret_pf_month = total(log_ret), by(month pf_B pf_M)
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

egen beta_pf_month = mean(beta_PF), by(pf_B pf_M month)
egen MAX_pf_month = mean(MAX_PF), by(pf_M pf_B month)
drop ret_PF mktrf ret_pf_month log_ret_pf beta_PF

sort pf_B pf_M date
by pf_B pf_M month, sort: keep if _n == _N

matrix ret_Ind = J(5,5,.)						
matrix rownames ret_Ind = pf1 pf2 pf3 pf4 pf5 
matrix colnames ret_Ind = pf1 pf2 pf3 pf4 pf5 

forvalues B=1/5 {
	forvalues M=1/5 {
		sum exret_pf_month if pf_M == `M' & pf_B == `B'
		scalar row=`M'
		scalar col=`B'
		matrix ret_Ind[row,col]=r(mean)
	}
}
matrix list ret_Ind


// Low-High Beta
gen LS_Ind = 1 if pf_B == 1
replace LS_Ind =-1 if pf_B==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_M, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_M, sort: keep if _n == _N

tsset pf_M month
bys pf_M: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_B =J(2,5,.)
matrix colnames LS_B = Max_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_B = Av_exret t-stat

forvalues M=1/5 {
ttest Ret_LS_m==0 if pf_M==`M'
scalar col = `M'
matrix LS_B[1,col]=r(mu_1)
matrix LS_B[2,col]=r(t)
}
matrix list LS_B

// Low-High Max
gen LS_Ind = 1 if pf_M == 1
replace LS_Ind =-1 if pf_M==5
gen Ret_LS = exret_pf_month * LS_Ind
by month pf_B, sort: egen Ret_LS_m = total(Ret_LS)
by month pf_B, sort: keep if _n == _N

tsset pf_B month
bys pf_B: newey Ret_LS_m exret_m_month smb hml rmw cma mom,lag(6) //FFC-6 regression

matrix LS_M =J(2,5,.)
matrix colnames LS_M = Beta_pf1 pf2 pf3 pf4 pf5
matrix rownames LS_M = Av_exret t-stat

forvalues B=1/5 {
ttest Ret_LS_m==0 if pf_B==`B'
scalar col = `B'
matrix LS_M[1,col]=r(mu_1)
matrix LS_M[2,col]=r(t)
}
matrix list LS_M
