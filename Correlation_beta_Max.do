********* Cross-sectional beta-MAX correlation time-series
cd "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Main_analysis/Univariate sorts"

// Calculate cross-sectional beta-MAX correlation over time based on 60-m beta
use  "/Users/benita/Documents/Benita/Uni/CBS/Semester 4/Data analysis/Data/CRSPdaily2000prep.dta",clear

sort PERMNO date
by PERMNO month, sort: keep if _n == _N
correlate CAPM_beta MAX5
sort month
egen corr_BM = corr(CAPM_beta MAX5), by(month)

by month, sort: keep if _n == _N
keep month corr_BM
sum corr_BM, detail
scalar corr_median = r(p50)
scalar corr_mean = r(mean)

gen high = 1 if corr_BM > corr_median
replace high = 0 if corr_BM <= corr_median

save BM_corr_months, replace

format month %tm
tsset month
tsline corr_BM // graph for correlation over time
// the remainder of the analysis is part of the BAB_monthly file



