use "C:/Users/Julio/OneDrive - Universidad Católica de Chile/papers\refgroup-social/input/data/proc/df1_panelV2.dta"
use "C:\Users\jiturra\OneDrive - Universidad Católica de Chile\papers\refgroup-social\input/data/proc/df1_panelV2.dta"
* If not already set:
xtset id time

* Clean generation via operators
gen D_market           = D.market
gen D_diversityclass   = D.diversityclass
gen L1_diversityclass  = L.diversityclass
gen D_L1_diversityclass = L.D.diversityclass   // (ΔX)_{t-1}

* Keep defined differences
keep if !missing(D_market)

* Clear any stored estimates
eststo clear

* (1) Baseline FD: Δmarket = α + β Δdiversityclass + τ_t + ε
eststo m1: reg D_market D_diversityclass i.time, vce(robust)

* (2) FD + lagged level of X: Δmarket = α + β ΔX + γ X_{t-1} + τ_t + ε
eststo m2: reg D_market D_diversityclass L1_diversityclass i.time, vce(robust)

* (3) FD + lagged ΔX: Δmarket = α + β ΔX + γ ΔX_{t-1} + τ_t + ε
eststo m3: reg D_market D_diversityclass D_L1_diversityclass i.time, vce(robust)


* Pretty table on screen (stars, clustered SEs shown, compact)
esttab m1 m2 m3, ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(r2_a N, labels("Adj. R^2" "Observations")) ///
    nomtitles label compress ///
    addnotes("SEs clustered by id")

reg D_market D_diversityclass D_L1_diversityclass i.time, vce(robust)
lincom D_diversityclass + D_L1_diversityclass	
	
* Weighted OLS on FD with time indicator and clustered SEs
reg D_market D_diversityclass D_L1_diversityclass i.time ///
    [pweight = ponderador02], vce(cluster id)

* Combine parameters (uses the weighted VCE from the model above)
lincom D_diversityclass + D_L1_diversityclass
