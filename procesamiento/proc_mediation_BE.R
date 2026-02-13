pacman::p_load(dplyr,lme4,mediation,psych)
load(file = here::here("input/data/proc/df1_panel_v2.RData"));paneldata137 <- paneldata

paneldata137 <- paneldata137 %>% group_by(id) %>% mutate(n_obs=n())

# standardize predictors--------------------------------------------------------
# paneldata137$isei_avg <- as.numeric(scale(paneldata137$isei_avg))
# paneldata137$n_total <- as.numeric(scale(paneldata137$n_total))
paneldata137$isei_avg <- as.numeric(paneldata137$isei_avg/10)
paneldata137$n_total <- as.numeric(paneldata137$n_total/25)
paneldata137$isei <- factor(paneldata137$isei,levels = c(1,2,3,99))
paneldata137$isei <- relevel(paneldata137$isei,ref = 2)
paneldata137$age <- paneldata137$age 
paneldata137$neet <- ifelse(paneldata137$isei==99,1,0)
# If lab status is NEET, then ISEI =10
paneldata137$isei_c <- ifelse(paneldata137$isei_c ==0,10,paneldata137$isei_c) 
paneldata137$isco03_neet <- factor(paneldata137$isco03_neet)

# Network diversity for longitudinal analysis
pca_fit_v10 <- 
  psych::principal(r = as.data.frame(paneldata137) %>% 
                     dplyr::select("entropy_contacts","know_total"),
                   nfactors = 1,rotate = "varimax",scores = T,missing = T, 
                   cor = "cor",weight = paneldata137$weights1)
paneldata137$diversityclass <- as.numeric(pca_fit_v10$scores)


paneldata137$diversityclass_gm<- MLMusingR::group_mean(paneldata137$diversityclass,grp = paneldata137$id)
paneldata137$diversityclass_gc<- MLMusingR::group_center(paneldata137$diversityclass,grp = paneldata137$id)

paneldata137$n_total_gm<- MLMusingR::group_mean(paneldata137$n_total,grp = paneldata137$id)
paneldata137$n_total_gc<- MLMusingR::group_center(paneldata137$n_total,grp = paneldata137$id)

paneldata137$isei_avg_gm<- MLMusingR::group_mean(paneldata137$isei_avg,grp = paneldata137$id)
paneldata137$isei_avg_gc<- MLMusingR::group_center(paneldata137$isei_avg,grp = paneldata137$id)

paneldata137$isei_c_gm <- MLMusingR::group_mean(paneldata137$isei_c,grp = paneldata137$id)
paneldata137$isei_c_gc <- MLMusingR::group_center(paneldata137$isei_c,grp = paneldata137$id)

# --- Mediator model (M ~ X + controls + random intercept) ---
fit.M.between <- lmer(
  isei_avg_gm ~ isei_c_gc+isei_c_gm+ 
    n_total_gc+n_total_gm+ 
    diversityclass_gc+diversityclass_gm+  
    factor(time)+
    (1 | id),
  data    = paneldata137,
  weights = weights1,
  REML    = FALSE
)

# --- Outcome model (Y ~ M + X + controls + random intercept) ---
fit.Y.between <- lmer(
  market ~ isei_avg_gc +isei_avg_gm+ # network status
    isei_c_gc + isei_c_gm+ # isei individual
    n_total_gc+ n_total_gm + # network size
    diversityclass_gc + diversityclass_gm # network diversity
    factor(time)+
    (1 | id),
  data    = paneldata137,
  weights = weights1,
  REML    = FALSE
)

screenreg(list(fit.M.between,fit.Y.between))

# --- Imai et al. mediation step ---
med.between <- mediation::mediate(
  model.m  = fit.M.between,
  model.y  = fit.Y.between,
  treat    = "isei_c_gm",     # X
  mediator = "isei_avg_gm",   # M
  sims     = 100,            # increase for publication results (e.g., 2000–5000)
  boot     = FALSE            # keep FALSE when using robustSE+cluster (faster & coherent)
)

summary(med.between)            # ACME, ADE, Total, Prop. Mediated with CIs
save(med.between,file = "mediation_BE.RData")

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
# Mediator Groups: id 
# 
# Outcome Groups: id 
# 
# Output Based on Overall Averages Across Groups 
# 
#                 Estimate 95% CI Lower 95% CI Upper               p-value    
# ACME           0.0012650    0.0011631    0.0013298 < 0.00000000000000022 ***
# ADE            0.0017592    0.0010019    0.0025933 < 0.00000000000000022 ***
# Total Effect   0.0030242    0.0022699    0.0038756 < 0.00000000000000022 ***
# Prop. Mediated 0.4290002    0.3309587    0.5591376 < 0.00000000000000022 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Sample Size Used: 6542 
# 
# 
# Simulations: 5 