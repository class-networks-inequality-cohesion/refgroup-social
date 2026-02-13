pacman::p_load(dplyr,lme4,mediation,psych)
load(file = here::here("input/data/proc/df1_panel_v2.RData"));paneldata137 <- paneldata

paneldata137 <- paneldata137 %>% group_by(id) %>% mutate(n_obs=n())

# standardize predictors--------------------------------------------------------
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
fit.M.within <- lmer(
  isei_avg_gc ~ isei_c_gc+isei_c_gm+
    n_total_gc+n_total_gm+
    diversityclass_gc+diversityclass_gm+ 
    factor(time)+
    (1 | id),
  data    = paneldata137,
  weights = weights1,
  REML    = FALSE
)

# --- Outcome model (Y ~ M + X + controls + random intercept) ---
fit.Y.within <- lmer(
  market ~ isei_avg_gc +isei_avg_gm+isei_c_gc + isei_c_gm+
    n_total_gc+ n_total_gm + diversityclass_gc + diversityclass_gm+
    factor(time)+
    (1 | id),
  data    = paneldata137,
  weights = weights1,
  REML    = FALSE
)

screenreg(list(fit.M.within,fit.Y.within)) 

# --- Imai et al. mediation step ---
med.within <- mediation::mediate(
  model.m  = fit.M.within,
  model.y  = fit.Y.within,
  treat    = "isei_c_gc",     # X
  mediator = "isei_avg_gc",   # M
  sims     = 100,            # increase for publication results (e.g., 2000–5000)
  boot     = FALSE            # keep FALSE when using robustSE+cluster (faster & coherent)
)

summary(med.within)            # ACME, ADE, Total, Prop. Mediated with CIs
save(med.within,file = "mediation_WE.RData")


# Causal Mediation Analysis 
# Quasi-Bayesian Confidence Intervals
# Mediator Groups: id 
# Outcome Groups: id 
# Output Based on Overall Averages Across Groups 
# 
#                     Estimate  95% CI Lower  95% CI Upper p-value
# ACME           -0.0000485383 -0.0001041256  0.0000094800     0.2
# ADE            -0.0000048413 -0.0012689764  0.0014996899     1.0
# Total Effect   -0.0000533796 -0.0012817235  0.0014322825     1.0
# Prop. Mediated  0.0257749767 -0.1949265976  0.2853000820     0.8
# 
# Sample Size Used: 6542 
# Simulations: 10 
