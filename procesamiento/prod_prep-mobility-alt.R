# =============================================================================
# Alternative construction of the mobility trajectories (Study 2)
# -----------------------------------------------------------------------------
# Does NOT modify the paper's pipeline. Reads the panel saved by
# 00-main_analysis.Rmd (df1_panel_for_mice.RData) and writes a separate file:
#   input/data/proc/df1_panel_mobility_alt.RData   (object: df1_alt)
#
# For each of the five position variables used in the paper
#   isco3  = isco03_neet  (alternative A, used in the paper)
#   isco3b = isco03b_neet (alternative B)
#   isco3c = isco03c_neet (alternative C)
#   isco3d = isco03d_neet (alternative D)
#   egp3   = egp3         (EGP)
# it builds five person-level trajectories:
#   traj_orig_<k>  replication of 00-main_analysis.Rmd (checked against the stored variable)
#   traj_fix_<k>   same rule, with the label mismatch of the 75% rule corrected
#   traj_fixna_<k> traj_fix + missing occupation treated as missing (not as NEET)
#   traj_p2_<k>    sequence typology with Oscillating, single interior deviations smoothed
#   traj_p3_<k>    traj_p2 + corrected 75% rule
# plus seq_<k>, the observed sequence (N = NEET, L = low, M = middle, H = high).
#
# The label mismatch: in 00-main_analysis.Rmd the vector `mobility_categories`
# lists "Upward (L - H)", "Upward (L - M)", "Upward (M - H)", but the categories
# are labelled "Upward (L to H)", "Upward (L to M)", "Upward (M to H)". The 75%
# rule therefore never applies to upward mobility between occupations.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)

load(here::here("input/data/proc/df1_panel_for_mice.RData"))   # df1
df1 <- as.data.frame(df1)

pos_vars <- c(isco3 = "isco03_neet", isco3b = "isco03b_neet", isco3c = "isco03c_neet",
              isco3d = "isco03d_neet", egp3 = "egp3")
stored   <- c(isco3 = "mobility_cat_adj_isco3", isco3b = "mobility_cat_adj_isco3b",
              isco3c = "mobility_cat_adj_isco3c", isco3d = "mobility_cat_adj_isco3d",
              egp3 = "mobility_cat_adj_egp3")
lab      <- c("N", "L", "M", "H")
pos_name <- c("NEET", "low", "middle", "high")

# --- Position in each wave (1 NEET, 2 low, 3 middle, 4 high) -----------------
# na_as_neet = TRUE reproduces the paper: a missing position is coded NEET.
position <- function(x, k, na_as_neet = TRUE) {
  if (k == "egp3") {                        # factor 1, 2, 3, 99 -> 99 is NEET
    p <- as.numeric(car::recode(x, "c(99,NA)=0"))           # factor levels 0..3 -> 1..4
    if (!na_as_neet) p[is.na(x)] <- NA
  } else {                                  # 1 low, 2 middle, 3 high, 4 NEET
    p <- as.numeric(car::recode(x, "c(4,NA)=0")) + 1
    if (!na_as_neet) p[is.na(x)] <- NA
  }
  p
}

# --- First/last rule of the paper -------------------------------------------
first_last_label <- function(f, l) {
  dplyr::case_when(
    f == 1 & l == 1 ~ "Stable: NEET",   f == 2 & l == 2 ~ "Stable: low",
    f == 3 & l == 3 ~ "Stable: middle", f == 4 & l == 4 ~ "Stable: high",
    f == 2 & l == 3 ~ "Upward (L to M)", f == 3 & l == 4 ~ "Upward (M to H)",
    f == 2 & l == 4 ~ "Upward (L to H)", f == 1 & l %in% c(4, 3, 2) ~ "Upward (NEET to Any)",
    f == 4 & l == 3 ~ "Downward (H to M)", f == 4 & l == 2 ~ "Downward (H to L)",
    f == 3 & l == 2 ~ "Downward (M to L)", f %in% c(4, 3, 2) & l == 1 ~ "Downward (Any to NEET)",
    TRUE ~ NA_character_)
}
mob_cats_bug <- c("Downward (Any to NEET)", "Downward (H to L)", "Downward (H to M)", "Downward (M to L)",
                  "Upward (L - H)", "Upward (L - M)", "Upward (M - H)", "Upward (NEET to Any)")
mob_cats_fix <- c("Downward (Any to NEET)", "Downward (H to L)", "Downward (H to M)", "Downward (M to L)",
                  "Upward (L to H)", "Upward (L to M)", "Upward (M to H)", "Upward (NEET to Any)")

# 75% rule as in the paper: >= 3 observations, >= 75% of them in one position, classified as mobile
rule75_paper <- function(cat, v, cats) {
  n <- length(v); if (n < 3 || !cat %in% cats) return(cat)
  for (p in 1:4) if (mean(v == p) >= 0.75) return(paste0("Stable: ", pos_name[p]))
  cat
}
first_last <- function(v, cats) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_character_)
  rule75_paper(first_last_label(v[1], v[length(v)]), v, cats)
}

# --- Sequence typology with Oscillating -------------------------------------
classify_seq <- function(v) {
  d <- sign(diff(v)); d <- d[d != 0]
  if (length(d) == 0) return(paste0("Stable: ", pos_name[v[1]]))
  if (all(d > 0)) return("Upward")
  if (all(d < 0)) return("Downward")
  "Oscillating"
}
# single interior wave that differs from two equal neighbours (A-B-A), detected on the observed sequence
smooth_one <- function(v) {
  n <- length(v); if (n < 3) return(v)
  b <- which(sapply(2:(n - 1), function(i) v[i - 1] == v[i + 1] && v[i] != v[i - 1])) + 1
  if (length(b) == 1) v[b] <- v[b - 1]
  v
}
p2 <- function(v) { v <- v[!is.na(v)]; if (!length(v)) return(NA_character_); classify_seq(smooth_one(v)) }
p3 <- function(v) {
  v <- v[!is.na(v)]; if (!length(v)) return(NA_character_)
  cl <- classify_seq(smooth_one(v))
  if (!cl %in% c("Upward", "Downward") || length(v) < 3) return(cl)
  for (p in 1:4) if (mean(v == p) >= 0.75) return(paste0("Stable: ", pos_name[p]))   # shares on the observed sequence
  cl
}

# --- Build all versions -------------------------------------------------------
df1 <- df1 %>% arrange(id, ola)
traj_alt <- df1 %>% distinct(id)

for (k in names(pos_vars)) {
  pos_paper <- position(df1[[pos_vars[[k]]]], k, na_as_neet = TRUE)
  pos_na    <- position(df1[[pos_vars[[k]]]], k, na_as_neet = FALSE)
  out <- data.frame(id = df1$id, a = pos_paper, b = pos_na) %>%
    group_by(id) %>%
    summarise(
      orig  = first_last(a, mob_cats_bug),
      fix   = first_last(a, mob_cats_fix),
      fixna = first_last(b, mob_cats_fix),
      p2    = p2(b),
      p3    = p3(b),
      seq   = paste(ifelse(is.na(b), "_", lab[b]), collapse = "-"),
      .groups = "drop")
  names(out)[-1] <- paste0(c("traj_orig_", "traj_fix_", "traj_fixna_", "traj_p2_", "traj_p3_", "seq_"), k)
  traj_alt <- left_join(traj_alt, out, by = "id")

  # Check: the replication equals the variable stored by 00-main_analysis.Rmd
  chk <- df1 %>% distinct(id, .keep_all = TRUE) %>% select(id, s = all_of(stored[[k]])) %>%
    left_join(out %>% select(id, o = 2), by = "id")
  stopifnot(identical(as.character(chk$s), chk$o))
}
cat("Replication of the five stored trajectories: OK\n")

# Collapsed versions (Upward / Downward / Stable: x), as used in the models
collapse <- function(x) sub("^(Upward|Downward) \\(.*\\)$", "\\1", x)
for (v in grep("^traj_(orig|fix|fixna)_", names(traj_alt), value = TRUE))
  traj_alt[[sub("^traj_", "prof_", v)]] <- collapse(traj_alt[[v]])
for (v in grep("^traj_p[23]_", names(traj_alt), value = TRUE))
  traj_alt[[sub("^traj_", "prof_", v)]] <- traj_alt[[v]]

df1_alt <- df1 %>% left_join(traj_alt, by = "id")
save(df1_alt, traj_alt, file = here::here("input/data/proc/df1_panel_mobility_alt.RData"))
cat("Saved input/data/proc/df1_panel_mobility_alt.RData\n")
