# The present script is not part of the analysis for the IJE article:
# "Long-term effect of pharmacological treatment on academic achievement
#  of Norwegian children diagnosed with ADHD: a target trial emulation"
# But part of the third article of my dissertation:
# "A mixture multiverse replication approach to explore estimand differences between studies:
#  Application to the effects of discontinuing versus initiating ADHD pharmaceutical treatment"

# This script should be run after scripts 
# (in the repository for the corresponding article: https://github.com/tvarnetperez/TTE_ADHD)
# source(here::here("scripts", "00_load_data.R")) and
# source(here::here("scripts", "01_prepare_data.R"))
# in the main_script.R, with option reduce_columns <- FALSE

# 0. Dependencies ---------------------------------------------------------
library(here)
library(data.table)
library(multiverse)
library(marginaleffects)
library(mice)
library(glue, include.only = "glue")
set.seed(1234)

# For standardization relative to the population of GPA

load(here::here("data", "semiraw", "test_and_gpa_pop_stats.Rdata"))
dt_pop_GPA <- school.perf.pop.mean_and_sd[variable == "p.GPA_10"]
rm(school.perf.pop.mean_and_sd)

# Number of imputed datasets
m = 100L

# Options will be:
# `exclusion`, which does not include missingness as criterion as it has its own option
# `outcome`
# `treatment`
# `missingness`
# `adjustment`

# 1. Pre-processing -------------------------------------------------------

# :: 1.1 Define groups of variables and subset to variables of int --------

keilow_controls <- c(
  "sex",
  "ADHD.m", # ADHD diagnosis of mother
  "ADHD.f", # ADHD diagnosis of father
  "m.edu", # Mother's education
  "m.age", # Mother's age (at childbirth??, ask)
  "birthweight", # Birth weight
  "Preglength"  # Gestational age
)

# Will remove school ids as their influence was negligible in the target trial paper and they consume many df
varnet_controls <- setdiff(outcome_covariates, c("s.id5", "s.id8", "medicated.pt0", "med.days.p6")) 
# > outcome_covariates
# [1] "p.EN_5_std"        "p.RE_5_std"        "p.MA_5_std"        "exempt.p.EN_5"     "exempt.p.RE_5"     "exempt.p.MA_5"     "parity"            "pre6.P81.father"   "pre6.P81.mother"  
# [10] "ADHD.m"            "ADHD.f"            "pre6.all.mother"   "pre6.all.father"   "pre6.int.mother"   "pre6.int.father"   "pre6.ext.mother"   "pre6.ext.father"   "m.edu"            
# [19] "m.age"             "P81.s.any"         "n.sibs.w.diag"     "sex"               "Preglength"        "birthweight"       "byear"             "bmonth"            "pre6.n.visits_all"
# [28] "pre6.n.visits_P24" "pre6.n.visits_int" "pre6.n.visits_ext" "pre6.n.visits_P06" "s.avg_5_RE"        "s.avg_5_MA"        "cs.5.m"            "cs.5.f"            "cs.6.m"           
# [37] "cs.6.f"            "s.id8"             "s.id5

# Subset dt_merged only to columns we will use
dt_merged <- dt_merged[, .SD, .SDcols = c('id',
                                          "initiated_01",   # Treatment in VØFB
                                          "med.days.6to10",  # Variables needed to define treatment in KHF
                                          "med.start.6to10",
                                          "med.delta.max.6to10", # Idem
                                          "p.MA_8_std",      # Outcome in VØFB
                                          "p.GPA_10",        # Outcome in KHF
                                          "date.10",         # Needed to standardize outcome in KHF
                                          outcome_covariates,
                                          keilow_controls)]

# # Population means and sds for GPA
# load(here::here("data", "semiraw", "test_and_gpa_pop_stats.Rdata"))
# dt_pop_GPA <- school.perf.pop.mean_and_sd[variable == "p.GPA_10"]
# rm(school.perf.pop.mean_and_sd)


# :: 1.2 Remove sparse cells ------------
# Else there are empty cells for the imputation step.


# ::: Remove widows and widowers subjects and factor levels
# Vector of civil status variable names
civil_status_vars <- c("cs.5.f", "cs.5.m", "cs.6.m", "cs.6.f")

# Find id's of individuals whose parents are widows/widowers:
widow_ids <- list()
widow_ids$merged <- melt.data.table(dt_merged, id.vars = 'id', measure.vars = civil_status_vars)[value == 'widdow', id] |>
  unique()
widow_ids$analytic <- melt.data.table(dt_eligible, id.vars = 'id', measure.vars = civil_status_vars)[value == 'widdow', id] |>
  unique()

# > sapply(widow_ids, length)
# merged analytic 
# 57       14 

dt_merged   <- dt_merged[!(id %in% widow_ids$merged)]
dt_eligible <- dt_eligible[!(id %in% widow_ids$analytic)]

# ::: Remove sparse birth years
# 2000 2001 2002 2003 2004 2005 2006 2007 
# 1351 1345 1380 1512 1526    1    0    0 

dt_merged   <- dt_merged[!byear %in% c('2005', '2006', '2007')]
dt_eligible <- dt_eligible[!byear %in% c('2005', '2006', '2007')]

# Drop unused factor levels
dt_merged   <- droplevels(dt_merged)
dt_eligible <- droplevels(dt_eligible)

# :: 1.3 Create KHFtreatment pattern factor in dt_merged and dt_eligible ---------

# PT_group (Pharmacological treatment)
# "Having purchased medication for maximum three months within the data window"
# are set to Discontinuous Pharmacological Treatment
# We will ignore for now that some dispensations are
# enough stock for three months. So we will just
# stay close to the Keilow operationalization
dt_merged[med.days.6to10 < 90,
          PT_group := "DPT"] # n = 8,262 including the unmedicated, 240 excluding unmedicated (n = 158 in Keilow subset)

# For those that had "regular and stable use of medication" (p.5)
# (since categories are mutually exclusive, we assume this to mean >90 days medicated) 
# with maximum number of days between dispensions less than 30,
# their PT group is set to Continuous Pharmacological Treatment

dt_merged[med.delta.max.6to10 < 30 & med.days.6to10 > 90,
          PT_group := "CPT"]  # n = 31 (n = 22 in Keilow subset)


# Those that are neither, will be set to
# Ambiguous Pharmacological Treatment
dt_merged[is.na(PT_group),
          PT_group := "APT"] # n = 6,896 (n = 5,881 in Keilow subset)
# Typing
dt_merged$PT_group <- as.factor(dt_merged$PT_group)
dt_merged$PT_group <- relevel(dt_merged$PT_group, ref = "CPT")

# Same for dt_eligible
dt_eligible[med.days.6to10 < 90,
            PT_group := "DPT"]
dt_eligible[med.delta.max.6to10 < 30 & med.days.6to10 > 90,
            PT_group := "CPT"]
dt_eligible[is.na(PT_group),
            PT_group := "APT"]
dt_eligible$PT_group <- as.factor(dt_eligible$PT_group)
dt_eligible$PT_group <- relevel(dt_eligible$PT_group, ref = "CPT")


# :: 1.4 Create standardized GPA variable ----------------------------------------

# Standardization of scores relative to full population sample
# Create year for GPA score
dt_merged[, year_GPA := data.table::year(date.10)]

# Will exclude rows for years 2014 (n=3) and 2015 (n=4) [In Keilow subset]
dt_merged <- dt_merged[year_GPA != 2014 & year_GPA != 2015] 
# For each year for the GPA, standardize the score relative
# to the full population that got their gpa that year
for (i in unique(dt_pop_GPA$year)) {
  dt_merged[year_GPA == i,
            p.GPA_10_std := (p.GPA_10 - dt_pop_GPA[year == i,
                                                   m])/
              dt_pop_GPA[year == i, sd]]
  
}

# Also for dt_eligible

# Standardization of scores relative to full population sample
# Create year for GPA score
dt_eligible[, year_GPA := data.table::year(date.10)]

# Will exclude rows for years 2014 (n=3) and 2015 (n=4) [In Keilow subset]
dt_eligible <- dt_eligible[year_GPA != 2014 & year_GPA != 2015] 
# For each year for the GPA, standardize the score relative
# to the full population that got their gpa that year
for (i in unique(dt_pop_GPA$year)) {
  dt_eligible[year_GPA == i,
              p.GPA_10_std := (p.GPA_10 - dt_pop_GPA[year == i,
                                                     m])/
                dt_pop_GPA[year == i, sd]]
  
}


# :: 1.5 Treatment pattern distribution ------------------------------------------------------------------
# Ugly quick code for figure showing different resulting distributions

treatment_distribution <- local({
  dt_keilow <- data.table::copy(dt_merged)
  # dt_keilow <- data.table::copy(dt_merged)
  # Exclude children who initiated 3 months before GPA or less
  dt_keilow <- dt_keilow[med.start.6to10 < {dt_keilow$date.10 - 90},] # n = 30 excluded, n = 7,726 left
  # Include children only children who were medicated between grade 6 and 10
  dt_keilow <- dt_keilow[med.days.6to10 > 0, ] # n = 1,660 excluded, n = 6,066 left
  
  temp <- rbind(dt_keilow$PT_group |> table() |>  prop.table(),
        dt_keilow$PT_group |> table() |> as.integer()
  )
  
  rownames(temp) <- c("Proportions", "Count")
  
  temp
}
)


# Save output
saveRDS(treatment_tables,
        file = here::here("output", "objects", "multiverse_paper", "treatment_distribution.RDS"))

file.copy(from = here::here("output", "objects", "multiverse_paper", "treatment_distribution.RDS"),
          to = "N:/durable/file-export/paper3/treatment_distribution.RDS", overwrite = TRUE)


# 2. Multiverse code ---------------------------------------------------------

# Initiate multiverse
M = multiverse()

# :: 2.1 Options and choices code ----------------------------------------------

# Exclusion option
#   This defines which dataset will be used. 
inside(M,
       {
         dt <- branch(
           exclusion,
           "KHF"  ~ local({
             dt_keilow <- data.table::copy(dt_merged)
             # dt_keilow <- data.table::copy(dt_merged)
             # Exclude children who initiated 3 months before GPA or less
             dt_keilow <- dt_keilow[med.start.6to10 < {dt_keilow$date.10 - 90},] # n = 30 excluded, n = 7,726 left
             # Include children only children who were medicated between grade 6 and 10
             dt_keilow <- dt_keilow[med.days.6to10 > 0, ] # n = 1,660 excluded, n = 6,066 left
             
             
             dt_keilow
           }),
           
           "VØFB" ~ dt_eligible[medicated.pt0 == 'No'] # Complete wash out period as in main analysis of TTE paper
         )
       })

# Outcome option

inside(M,
       {
         outcome_var <- branch(
           outcome,
           "KHF"  ~ "p.GPA_10_std",
           "VØFB" ~ "p.MA_8_std"
         )     
       })


# Treatment option

inside(M,
       {
         dt$treat_var <- branch(
           treatment,
           "KHF"  ~ dt$PT_group, # To avoid assignment by reference issues, we do it this way
           "VØFB" ~ dt$initiated_01
         )
       })



# Adjustment set option
# 
inside(M,
       {
         adjustment_vars <- branch(
           adjustment,
           "KHF"  ~ keilow_controls,
           "VØFB" ~ varnet_controls,
           "Unadjusted" ~ NULL
         )
       }
)


# :: 2.2 Model fit ---------------------------------------------------------------

# To Create predictor matrix that excludes pretest missingness indicators from their respective imputation model
# (thus avoiding complete separation)
# we need to resort to local due to a bug in the multiverse package
# (https://github.com/MUCollective/multiverse/issues/128)
# We also resort to ugly Imputation1 and Imputation2 solution to conditionally 
# define the predictor matrix based on covariates

inside(M,
       {
         # Make formula
         formula_lm_string <- glue::glue("{outcome_var} ~ {paste(c('treat_var', adjustment_vars), collapse = ' + ')}")
         # Scoping issues with `with.mids` do not allow us to define the formula object here (see https://stackoverflow.com/a/75582131)
         # model_formula_string <- glue::glue("~ {paste(c('treat_var', adjustment_vars), collapse = ' + ')}")
         # Fit OLS
         
         branch(
           missingness, 
           # Ugly solution, but the predictor matrix has to be adjusted only when pretests are in covariates
           "Imputation1" %when% (adjustment != "VØFB")  ~ {
             mice_object <- mice(data = dt[, .SD, .SDcols = c(outcome_var, "treat_var", adjustment_vars)],
                                 m = m, .Random.seed = 1234, method = 'pmm'
             )
             
             fit <- with(mice_object
                         , lm(as.formula(formula_lm_string)))
           }
           ,
           "Imputation2" %when% (adjustment == "VØFB")  ~ {
             mice_object <- mice(data = dt[, .SD, .SDcols = c(outcome_var, "treat_var", adjustment_vars)],
                                 m = m, .Random.seed = 1234, method = 'pmm', predictorMatrix = local({
                                   varnames <- dt[, .SD, .SDcols = c(outcome_var, "treat_var", adjustment_vars)] |> 
                                     names()
                                   
                                   tempmat <- matrix(data = 1L,
                                                     nrow = length(varnames),
                                                     ncol = length(varnames),
                                                     dimnames = list(varnames, varnames)
                                   )
                                   
                                   diag(tempmat) <- 0L
                                   
                                   tempmat[pretest_names_std[1], exempt_pretest_names[1]] <- 0L
                                   tempmat[pretest_names_std[2], exempt_pretest_names[2]] <- 0L
                                   tempmat[pretest_names_std[3], exempt_pretest_names[3]] <- 0L
                                   tempmat
                                 }
                                 )
             )
             
             fit <- with(mice_object
                         , lm(as.formula(formula_lm_string)))
           }
           ,
           "CompleteCase" ~ {
             fit <- lm(as.formula(formula_lm_string), data = dt)
           }
         )
         marginalfx_output <- marginaleffects::avg_comparisons(fit,
                                                               variables = list(treat_var = branch(
                                                                 treatment,
                                                                 "KHF" ~ c("CPT", "DPT"), # Define contrast so this is discontinuation effect
                                                                 "VØFB" ~ c(0, 1) # This is initiation effect
                                                               )
                                                               )
         )
         
         
         # Log the analytic sample size and excluded elements for each
         analytic_n <- branch(
           missingness,
           "Imputation1" ~  {fit$analyses[[1]]$model |> nrow()},
           "Imputation2" ~  {fit$analyses[[1]]$model |> nrow()},
           "CompleteCase" ~ {fit$model |> nrow()}
         )
         
         excluded_n <- branch(
           missingness,
           "Imputation1" ~  {fit$analyses[[1]]$na.action |> length()},
           "Imputation2" ~  {fit$analyses[[1]]$na.action |> length()},
           "CompleteCase" ~ {fit$na.action |> length()}
         )
         
         rm(fit) # To reduce size of complete multiverse object
         
         point_estimate    <- marginalfx_output$estimate
         se                <- marginalfx_output$std.error
         lower_CI          <- marginalfx_output$conf.low
         upper_CI          <- marginalfx_output$conf.high
       }
)


multiverse::execute_multiverse(M, progress = TRUE)

dt_multiverse <- multiverse::extract_variables(M, point_estimate, lower_CI, upper_CI, analytic_n, excluded_n) |>
  as.data.table()
dt_multiverse

# Reduce size of file
butcher::weigh(dt_multiverse)
glue::glue("The weight of the multiverse object is {sum(butcher::weigh(dt_multiverse)[, 'size'])} MB")

dt_multiverse[, .results := NULL]

butcher::weigh(dt_multiverse)
glue::glue("The weight of the shrunk multiverse object is {sum(butcher::weigh(dt_multiverse)[, 'size'])} MB")
# A more than 99% reduction in size


# Save output
saveRDS(dt_multiverse,
        file = here::here("output", "objects", "multiverse_paper", "dt_multiverse.RDS"))

file.copy(from = here::here("output", "objects", "multiverse_paper", "dt_multiverse.RDS"),
          to = "N:/durable/file-export/paper3/dt_multiverse.RDS", overwrite = TRUE)


file.copy(from = here::here("999_mixture_multiverse.R"),
          to = "N:/durable/file-export/paper3/main_mixture_multiverse.R", overwrite = TRUE)
