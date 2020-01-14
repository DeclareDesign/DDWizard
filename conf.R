# Configuration options
#
# Sisi Huang <sisi.huang@wzb.eu>
# Markus Konrad <markus.konrad@wzb.eu>
# Clara Bicalho <clara.bicalho@wzb.eu>
#
# Oct. 2018
#

library(future)

options(future.fork.enable = TRUE)  # see ?future::supportsMulticore

app_title <- "DeclareDesign Wizard"
nav_bar_color <- " light-blue darken-3"
feedback_form_url <- "https://docs.google.com/forms/d/e/1FAIpQLSfH8_zy14p1OgvA4Kpx1OSuqx3Kihb5f3OrxA6W_KwDFPqijA/viewform"

args_control_skip_design_args <- c('design_name', 'args_to_fix')   # for all designers
args_control_skip_specific_designer_args <- list(                  # for specific designers    
    'block_cluster_two_arm_designer' = c('treatment_mean'),
    'two_arm_designer'               = c('treatment_mean'),
    'two_arm_covariate_designer'     = c('treatment_mean'),
    'two_by_two_designer'            = c('outcome_means')
)

n_diagnosis_workers <- availableCores() - 1   # number of parallel processes when running diagnoses

default_diag_sims <- 100
default_diag_bootstrap_sims <- 30

