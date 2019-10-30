# Configuration options
#
# Sisi Huang <sisi.huang@wzb.eu>
# Markus Konrad <markus.konrad@wzb.eu>
# Clara Bicalho <clara.bicalho@wzb.eu>
#
# Oct. 2018
#

library(future)

app_title <- "DeclareDesign Wizard"
nav_bar_color <- " light-blue darken-3"

args_control_skip_design_args <- c('design_name', 'args_to_fix')   # for all designers
args_control_skip_specific_designer_args <- list(                  # for specific designers    
    'two_by_two_designer' = c('outcome_means')
)

n_diagnosis_workers <- availableCores() - 1   # number of parallel processes when running diagnoses

default_diag_sims <- 100
default_diag_bootstrap_sims <- 30

