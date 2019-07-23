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

args_control_skip_design_args <- c('design_name', 'args_to_fix')

n_diagnosis_workers <- availableCores() - 1   # number of parallel processes when running diagnoses

default_diag_sims <- 100
default_diag_bootstrap_sims <- 30

# Cookie configuration
jsCode <- '
  shinyjs.getcookie = function(params) {
var cookie = Cookies.get("id");
if (typeof cookie !== "undefined") {
Shiny.onInputChange("jscookie", cookie);
} else {
var cookie = "";
Shiny.onInputChange("jscookie", cookie);
}
}
shinyjs.setcookie = function(params) {
Cookies.set("id", escape(params), { expires: 0.5 });
Shiny.onInputChange("jscookie", params);
}
shinyjs.rmcookie = function(params) {
Cookies.remove("id");
Shiny.onInputChange("jscookie", "");
}
'