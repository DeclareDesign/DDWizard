# DeclareDesign Wizard

A [Shiny app](http://shiny.rstudio.com/) for creating, editing and diagnosing research designs.

Authors: 

- Clara Bicalho <clara.bicalho@wzb.eu>
- Sisi Huang <sisi.huang@wzb.eu>
- Markus Konrad <markus.konrad@wzb.eu>


## Dependencies

**From GitHub:**

- shinymaterial 0.5.5.900 or newer (`select_material_tab()` is required): `devtools::install_github("ericrayanderson/shinymaterial")`

**From CRAN:**


- DeclareDesign **>= 0.18.0**
- DesignLibrary **>= 0.1.4**
- shiny **>= 1.3.2**
- shinyalert **>= 1.0**
- shinyBS **>= 0.61**
- shinyjs **>= 1.0**
- stringr
- rlang **0.3.4 (>= 0.4.0 produces problems)** 
- dplyr **0.8.1**
- stringi
- ggplot2
- htmlwidgets **1.3**
- htmltools **0.3.6**
- digest
- MASS

Optional for parallel processing during simulation:

- future **<= 1.12.0 (>= 1.13.0 produces problems)**
- future.apply

Optional for running tests:

- RUnit
- shinytest

## Overview of files

This shiny app displays several tabs, each of which is a separate shiny module (http://shiny.rstudio.com/articles/modules.html) implemented in the respective "tab_ ... .R" file. Hence `app.R` only implements the "skeleton" of the application.

- `app.R`: Shiny application skeleton
- `tab_design.R`: "Design" tab for loading and manipulating existing designs
- `tab_inspect.R`: "Diagnose" tab for visual design inspection
- `common.R`: Common utility functions
- `inspect_helpers.R`: Utility functions for design inspection
- `uihelpers.R`: UI related utility functions
- `conf.R`: Configuration options

## Documentation

The `_docs` folder contains the following documentation files that provide an overview:

- `ddnotes.Rmd`: introduces designers from the DesignLibrary package from a DDWizard development perspective; read this first
- `app_structure.Rmd`: gives and overview about the source code structure for DDWizard and some details that are quite specific for this app such as use of namespaces, use of Shiny UI extension packages as well as some notes on reactivity and running diagnoses


## Tests

### Unit tests

Unit tests are implemented in the *tests* folder with [RUnit](https://cran.r-project.org/web/packages/RUnit/index.html).

A shortcut to run all tests was added to the Makefile so you can run in the console:

```
make run_tests
```

### Functional tests with shinytest

We tried to setup [shinytest](https://rstudio.github.io/shinytest/) for DDWizard to record app states and then replay them to check automatically if (and where) the app doesn't provide the expected output if we changed the source or updated one of its dependency packages. Unfortunately, shinytest doesn't work well with our app and will fail with [an already reported error](https://github.com/rstudio/shinytest/issues/144) for all but the very simplest test, which is available in `tests/startup.R`.

Once the shinytest package works with our package, we should generate more test cases like this:

```R
library(shinytest)
recordTest(seed = 1234)    # setting a seed is important since simulations need to be the same
```

And test them via:

```R
library(shinytest)
testApp()                  # optionally provide `testnames = ...`
```
