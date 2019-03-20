# DeclareDesign Wizard

A [Shiny app](http://shiny.rstudio.com/) for creating, editing and inspecting designs.

Authors: 

- Clara Bicalho <clara.bicalho@wzb.eu>
- Sisi Huang <sisi.huang@wzb.eu>
- Markus Konrad <markus.konrad@wzb.eu>


## Dependencies

**From GitHub:**

- [*DesignLibrary* from branch `enhancements_wizard`](https://github.com/DeclareDesign/DesignLibrary/tree/enhancements_wizard)

**From CRAN:**

- DeclareDesign
- shiny
- shinyalert
- shinymaterial
- shinythemes
- shinyBS
- shinyjs
- ggplot2
- stringr
- rlang
- digest

Optional for parallel processing during simulation:

- future
- future.apply

Optional for running tests:

- RUnit
- shinytest


## Overview of files

This shiny app displays several tabs, each of which is a separate shiny module (http://shiny.rstudio.com/articles/modules.html) implemented in the respective "tab_ ... .R" file. Hence `app.R` only implements the "skeleton" of the application.

- `app.R`: Shiny application skeleton
- `tab_design.R`: "Design" tab for loading and manipulating existing designs
- `tab_inspect.R`: "Inspect" tab for visual design inspection
- `common.R`: Common utility functions
- `inspect_helpers.R`: Utility functions for design inspection
- `uihelpers.R`: UI related utility functions
- `conf.R`: Configuration options

## Tests

### Unit tests

Unit tests are implemented in the *tests* folder with [RUnit](https://cran.r-project.org/web/packages/RUnit/index.html).

A shortcut to run all tests was added to the Makefile so you can run:

```
make run_tests
```

### Functional tests

tbd.

## TODOs

See "Issues" page on GitHub.
