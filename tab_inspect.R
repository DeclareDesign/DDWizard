# UI and server module for "Inspect" tab.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

### CONFIG ###

diagnosis_table_opts <- list(searching = FALSE,
                             ordering = FALSE,
                             paging = TRUE,
                             pageLength = 10,
                             info = FALSE,
                             lengthChange = FALSE,
                             scrollX = TRUE)

### UI ###

inspectTabUI <- function(id, label = 'Inspect') {
    nspace <- NS(id)
    nspace_design <- NS('tab_design')
    
    # "Inspect" tab
    material_tab_content(
        tab_id = id,
        material_row(
            material_column(   # left: design parameters for comparison
                width = 3,
                material_card("Compare design parameters",
                    conditionalPanel(paste0("output['", nspace_design('design_loaded'), "'] != ''"),
                        uiOutput(nspace("compare_design_parameters"))    # display not-fixed parameters of a design / allow to define sequences
                    ),
                    conditionalPanel(paste0("output['", nspace_design('design_loaded'), "'] == ''"),
                        p('Load a design first')
                    )
                )
            ),
            material_column(   # center: inspection output
                width = 6,
                bsCollapse(id=nspace('inspect_sections_simconf_container'),
                           bsCollapsePanel('Configure simulations',
                                           checkboxInput(nspace('simconf_force_rerun'), label = 'Always re-run simulations (disable cache)'),
                                           numericInput(nspace("simconf_sim_num"), label = "Num. of simulations",
                                                        value = default_diag_sims,
                                                        min = 1, max = 1000, step = 1),
                                           numericInput(nspace("simconf_bootstrap_num"), label = "Num. of bootstraps",
                                                        value = default_diag_bootstrap_sims,
                                                        min = 1, max = 1000, step = 1))),
                conditionalPanel(paste0("output['", nspace('all_design_args_fixed'), "'] === false"),
                    material_card("Diagnostic plots",
                                  uiOutput(nspace('plot_message')),
                                  div(uiOutput(nspace("update_plot")), style = "margin-bottom:10px"),
                                  uiOutput(nspace('plot_output')),
                                  downloadButton(nspace("download_plot"), label = "Download plot", disabled = "disabled")
                    ),
                    bsCollapse(id='inspect_sections_container',
                               bsCollapsePanel('Diagnosis',
                                               uiOutput(nspace("section_diagnosands_message")),
                                               dataTableOutput(nspace("section_diagnosands_table")),
                                               downloadButton(nspace("section_diagnosands_download_subset"),
                                                              label = "Download above table", disabled = "disabled"),
                                               downloadButton(nspace("section_diagnosands_download_full"),
                                                              label = "Download full diagnosands table", disabled = "disabled"))
                    )
                ),
                conditionalPanel(paste0("output['", nspace('all_design_args_fixed'), "'] === true"),
                    material_card("Diagnostic plots",
                        HTML('<p>Diagnosis plot not available since all parameters were set to <i>fixed</i>.</p>'),
                        actionButton(nspace('update_plot_all_fixed'), 'Run single design diagnosis'),
                        uiOutput(nspace("single_diagnosands_message")),
                        dataTableOutput(nspace("single_diagnosands_table"))
                    )
                )
            ),
            material_column(   # right: plot configuration
                width = 3,
                uiOutput(nspace("plot_conf"))
            )
        )
    )
}

### Server ###

inspectTab <- function(input, output, session, design_tab_proxy) {
    react <- reactiveValues(
        cur_design_id = NULL,       # current design name used in inspection (coming from design tab)
        diagnosands = NULL,         # diagnosands for current plot in "inspect" tab
        diagnosands_cached = FALSE, # records whether current diagnosand results came from cache
        diagnosands_call = NULL,    # a closure that actually calculates the diagnosands, valid for current design
        insp_args_used_in_plot = NULL  # last used design parameters used in plot
    )
    
    # Run diagnoses using inspection arguments `insp_args`
    run_diagnoses_using_inspection_args <- function(insp_args, advance_progressbar = 0) {
        isolate({
            if (length(input$plot_conf_diag_param_param) == 0) {
                diag_param_alpha <- 0.05
            } else {
                diag_param_alpha <- input$plot_conf_diag_param_param
            }
            
            diag_res <- run_diagnoses(design_tab_proxy$react$design, insp_args,
                                      sims = input$simconf_sim_num,
                                      bootstrap_sims = input$simconf_bootstrap_num,
                                      diagnosands_call = react$diagnosands_call(diag_param_alpha),
                                      use_cache = !input$simconf_force_rerun,
                                      advance_progressbar = advance_progressbar)
        })
        
        diag_res
    }
    
    # reactive function to run diagnoses and return the results once "Update plot" is clicked
    get_diagnoses_for_plot <- eventReactive(input$update_plot, {
        req(design_tab_proxy$react$design, design_tab_proxy$react$design_argdefinitions)
        
        # get all arguments from the left side pane in the "Inspect" tab
        d_args <- design_tab_proxy$design_args()
        d_args_vecinput <- sapply(d_args, function(x) { length(x) > 1 })

        insp_args <- get_args_for_inspection(design_tab_proxy$react$design,
                                             design_tab_proxy$react$design_argdefinitions,
                                             input,
                                             design_tab_proxy$get_fixed_design_args(),
                                             design_tab_proxy$input,
                                             names(d_args_vecinput)[d_args_vecinput])
        
        if (max(sapply(insp_args, length)) == 0) {
            # only if at least one argument is a sequence (i.e. its length is > 1) for comparison,
            # run the diagnoses and return a result
            return(NULL)
        }
        
        print('will run diagnoses with arguments:')
        print(insp_args)
        
        # save the current state of the inspection parameters
        react$insp_args_used_in_plot <- insp_args
        
        # run diagnoses and get results
        diag_results <- run_diagnoses_using_inspection_args(insp_args, advance_progressbar = 1/6)
        react$diagnosands_cached <- diag_results$from_cache
        plotdf <- diag_results$results$diagnosands_df
        
        # when the coefficients are empty 
        if (isTruthy(input$plot_conf_coefficient)) {
            plotdf <- plotdf[plotdf$estimator_label == input$plot_conf_estimator & plotdf$term == input$plot_conf_coefficient,]   
        } else if (isTruthy(input$plot_conf_estimator)) {
            plotdf <- plotdf[plotdf$estimator_label == input$plot_conf_estimator,]   
        }
        
        react$diagnosands <- plotdf
        diag_results$results$diagnosands_df_for_plot <- plotdf
        
        diag_results
    })
    
    # reactive function to run diagnoses and return the results once "Run single design diagnosis" is clicked
    get_diagnosis_for_single_design <- eventReactive(input$update_plot_all_fixed, {
        req(design_tab_proxy$react$design, design_tab_proxy$react$design_argdefinitions)
        
        args <- design_tab_proxy$design_args()
        argnames <- names(args)
        argnames <- setdiff(argnames, 'fixed')
        insp_args <- args[argnames]
        
        run_diagnoses_using_inspection_args(insp_args)
    })
    
    # get subset data frame of diagnosands for display and download once "Update plot" is clicked
    get_diagnosands_for_display <- reactive({
        req(react$diagnosands)
        
        # set columns to show
        cols <- c(input$plot_conf_x_param)
        
        if (isTruthy(input$plot_conf_color_param) && input$plot_conf_color_param != '(none)') {
            cols <- c(cols, input$plot_conf_color_param)
        }
        if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
            cols <- c(cols, input$plot_conf_facets_param)
        }
       
        if ("term" %in% colnames(react$diagnosands)){
            cols <- c(cols, 'estimator_label', 'term', input$plot_conf_diag_param, paste0('se(', input$plot_conf_diag_param, ')'))
        } else {
            cols <- c(cols, 'estimator_label', input$plot_conf_diag_param, paste0('se(', input$plot_conf_diag_param, ')'))
        }
        
        
        # return data frame subset
        react$diagnosands[cols]
    })
    
    results_cached_message <- reactive({
        if (react$diagnosands_cached) {
            return(p('Results loaded from cached diagnoses. You can disable caching in the top panel "Configure simulations".'))
        } else {
            return('')
        }
    })

    
    ### output elements ###
    
    # hidden (for conditional panel): return TRUE when all designer arguments were fixed, otherwise FALSE
    output$all_design_args_fixed <- reactive({
        req(design_tab_proxy$react$design)
        design_tab_proxy$all_design_args_fixed()
    })
    outputOptions(output, 'all_design_args_fixed', suspendWhenHidden = FALSE)
    
    # hidden output that records current design ID (i.e. designer name) in order to detect changes of the
    # designer and then reset the state of the inspect tab
    output$cur_design_id <- reactive({
        if (!is.null(react$cur_design_id) && react$cur_design_id != design_tab_proxy$react$design_id) {
            # if the designer was changed, reset the reactive values
            react$diagnosands <- NULL
            react$diagnosands_cached <- FALSE
            react$diagnosands_call <- NULL
            react$design_params_used_in_plot <- NULL
        }
        
        react$cur_design_id <- design_tab_proxy$react$design_id
        react$cur_design_id
    })
    outputOptions(output, 'cur_design_id', suspendWhenHidden = FALSE)
    
    # left: design parameters to inspect
    output$compare_design_parameters <- renderUI({
        req(design_tab_proxy$react$design)
        
        if (design_tab_proxy$all_design_args_fixed()) {
            return(HTML('<p>No comparisons available since all parameters were set to <i>fixed</i>.<br></p>'))
        }
        
        d_args <- design_tab_proxy$design_args()
        d_args_vecinput <- sapply(d_args, function(x) { length(x) > 1 })
        
        isolate({
            # set defaults: use value from design args in design tab unless a sequence of values for arg comparison
            # was defined in inspect tab
            defaults <- sapply(names(d_args), function(argname) {
                arg_inspect_input <- input[[paste0('inspect_arg_', argname)]]
                if (is.null(arg_inspect_input) || length(parse_sequence_string(arg_inspect_input)) < 2) {
                    arg_char <- as.character(d_args[[argname]])
                    if (d_args_vecinput[argname]) {  # vector of vectors input
                        return(sprintf('(%s)', paste(arg_char, collapse = ', ')))
                    } else {
                        return(arg_char)
                    }
                } else {
                    return(arg_inspect_input)
                }
            }, simplify = FALSE)
        })
        
        # set a default value for "N" the first time
        # but there are some design without N argument
        first_arg <- names(d_args)[1]
        if (first_arg == 'N' && is.null(d_args['N'])) first_arg <- names(d_args)[2]
        if (first_arg == 'N') {
            n_int <- as.integer(d_args[[first_arg]])
            defaults['N'] <- sprintf('%d, %d ... %d', n_int, n_int + 10, n_int + 100)
        } else {
            defs <- design_tab_proxy$react$design_argdefinitions
            min_int <- defs$inspector_min[defs$names == first_arg]
            max_int <- defs$inspector_max[defs$names == first_arg]
            step_int <- defs$inspector_step[defs$names == first_arg]
            defaults[first_arg] <- sprintf('%d, %d ... %d', min_int, min_int + step_int, max_int)
        }
        
        param_boxes <- create_design_parameter_ui('inspect', design_tab_proxy$react, NS('tab_inspect'),
                                                  input = design_tab_proxy$input,
                                                  defaults = defaults,
                                                  textarea_inputs = names(d_args_vecinput)[d_args_vecinput])
        tags$div(param_boxes)
    })
    
    # make the plot reactive
    generate_plot <- reactive({
        n_steps = 6
        withProgress(message = 'Simulating data and generating plot...', value = 0, {
            incProgress(1/n_steps)
            diag_res <- get_diagnoses_for_plot()
            
            if (is.null(diag_res)) {
                return(NULL)
            }
            
            plotdf <- diag_res$results$diagnosands_df_for_plot
            
            isolate({  # isolate all other parameters used to configure the plot so that the "Update plot" button has to be clicked
                # the bound value of confidence interval: diagnosand values +/-SE*1.96
                plotdf$diagnosand_min <- plotdf[[input$plot_conf_diag_param]] - plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]] * 1.96
                plotdf$diagnosand_max <- plotdf[[input$plot_conf_diag_param]] + plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]] * 1.96
            
                # base aesthetics for line plot
                aes_args <- list(
                    'x' = input$plot_conf_x_param,   
                    'y' = input$plot_conf_diag_param,
                    'ymin' = 'diagnosand_min',
                    'ymax' = 'diagnosand_max'
                    
                )
                
                # if the "color" parameter is set, add it to the aeshetics definition
                if (isTruthy(input$plot_conf_color_param) && input$plot_conf_color_param != '(none)') {
                    plotdf[[input$plot_conf_color_param]] <- factor(plotdf[[input$plot_conf_color_param]])
                    aes_args$color <- input$plot_conf_color_param
                    aes_args$fill <- input$plot_conf_color_param
                    aes_args$group <- input$plot_conf_color_param
                } else {
                    aes_args$group <- 1
                }
                
                # if the "facets" parameter is set, add it to the aeshetics definition
                if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
                    plotdf$facets_param <- as.factor(plotdf[[input$plot_conf_facets_param]])
                }
                
                # create aesthetics definition
                aes_definition <- do.call(aes_string, aes_args)
                
                incProgress(1/n_steps)
                
                # create base line plot
                p <- ggplot(plotdf, aes_definition) +
                    geom_line() +
                    geom_point() +
                    scale_y_continuous(name = input$plot_conf_diag_param) +
                    dd_theme() +
                    labs(x = input$plot_conf_x_param)
                
                # add confidence interval if requested
                if (isTruthy(input$plot_conf_confi_int_id)) {
                    p <- p + geom_ribbon(alpha = 0.25, color = 'white')
                }
                
                # add facets if necessary
                if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
                    p <- p + facet_wrap(input$plot_conf_facets_param, ncol = 2, labeller = label_both)
                }
                
                incProgress(1/n_steps)
                
                shinyjs::enable('section_diagnosands_download_subset')
                shinyjs::enable('section_diagnosands_download_full')
                
                p
            })
        })
    })
    
    # -------------- center: messages for plot --------------
    
    output$plot_message <- renderUI({
        if (is.null(react$diagnosands)) {
            return(HTML('<p><b>Top panel:</b> Specify the number of simulations
                        in your diagnosis.</p> 
                        <p><b>Left panel:</b> Specify which design arguments to
                        vary with each design diagnosis. You can input numeric 
                        values separated by a comma (e.g., 10, 20, 30) or provide 
                        ranges of values with one step to create an arithmetic
                        sequence of values (e.g., 10, 20, ..., 100 generates a sequence 
                        from 10 to 100 where the difference in terms is 10).</p>
                        <p><b>Right panel:</b> Choose which arguments should
                        be mapped to which visual property of the diagnosis plot
                        and which diagnosand should be displayed.</p>
                        <p>Click <b>"Run diagnoses"</b> to run the diagnoses and
                        display the updated plot.<br><br></p>'))
        } else {
            results_cached_message()
        }
    })
    
    
    # -------------- center: plot output --------------
    
    # Static action button as default
    output$update_plot <- renderUI({
        nspace <- NS('tab_inspect')
        actionButton(nspace('update_plot'), 'Run diagnoses and update plot')
    })
    
    # Reactive button label
    btn_label <- reactive({
        if (is.null(react$insp_args_used_in_plot)) {
            btn <- 'Run diagnoses and update plot'
        } else {
            d_args <- design_tab_proxy$design_args()
            d_args_vecinput <- sapply(d_args, function(x) { length(x) > 1 })
            
            insp_args <- get_args_for_inspection(design_tab_proxy$react$design,
                                                 design_tab_proxy$react$design_argdefinitions,
                                                 input,
                                                 design_tab_proxy$get_fixed_design_args(),
                                                 design_tab_proxy$input,
                                                 names(d_args_vecinput)[d_args_vecinput])

            if (lists_equal_shallow(react$insp_args_used_in_plot, insp_args)) {
                btn <- 'Update plot'
            } else {
                btn <- 'Run diagnoses and update plot'
            }
        }
        btn
    })
    
    # Action button label gets updated only when reactive inspector values don't change
    observeEvent(btn_label(), { updateActionButton(session, 'update_plot', btn_label()) })
    
    # all the following hassle because Shiny would neither:
    # - accept "auto" as plot height
    # - allow to show/hide the plot inside a conditional panel (the "Run diagnoses" button would not work anymore)
    # - allow to show/hide the plot using shinyjs (same as above)
    
    output$plot_output <- renderUI({
        if (is.null(react$diagnosands)) {
            h <- 1
        } else {
            h <- 400
        }
        
        nspace <- NS('tab_inspect')
        plotOutput(nspace('actual_plot_output'), height = h)
    })
    
    output$actual_plot_output <- renderPlot({
        p <- generate_plot()
        
        if (!is.null(p) && !is.null(react$diagnosands)) {
            shinyjs::enable('download_plot')
        } else {
            shinyjs::disable('download_plot')
        }
        
        print('PRINT PLOT')
        
        p
    })
    
    # -------- download the plot --------
    output$download_plot <- downloadHandler(
        filename = function() {
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnostic_plot.png')
        },
        content = function(file) {
            png(file, width = 1200, height = 900)
            print(generate_plot())
            dev.off()
        }
    )
    
    # center below plot: diagnosands table message
    output$section_diagnosands_message <- renderUI({
        if (is.null(react$diagnosands)) {
            return(p('Missing simulations data. Vary design parameters on the left and click "Run diagnoses".'))
        } else {
            results_cached_message()
        }
    })
    
    output$single_diagnosands_message <- renderUI({
        if (is.null(react$diagnosands)) {
            return(p('Not data yet. Set comparison parameters and generate a plot first.'))
        } else {
            results_cached_message()
        }
    })
    
    # center below plot: diagnosands table
    output$section_diagnosands_table <- renderDataTable({
        get_diagnosands_for_display()
    }, options = diagnosis_table_opts)
    
    # center below plot: download buttons
    output$section_diagnosands_download_subset <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnosands.csv')
        },
        content = function(file) {
            write.csv(get_diagnosands_for_display(), file = file, row.names = FALSE)
        }
    )
    
    output$section_diagnosands_download_full <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnosands_full.csv')
        },
        content = function(file) {
            write.csv(react$diagnosands, file = file, row.names = FALSE)
        }
    )
    
    # diagnosis table for single design
    output$single_diagnosands_table <- renderDataTable({
        diag_res <- get_diagnosis_for_single_design()
        react$diagnosands_cached <- diag_res$from_cache
        select(diag_res$results$diagnosands_df, -c(design_label, n_sims))
    }, options = list_merge(diagnosis_table_opts, list(paging = FALSE)))
    
    # right: inspection plot configuration
    output$plot_conf <- renderUI({
        if (is.null(design_tab_proxy$react$design) || is.null(design_tab_proxy$react$design_argdefinitions)) {
            boxes <- list(p('Load a design first'))
        } else {
            # create list of input elements, all with a common prefix
            nspace <- NS('tab_inspect')
            inp_prefix <- 'plot_conf_'
            boxes <- list()
            args_fixed <- design_tab_proxy$get_fixed_design_args()
            all_fixed <- design_tab_proxy$all_design_args_fixed()
            
            # create the design instance and get its estimates
            d <- design_tab_proxy$design_instance()
            d_estimates <- draw_estimates(d)
            
            # get available diagnosands
            diag_call <- attr(d, 'diagnosands')
            if (is.null(diag_call)) {
                react$diagnosands_call <- function(diag_param_alpha) {  # here we can pass alpha
                    function(data) {
                        DeclareDesign:::default_diagnosands(data, alpha = diag_param_alpha)
                    }
                }
                
                available_diagnosands <- DeclareDesign:::default_diagnosands(NULL)$diagnosand_label
            } else {
                react$diagnosands_call <- function(diag_param_alpha) {  # here we ignore alpha
                    attr(diag_call, 'call')
                }
                
                available_diagnosands <- names(call_args(attr(diag_call, 'call')))
                available_diagnosands <- setdiff(available_diagnosands, c("keep_defaults", "label"))
            }
            
            # 1. estimator
            inp_estimator_id <- paste0(inp_prefix, "estimator")
            inp_estimator <- selectInput(nspace(inp_estimator_id), "Estimator Label",
                                         choices = unique(d_estimates$estimator_label),
                                         selected = input[[inp_estimator_id]])
            boxes <- list_append(boxes, inp_estimator)
            
            # 2. coefficient
            if ("term" %in% names(d_estimates)) {
                coefficients <- d_estimates$term[d_estimates$estimator_label == input[[inp_estimator_id]]]
            } else {
                coefficients <- ""
            }
            
            inp_coeff_id <- paste0(inp_prefix, "coefficient")
            inp_coeff <- selectInput(nspace(inp_coeff_id), "Coefficient",
                                     choices = coefficients,
                                     selected = input[[inp_coeff_id]])
            boxes <- list_append(boxes, inp_coeff)
            
            # 3. diagnosand (y-axis)
            if (!all_fixed) {
                inp_diag_param_id <- paste0(inp_prefix, "diag_param")
                inp_diag_param <- selectInput(nspace(inp_diag_param_id), "Diagnosand (y-axis)",
                                              choices = available_diagnosands,
                                              selected = input[[inp_diag_param_id]])
                boxes <- list_append(boxes, inp_diag_param)
            }
            
            # 3b. optional: diagnosand parameter
            if (all_fixed || (length(input[[inp_diag_param_id]]) > 0 && input[[inp_diag_param_id]] == 'power')) {
                inp_diag_param_param_id <- paste0(inp_prefix, "diag_param_param")
                if (length(input[[inp_diag_param_param_id]]) > 0) {
                    inp_diag_param_param_default <- input[[inp_diag_param_param_id]]
                } else {
                    inp_diag_param_param_default <- 0.05
                }
                inp_diag_param_param <- numericInput(nspace(inp_diag_param_param_id), "Alpha for power",
                                                     min = 0, max = 1, step = 0.01,
                                                     value = inp_diag_param_param_default)
                boxes <- list_append(boxes, inp_diag_param_param)
            }
            
            if (!all_fixed) {
                # 4. CI check box 
                inp_con_int_param_id <- paste0(inp_prefix, "confi_int_id")
                inp_con_int_param <- checkboxInput(nspace(inp_con_int_param_id), label = "Show confidence interval", value = TRUE)
                boxes <- list_append(boxes, inp_con_int_param)
                
                # 5. main inspection parameter (x-axis)
                d_args <- design_tab_proxy$design_args()
                d_args_vecinput <- sapply(d_args, function(x) { length(x) > 1 })
                
                insp_args <- get_args_for_inspection(design_tab_proxy$react$design,
                                                     design_tab_proxy$react$design_argdefinitions,
                                                     input,
                                                     design_tab_proxy$get_fixed_design_args(),
                                                     design_tab_proxy$input,
                                                     names(d_args_vecinput)[d_args_vecinput])
                
                insp_args_lengths <- sapply(insp_args, length)
                variable_args <- names(insp_args_lengths[insp_args_lengths > 1])
                variable_args <- setdiff(variable_args, args_fixed)
                
                inp_x_param_id <- paste0(inp_prefix, "x_param")
                inp_x_param <- selectInput(nspace(inp_x_param_id), "Primary parameter (x-axis)",
                                           choices = variable_args,
                                           selected = input[[inp_x_param_id]])
                boxes <- list_append(boxes, inp_x_param)
                
                # 6. secondary inspection parameter (color)
                variable_args_optional <- c('(none)', variable_args)
                inp_color_param_id <- paste0(inp_prefix, "color_param")
                inp_color_param <- selectInput(nspace(inp_color_param_id), "Secondary parameter (color)",
                                               choices = variable_args_optional,
                                               selected = input[[inp_color_param_id]])
                boxes <- list_append(boxes, inp_color_param)
                
                # 7. tertiary inspection parameter (small multiples)
                inp_facets_param_id <- paste0(inp_prefix, "facets_param")
                inp_facets_param <- selectInput(nspace(inp_facets_param_id), "Tertiary parameter (small multiples)",
                                                choices = variable_args_optional,
                                                selected = input[[inp_facets_param_id]])
                boxes <- list_append(boxes, inp_facets_param)
            }
        }
        
        do.call(material_card, c(title="Plot configuration", boxes))
    })
}