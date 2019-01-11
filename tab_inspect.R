# UI and server module for "Inspect" tab.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

### UI ###

inspectTabUI <- function(id, label = 'Inspect') {
    nspace <- NS(id)
    
    # "Inspect" tab
    material_tab_content(
        tab_id = id,
        material_row(
            material_column(   # left: design parameters for comparison
                width = 3,
                uiOutput(nspace("compare_design_parameters"))    # display not-fixed parameters of a design / allow to define sequences
                # for comparison in plots
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
                material_card("Diagnostic plots",
                              uiOutput(nspace('plot_message')),
                              actionButton(nspace('update_plot'), 'Update plot'),
                              plotOutput(nspace('plot_output')),
                              downloadButton(nspace("download_plot"), label = "Download plot", disabled = "disabled")
                ),
                bsCollapse(id='inspect_sections_container',
                           bsCollapsePanel('Diagnosands',
                                           textOutput(nspace("section_diagnosands_message")),
                                           dataTableOutput(nspace("section_diagnosands_table")),
                                           downloadButton(nspace("section_diagnosands_download_subset"),
                                                          label = "Download above table", disabled = "disabled"),
                                           downloadButton(nspace("section_diagnosands_download_full"),
                                                          label = "Download full diagnosands table", disabled = "disabled"))
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
        diagnosands = NULL              # diagnosands for current plot in "inspect" tab
    )
    
    # reactive function to run diagnoses and return the results once "Update plot" is clicked
    get_diagnoses_for_plot <- eventReactive(input$update_plot, {
        req(design_tab_proxy$react$design, design_tab_proxy$react$design_argdefinitions)
        
        # get all arguments from the left side pane in the "Inspect" tab
        insp_args <- get_args_for_inspection(design_tab_proxy$react$design, design_tab_proxy$react$design_argdefinitions, input)
        
        # only if at least one argument is a sequence (i.e. its length is > 1) for comparison,
        # run the diagnoses and return a result
        if (max(sapply(insp_args, length)) > 1) {
            print('will run diagnoses with arguments:')
            print(insp_args)
            
            # run diagnoses and get results
            isolate({
                if (length(input$plot_conf_diag_param_param) == 0) {
                    diag_param_alpha <- 0.05
                } else {
                    diag_param_alpha <- input$plot_conf_diag_param_param
                }
                
                diag_results <- run_diagnoses(design_tab_proxy$react$design, insp_args,
                                              sims = input$simconf_sim_num,
                                              bootstrap_sims = input$simconf_bootstrap_num,
                                              diag_param_alpha = diag_param_alpha,
                                              use_cache = !input$simconf_force_rerun,
                                              advance_progressbar = 1/6)
            })
            
            return(diag_results)
        } else {
            return(NULL)
        }
        
        
    })
    
    # get subset data frame of diagnosands for display and download
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
        
        cols <- c(cols, 'estimator_label', 'term', input$plot_conf_diag_param, paste0('se(', input$plot_conf_diag_param, ')'))
        
        # return data frame subset
        react$diagnosands[cols]
    })
    
    ### output elements ###
    
    # left: design parameters to inspect
    output$compare_design_parameters <- renderUI({
        d_args <- design_tab_proxy$design_args()
        print(d_args)
        isolate({
            # set defaults: use value from design args in design tab unless a sequence of values for arg comparison
            # was defined in inspect tab
            defaults <- sapply(names(d_args), function(argname) {
                ifelse(is.null(input[[paste0('inspect_arg_', argname)]]) || length(parse_sequence_string(input[[paste0('inspect_arg_', argname)]])) < 2,
                       as.character(d_args[[argname]]),
                       input[[paste0('inspect_arg_', argname)]])
            }, simplify = FALSE)
        })
        
        # set a default value for "N" the first time
        # but there are some design without N argument 
        d_args_charvec <- as.character(d_args)
        names(d_args_charvec) <- names(d_args)
        if (all(defaults == d_args_charvec) & !is.null(unlist(defaults['N']))) {
            n_int <- as.integer(defaults['N']) 
            defaults['N'] <- sprintf('%d, %d ... %d', n_int, n_int + 10, n_int + 100)
        }
        
        tags$div(create_design_parameter_ui('inspect', design_tab_proxy$react, NS('tab_inspect'),
                                            design_tab_proxy$design_instance,
                                            input = design_tab_proxy$input,
                                            defaults = defaults))
    })
    
    # make the plot reactive
    plotinput <- reactive({
        n_steps = 6
        withProgress(message = 'Simulating data and generating plot...', value = 0, {
            incProgress(1/n_steps)
            diag_results <- get_diagnoses_for_plot()
            req(diag_results)
            
            plotdf <- diag_results$diagnosands_df
            # when the coefficients are empty 
            if(input$plot_conf_coefficient != ""){
                plotdf <- plotdf[plotdf$estimator_label == input$plot_conf_estimator & plotdf$term == input$plot_conf_coefficient,]   
            }else{
                plotdf <- plotdf[plotdf$estimator_label == input$plot_conf_estimator,]   
            }
            
            react$diagnosands <- plotdf
            
            # the bound value of confidence interval: diagnosand values +/-SE*1.96
            # don't isolate this, because we can change the diagnosand on the fly (no reevaluation necessary)
            plotdf$diagnosand_min <- plotdf[[input$plot_conf_diag_param]] - plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]] * 1.96
            plotdf$diagnosand_max <- plotdf[[input$plot_conf_diag_param]] + plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]] * 1.96
            
            # base aesthetics for line plot
            isolate({  # isolate all other parameters used to configure the plot so that the "Update plot" button has to be clicked
                aes_args <- list(
                    'x' = input$plot_conf_x_param,   
                    'y' = input$plot_conf_diag_param,
                    'ymin' = 'diagnosand_min',
                    'ymax' = 'diagnosand_max'
                    
                )
                
                # if the "color" parameter is set, add it to the aeshetics definition
                if (isTruthy(input$plot_conf_color_param) && input$plot_conf_color_param != '(none)') {
                    plotdf$color_param <- factor(plotdf[[input$plot_conf_color_param]])
                    aes_args$color <- 'color_param'
                    aes_args$fill <- 'color_param'
                    aes_args$group <- 'color_param'
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
                
                print(p)
                
            })
        })
    })
    
    # -------------- center: messages for plot --------------
    
    output$plot_message <- renderUI({
        if (is.null(react$diagnosands)) {
            return(HTML('<p>Specify values for arguments that you want to diagnose 
                        the design with on the left by supplying scalar values, lists
                        of values (like 10, 20, 30) or ranges with a step size
                        (like 10, 20, ..., 100).</p>
                        <p>On the right side you can choose which arguments should
                        be mapped to which visual property of the diagnosis plot
                        and which type of diagnosand should be used. Finally click
                        "Update plot" to simulate data and run the diagnoses.<br><br></p>'))
        } else {
            return('')
        }
    })
    
    
    # -------------- center: plot output --------------
    
    output$plot_output <- renderPlot({
        print(plotinput())
        shinyjs::enable('download_plot')
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
            print(plotinput())
            dev.off()
        }
    )
    
    # center below plot: diagnosands table message
    output$section_diagnosands_message <- renderText({
        if (is.null(react$diagnosands)) {
            return('Not data yet. Set comparison parameters and generate a plot first.')
        }
    })
    
    # center below plot: diagnosands table
    output$section_diagnosands_table <- renderDataTable({
        get_diagnosands_for_display()
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      paging = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthChange = FALSE,
                      scrollX = TRUE)
    )
    
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
    
    # right: inspection plot configuration
    output$plot_conf <- renderUI({
        if (is.null(design_tab_proxy$react$design) || is.null(design_tab_proxy$react$design_argdefinitions)) {
            boxes <- list(p('Load a design first'))
        } else {
            # create list of input elements, all with a common prefix
            nspace <- NS('tab_inspect')
            inp_prefix <- 'plot_conf_'
            boxes <- list()
            
            # create the design instance and get its estimates
            d <- design_tab_proxy$design_instance()
            d_estimates <- draw_estimates(d)
            
            # we need to find out the set of available diagnosands
            available_diagnosands <- DeclareDesign:::default_diagnosands(NULL)$diagnosand_label
            
            # old approach: run a minimal diagnosis
            # minimal_diag <- diagnose_design(d, sims = 1, bootstrap_sims = 1)
            # available_diagnosands <- minimal_diag$diagnosand_names
            
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
            inp_diag_param_id <- paste0(inp_prefix, "diag_param")
            inp_diag_param <- selectInput(nspace(inp_diag_param_id), "Diagnosand (y-axis)",
                                          choices = available_diagnosands,
                                          selected = input[[inp_diag_param_id]])
            boxes <- list_append(boxes, inp_diag_param)
            
            # 3b. optional: diagnosand parameter
            if (length(input[[inp_diag_param_id]]) > 0 && input[[inp_diag_param_id]] == 'power') {
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
            
            
            # 4. CI check box 
            inp_con_int_param_id <- paste0(inp_prefix, "confi_int_id")
            inp_con_int_param <- checkboxInput(nspace(inp_con_int_param_id), label = "Show confidence interval", value = TRUE)
            boxes <- list_append(boxes, inp_con_int_param)
            
            # 5. main inspection parameter (x-axis)
            insp_args <- get_args_for_inspection(design_tab_proxy$react$design, design_tab_proxy$react$design_argdefinitions, input)
            insp_args_lengths <- sapply(insp_args, length)
            variable_args <- names(insp_args_lengths[insp_args_lengths > 1])
            
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
        
        do.call(material_card, c(title="Plot configuration", boxes))
    })
}