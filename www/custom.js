/*
 * Custom JavaScript for DDWizard.
 */

var switching_designer = false;

$(function() {
    // register "click" handler for items in tab menu
    $('ul.tabs.tabs-fixed-width li').on('click', function() {
        // find out the currently active tab
        var cur_tab = $(this).find('a').attr('href').substring(1);
        // send it directly to the server (see https://shiny.rstudio.com/articles/communicating-with-js.html)
        Shiny.setInputValue('current_tab', cur_tab);
    });
    
    // click handler for "import design" that sets "switching_designer" indicator to true
    // while in the process of switching the designers
    $('#tab_design-import_from_design_lib').on('click', function() {
        switching_designer = true;
        Shiny.setInputValue('tab_design-switching_designer', switching_designer);
    });
    
    // change handler on design name change
    // resets "switching_designer" when design name change was triggered while switching a designer
    $('#tab_design-design_arg_design_name').on('change', function() {
        if (switching_designer) {
            switching_designer = false;
            
            window.setTimeout(function() {  // need to add a little delay b/c design name is changed before UI creation is done
                Shiny.setInputValue('tab_design-switching_designer', switching_designer);
            }, 3000);
        }
    });
    
    window.setTimeout(function() {  // need to add a little delay b/c Shiny is not fully loaded yet
        if (window.location.href.indexOf('_state_id_=') === -1) {   // not restoring a bookmark
            Shiny.setInputValue('show_intro_modal', true);
        }
    }, 1500);
});