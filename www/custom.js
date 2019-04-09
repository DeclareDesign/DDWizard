/*
 * Custom JavaScript for DDWizard.
 */

$(function() {
    // register "click" handler for items in tab menu
    $('ul.tabs.tabs-fixed-width li').on('click', function(e) {
        // find out the currently active tab
        var cur_tab = $(this).find('a').attr('href').substring(1);
        // send it directly to the server (see https://shiny.rstudio.com/articles/communicating-with-js.html)
        Shiny.setInputValue('current_tab', cur_tab);
    });
});