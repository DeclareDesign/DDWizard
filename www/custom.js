/*
 * Custom JavaScript for DDWizard.
 */

var switching_designer = false;


// function to set selection on text in element `el`
// taken from https://stackoverflow.com/a/2838358, Tim Down
function selectElementText(el, win) {
    win = win || window;
    var doc = win.document, sel, range;
    if (win.getSelection && doc.createRange) {
        sel = win.getSelection();
        range = doc.createRange();
        range.selectNodeContents(el);
        sel.removeAllRanges();
        sel.addRange(range);
    } else if (doc.body.createTextRange) {
        range = doc.body.createTextRange();
        range.moveToElementText(el);
        range.select();
    }
}


// register click handler for selecting / copying share link in bookmark modal
// this function can be called from R via shinyjs
shinyjs.registerBookmarkModalClickHandler = function() {
    console.log('registering bookmark modal click handler');
    $('pre.share-url code').on('click', function() {
       selectElementText($('pre.share-url')[0]);
    });
    
    $('#copy-share-url').on('click', function(e) {
        e.stopPropagation();
        e.preventDefault();
        
        selectElementText($('pre.share-url')[0]);
        var copysuccess; // var to check whether execCommand successfully executed
        try {
            copysuccess = document.execCommand("copy");
        } catch(e) {
            copysuccess = false;
        }
       
        return false;
    });
};


// register click handlers in bookmark modal
// this function can be called from R via shinyjs
shinyjs.unregisterBookmarkModalClickHandler = function() {
    console.log('unregistering bookmark modal click handler');
    $('pre.share-url code').off("click", "**");
    $('#copy-share-url').off("click", "**");
};


// initializations after page is completely loaded
$(function() {
    console.log('DDWizard init');
    
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