library(RUnit)
library(DesignLibrary)


get_available_designers <- function() {
    all_designers <- grep("designer$", ls(as.environment("package:DesignLibrary")), value = TRUE)
    all_designers[!grepl('^simple_', all_designers)]
}

get_designer <- function(designer_name) {
    getFromNamespace(designer_name, 'DesignLibrary')
}


test.designers_have_valid_definitions <- function() {
    designer_names <- get_available_designers()
    for (d_name in designer_names) {
        d <- get_designer(d_name)
        checkTrue('definitions' %in% names(attributes(d)),
                  paste(d_name, 'definitions exist'))
        
        defs <- attr(d, 'definitions')
        args <- formals(d)
        argnames <- names(args)
        
        checkEquals(lapply(defs, class), list(names = 'character',
                                              tips = 'character',
                                              class = 'character',
                                              vector = 'logical',
                                              min = 'numeric',
                                              max = 'numeric',
                                              inspector_min = 'numeric',
                                              inspector_step = 'numeric'),
                    paste(d_name, 'definitions data frame has expected columns and data types'))
        
        checkEquals(defs$names, argnames,
                    paste(d_name, 'definitions arg. names match function arg. names'))
        checkTrue(sum(is.na(defs$tips)) == 0 && sum(nchar(defs$tips) == 0) == 0,
                  paste(d_name, 'definitions tips are non-NA and filled out'))
        checkTrue(all(defs$class %in% c('integer', 'numeric', 'character', 'logical')),
                  paste(d_name, 'definitions class are correct'))
        checkTrue(sum(is.na(defs[defs$class %in% c('integer', 'numeric'), c('min', 'max', 'inspector_min', 'inspector_step')])) == 0,
                  paste(d_name, 'min, max, inspector_min and inspector_step are all non-NA for numeric/integer args in definitions'))
    }
}


test.designers_can_be_run <- function() {
    designer_names <- get_available_designers()
    for (d_name in designer_names) {
        d <- get_designer(d_name)
        d_instance <- d()
        
        checkEquals(class(d_instance), c('design', 'dd'),
                    paste(d_name, 'design can be created and has correct class'))
        
        d_output <- capture.output({
            d_instance
        })
        
        checkTrue(class(d_output) == 'character' && sum(nchar(d_output)) > 0,
                  paste(d_name, 'design generates output'))
        checkEquals(paste0(d_output[1:3], collapse = ''), 'Design Summary',
                    paste(d_name, 'design generates Design Summary'))
        checkTrue('code' %in% names(attributes(d_instance)),
                  paste(d_name, 'design has code attribute'))
        
        d_code <- attr(d_instance, 'code')
        checkTrue(class(d_code) == 'character' && sum(nchar(d_code)) > 0,
                  paste(d_name, 'design generates code'))
    }
}
