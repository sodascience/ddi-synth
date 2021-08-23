#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(truncnorm)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    gen_dat <- eventReactive(input$click, {
        withProgress(message = 'Generating data', value = 0, {
            # validate url
            setProgress(detail = "Validating URL", value = 1/10)
            url <- parse_url(input$url)
            validate({
                res <- tryCatch(HEAD(build_url(url)), error = function(e) stop("Enter a valid URL"))
                if (!identical(status_code(res), 200L)) stop("Enter a valid URL.")
            })
            
            # download xml
            setProgress(detail = "Downloading XML", value = 2/10)
            ddi_url <- url
            ddi_url$path <- "api/datasets/export"
            ddi_url$query$exporter <- "ddi"
            xml_loc <- tempfile(fileext = ".xml")
            if (!identical(status_code(HEAD(build_url(ddi_url))), 200L))
                stop("XML Metadata schema could not be found.")
            
            download_xml(build_url(ddi_url), xml_loc, quiet = TRUE)
            
            # parsing xml for variables
            setProgress(detail = "Parsing XML", value = 3/10)
            ddi <- read_xml(xml_loc)
            var_defs <- xml_find_all(ddi, "//d1:var")
            if (length(var_defs) == 0) stop("No variable-level info available.")
            
            # generating data
            setProgress(detail = "Generating data", value = 5/10)
            N <- parse_integer(xml_text(xml_find_first(ddi, ".//d1:caseQnty")))
            df <- tibble(.rows = N)
            for (var_def in var_defs) {
                
                # variable format
                vfm_xml <- xml_find_all(var_def, ".//d1:varFormat")
                var_fmt <- xml_attr(vfm_xml, "type")
                
                # variable interval 
                var_int <- xml_attr(var_def, "intrvl")
                
                # summary statistics
                sst_xml <- xml_find_all(var_def, ".//d1:sumStat")
                sum_stats <- as.list(parse_number(xml_text(sst_xml), na = c("", ".", "NA")))
                names(sum_stats) <- xml_attr(sst_xml, "type")
                
                # sample values
                gen <- gen_column(N, var_int, var_fmt, sum_stats)
                
               
                # add to data
                var_name <- xml_attr(var_def, "name")
                df[[var_name]] <- gen
            }
            setProgress(detail = "Done", value = 1)
        })
        
        df
    })
    

    output$syn_dat <- DT::renderDataTable({
        round(gen_dat(), 2)
    }, width = "100%", options = list(scrollX = TRUE), selection = 'none')
    
    output$dwnload <- downloadHandler(
        filename = "synthetic_data.csv",
        content = function(file) {
            write.csv(gen_dat(), file, row.names = FALSE)
        }, 
        contentType = "text/csv"
    )

})


#' Generate column of data
#' 
#' Function to generate 1 column of synthetic data
#' 
#' @param N total number of rows in the final data frame
#' @param discrete whether to generate discrete data
#' @param sum_stats list with the following options (allowed to be NA): 
#'   - stdev
#'   - medn
#'   - invd
#'   - vald
#'   - max
#'   - mean
#'   - mode
#'   - min
gen_column <- function(N, var_int, var_fmt, sum_stats) {
    
    if (length(sum_stats) == 0) {
        if (var_fmt == "character") {
            return(rep(NA_character_, N))
        } 
        
        return(rep(NA, N))
    }
    
    
    # first, check if there are missing values & how many values to generate
    has_missings <- FALSE
    n_vals <- N
    if (is.numeric(sum_stats$vald) && sum_stats$vald != N) {
        has_missings <- TRUE
        n_vals <- sum_stats$vald
    } else if (is.numeric(sum_stats$invd) && sum_stats$invd > 0) {
        has_missings <- TRUE
        n_vals <- N - sum_stats$invd
    }
    
    # if mean is NA but median / mode isn't, use those as mean
    if (is.na(sum_stats$mean)) {
        if (!is.na(sum_stats$medn)) sum_stats$mean <- sum_stats$medn
        if (!is.na(sum_stats$mode)) sum_stats$mean <- sum_stats$mode
    }
    
    
    if ((!is.na(sum_stats$min) & !is.na(sum_stats$max)) && 
        sum_stats$min %in% c(0, 1) && sum_stats$max %in% c(N - 1, N)) {
        
        # try ID column sampling (e.g., row numbers)
        ids  <- sum_stats$min:sum_stats$max
        vals <- if (has_missings) sort(sample(ids, n_vals)) else ids
        vals <- as.integer(vals)
        
    } else if (!is.na(sum_stats$min) & !is.na(sum_stats$max) & !is.na(sum_stats$mean) & !is.na(sum_stats$stdev)) {
        
        # try truncated normal sampling: need min, max, mean, sd
        vals <- rtruncnorm(n_vals, sum_stats$min, sum_stats$max, sum_stats$mean, sum_stats$stdev)
        
    } else if (!is.na(sum_stats$mean) & !is.na(sum_stats$stdev)) {
        
        # try normal sampling: need mean, sd
        vals <- rnorm(n_vals, sum_stats$mean, sum_stats$stdev)
        
    } else if (!is.na(sum_stats$min) & !is.na(sum_stats$max)) {
        
        # try uniform sampling: need min, max
        vals <- runif(n_vals, sum_stats$min, sum_stats$max)
        
    } else {
        warning("Not enough info in current column metadata.")
        return(NA)
    }
    
    # handle missing
    if (has_missings) {
        gen <- rep(NA, N)
        idx <- sample(N, n_vals)
        gen[idx] <- vals
    } else {
        gen <- vals
    }
    
    
    # discretize
    if (var_int == "discrete") {
        gen <- as.integer(round(gen))
    }
    
    gen
}
