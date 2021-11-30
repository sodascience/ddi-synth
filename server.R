library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(truncnorm)

backend <- new.env()
source("backend.R", backend)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    gen_dat <- eventReactive(input$click, {
        withProgress(message = 'Generating data', value = 0, {
            # validate url
            setProgress(detail = "Validating URL", value = 1/10)
            page_url <- backend$validate_url(input$url)
            
            # download xml
            setProgress(detail = "Downloading XML", value = 2/10)
            xml_loc <- backend$download_metadata(page_url)
            
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
                gen <- backend$gen_column(N, var_int, var_fmt, sum_stats)
                
               
                # add to data
                var_name <- xml_attr(var_def, "name")
                df[[var_name]] <- gen
            }
            setProgress(detail = "Done", value = 1)
        })
        
        df
    })
    

    output$syn_dat <- DT::renderDataTable({
        dat <- gen_dat()
        dat %>% mutate(across(where(is.numeric), round, digits = 2))
    }, width = "100%", options = list(scrollX = TRUE), selection = 'none', server = TRUE)
    
    output$dwnload <- downloadHandler(
        filename = "synthetic_data.csv",
        content = function(file) {
            write.csv(gen_dat(), file, row.names = FALSE)
        }, 
        contentType = "text/csv"
    )

})

