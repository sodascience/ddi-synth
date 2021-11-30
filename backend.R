# backend functions for ddi-synth

#' Validate and return url
#' 
#' @param url_raw string of url
#' 
#' @retun url as httr object
validate_url <- function(url_raw) {
  url <- parse_url(url_raw)
  if (is.null(url$scheme)) url$scheme <- "https"
  validate({
    res <- tryCatch(HEAD(build_url(url)), error = function(e) stop("Enter a valid URL"))
    if (!identical(status_code(res), 200L)) stop("Enter a valid URL.")
  })
  return(url)
}

download_metadata <- function(dataverse_url) {
  dataverse_url$path <- "api/datasets/export"
  dataverse_url$query$exporter <- "ddi"
  xml_url <- build_url(dataverse_url)
  if (!identical(status_code(HEAD(xml_url)), 200L))
    stop("XML Metadata schema could not be found.")
  
  xml_loc <- tempfile(fileext = ".xml")
  return(download_xml(xml_url, xml_loc, quiet = TRUE))
}


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
