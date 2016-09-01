# Read all worksheets from Excel document
readxl_allsheets <- function(filename, ...) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, ...))
  names(x) <- sheets
  x
}

# Collapse string with commas and Oxford comma final entry
ox_comma <- function(string) {
  if (length(string) == 1) out <- string
  else if (length(string) == 2) out <- paste(string[1], string[2], sep = " and ")
  else out <- paste(paste(head(string, -1), collapse = ", "), 
                    tail(string, 1), sep = ", and ")
  out
}

convert_length <- function(data, from = c("in", "ft", "cm", "m"), to = c("in", "ft", "cm", "m")) {
  conv <- 1
  from <- match.arg(from)
  to <- match.arg(to)
  if (from == "ft") {
    from <- "in"
    data <- data * 12
  }
  if (from == "m") {
    from <- "cm"
    data <- data * 100
  }
  if (to == "ft") {
    to <- "in"
    conv <- 12
  }
  if (to == "m") {
    to <- "cm" 
    conv <- 100
  }
  out <- switch(from,
                'in' = grid::convertX(grid::unit(data, "in"), to), 
                'cm' = grid::convertX(grid::unit(data, "cm"), to))
  out <- as.numeric(out) / conv
  return(out)
}

# Replace waterData function to allow single day query
importDVs_1day <- function (staid, code = "00060", stat = "00003", sdate = "1851-01-01", 
                       edate = as.Date(Sys.Date(), format = "%Y-%m-%d")) 
{
  base_url <- "http://waterservices.usgs.gov/nwis/dv?"
  url <- paste(base_url, "site=", staid, "&parameterCd=", code, 
               "&statCd=", stat, sep = "")
  url <- paste(url, "&startDt=", sdate, "&endDt=", edate, sep = "")
  doc <- XML::xmlTreeParse(url, getDTD = FALSE, useInternalNodes = TRUE)
  r <- XML::xmlRoot(doc)
  val <- as.numeric(XML::xmlValue(r[[2]][[3]][[1]]))
  Attribute <- XML::xmlApply(r[[2]][[3]], XML::xmlAttrs)
  N <- length(val)
  NoDataValue <- XML::xmlValue(r[["timeSeries"]][["variable"]][["NoDataValue"]])
  NoDataValue <- as.integer(NoDataValue)
  dates <- vector(mode = "character", length = 1)
  dates <- as.Date(dates, "%Y-%m-%d")
  df <- data.frame(staid, val)
  df
}

points_to_line <- function(data, lon, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(lon, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

CB_classify <- function(sav_vals) {
  pacman::p_load(dplyr)
  # Calculate estimate of total area covered by SAV
  # i.e., estimated proportion per cell * cell area
  # cells are 1 ha, so simple sum = total ha
  est_ha <- sum(sav_vals)
  CB_bins <- c(0, 0.1, 0.4, 0.7, 1.01)
  CB_labels = data.frame(class = c("1", "2", "3", "4"),
                         label = c("Very sparse (< 10%)", 
                                   "Sparse (10 - 40%)", 
                                   "Moderate (40 - 70%)", 
                                   "Dense (> 70%)"),
                         stringsAsFactors = FALSE)
  sav_mat <- cut(sav_vals, breaks = CB_bins, labels = c("1", "2", "3", "4"), right = FALSE)
  sav_df <- data.frame(table(sav_mat)) %>% 
    mutate(class = as.character(sav_mat),
           area_ha = Freq) %>%
    dplyr::select(class, area_ha)
  out <- left_join(CB_labels, sav_df, by = "class") %>%
    mutate(area_ha = ifelse(is.na(area_ha), 0, area_ha),
           LM_SAV_ha = est_ha)
  return(out)

}

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

# Function to convert change in additive predictor to corresponding
# proportional increase in SAV coverage, and vice versa
# `inverse` = FALSE converts change in centered additive predictor
#             to proportional change in SAV
# `inverse` = TRUE converts from proportional change in SAV to scale
#             of centered additive predictor
prop_change <- function(x, inverse = FALSE) {
  if (!inverse) {
    base <- 0.5
    diff <- VGAM::cauchit(x, inverse = TRUE) - base
    pChange <- diff/base
    pChange
  } else {
    base <- 0.5
    diff <- x * base
    cauch <- VGAM::cauchit(diff + base)
    cauch
  }
}

