suppressMessages(library(rvest))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(readr))

GET_HTML <- function(url){
  measures <- list()
  for ( i in 1:length(url)){
    
    webpage <- read_html(url[[i]])
    
    # Get table nodes
    raw_table <- html_table(webpage, '.table', fill = TRUE, header = FALSE, trim = TRUE)
    
    pub_title <- html_nodes(webpage, ".publication-title") %>%
      html_text()
    
    # Select the desired table
    main <- as.data.frame(raw_table[[1]])
    
    # Clean trash (automated)
    main_red <- main[grep(pattern = "Outcomes",
                          main$X1)[1]:(grep(pattern = "\\*",
                                            main$X1) - 1), ]
    if(any(main_red[1, ] %in% "Comments")){
      main_red <- main_red[,-which(main_red[1, ] %in% "Comments")]
    } 
    
    # columns to remove 
    keys <- c("Anticipated", "Illustrative")
    to_remove <- grep(paste(keys, collapse = "|"), main_red[1, ])
    
    main_measures <- unique(main_red[, setdiff(1:ncol(main_red), to_remove)])  
    colnames(main_measures) <- as.character(main_measures[1, ])
    main_measures <- main_measures[-1, ]
    
    main_measures$pub_title <- NA
    main_measures$pub_title[1] <- pub_title
    main_measures$URL <- NA
    main_measures$URL[1] <- url[i]
    measures[[i]] <- main_measures
  }
  return(measures)
}
