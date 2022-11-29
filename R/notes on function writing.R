# load up package
# creating phase data 
# @retyrn 

# fivreg_recent<- function (x, n=5) {

    # take df slot from x and sort by desc publish date
    # sort by register name (alphabetical) 

   x$df 

    
    #####good way to do this is to build up functionality in the function - and think about the logical steps that you want it to do and #
  
    # can also include checks to see if data is what we expect  
#   such as stopifnot(is.character(df$register))
#    stopifnot(lubridate::is.POSIXct(df$date)) etc. etc.
    
    #dplyr::arrabfe(df, desc(date), register) %>%
    #  dplyr::select(-phase) %>%
    #    
    #   tibble::as.tibble()->sorted_list
    #
    #   sorted_list           
    
    
    #sorted_list <- 
    #  dplyr::arrabfe(df, desc(date), register) %>%
    #  dplyr::select(-phase) %>%
    #    
    #   tibble::as.tibble()
    #
    
 #   calling this to see what we get 
    #   sorted_list     
    
    
#    now looking to get just the top 5
#    df_n <-head(sorteD_LIST$REgister, n) ###n is defaukted to 5 earlier on but can change this in the formular 
    
    
 #   output<- paste(df_n, collapse = ", ")
    
    ###need to make the last comma an and 
    
#    return(output)
    
 
    
    
 # }   
    
    
    
#    can pput the documentation in the same file as the function(
#      if you put @title, @descriptio @details etc. this then appears as the ?function
    # can also @details blah blah \links{link.com}
#    )
    
    
#    cmd shift d uploads changes to the documentation
#    cmd shift l reloads so you can see your changes 
    
    
    
#can stop errors earlier is you have more errors stop in earlier in the proces    
#ege stopifnot() stop() assertthatpackage    