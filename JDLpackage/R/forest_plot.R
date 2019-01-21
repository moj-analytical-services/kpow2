# time_graph
#
#' This function outputs the 'time before first proven reoffence' graph
#' @param data The standard result dataset
#' @param subset Generic text size in graph
#' @param title Text size of confidence intervals
#' @param x_axis Text size of y-axis label
#' @param y_axis Text size of legend text


#subset is either 'all', 'sig' or 'not_sig'

forest_plot = function (data, subset, title, x_axis, y_axis){
  
  tryCatch({
    
    if  ((subset %in% c('all','sig','not_sig')) == FALSE) {
      
      # Check that input is a valid subset
      
      stop("Input to forest_plot is not a valid subset", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      
      if(subset == 'all'){
        df = data
        x = paste0('df$','effect_1y_reoff')
        y = paste0('df$','treat_size')
        CI =paste0('df$','CI')
        lCI ='df$lower_perc'
        uCI ='df$upper_perc'
      }
      else if (subset == 'sig'){
        df = eval(parse(text = "subset(data, data$stat_sig == 'Y')"))
        x = paste0("subset(df, df$stat_sig == 'Y', select = '",'effect_1y_reoff',"')")
        y = paste0("subset(df, df$stat_sig == 'Y', select = '",'treat_size',"')")
        CI = paste0("subset(df, df$stat_sig == 'Y', select = '",'CI',"')")
        lCI = paste0("subset(df, df$stat_sig == 'Y', select = lower_perc)")
        uCI = paste0("subset(df, df$stat_sig == 'Y', select = upper_perc)")
      }
      else if (subset == 'not_sig'){
        df = eval(parse(text = "subset(data, data$stat_sig == 'N')"))
        x = paste0("subset(df, df$stat_sig == 'N', select = '",'effect_1y_reoff',"')")
        y = paste0("subset(df, df$stat_sig == 'N', select = '",'treat_size',"')")
        CI = paste0("subset(df, df$stat_sig == 'N', select = '",'CI',"')")
        lCI = paste0("subset(df, df$stat_sig == 'N', select = 'lower_perc')")
        uCI = paste0("subset(df, df$stat_sig == 'N', select = 'upper_perc')")
      }
      
      
      
      if(subset == 'all'){
        colour = c('red','blue')
        ggplot(df, aes_string(x = x, y = y)) + scale_x_continuous() + scale_y_continuous() +
          
          geom_vline(aes(xintercept  = 0)) +
          
          geom_errorbarh(aes_string(xmin = lCI,
                                    xmax = uCI,
                                    height =150),
                         color = rgb(0, 0, 0, max = 255), size = .2) +
          
          geom_point (aes(color = df$stat_sig)) +
          
          labs(x = "",
               y="Number of proven re-offenders per 100 people") +
          
          theme_bw() +
          
          scale_x_continuous(limits = c(-40, 40)) + 
          
          scale_y_continuous(expand = c(0, 0), breaks=seq(0,9000,1000)) +
          
          theme(axis.line.x = element_line(color="black"), 
                axis.line.y = element_line(color="black"), 
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.border = element_blank()
          ) +
          
          scale_colour_manual(values = c('dark red', rgb(0, 32, 96, max = 255)),name = "", labels= c('No', 'Yes'))
      }
      else{
        ggplot(df, aes_string(x = x, y = y)) + 
          
          geom_vline(aes(xintercept  = 0)) +
          
          geom_errorbarh(aes_string(xmin = lCI,
                                    xmax = uCI,
                                    height =150),
                         color = rgb(0, 0, 0, max = 255), size = .2) +
          
          geom_point (color=rgb(0, 32, 96, max = 255)) +      
          theme_bw() +
          
          scale_x_continuous(limits = c(-40, 40)) + 
          
          scale_y_continuous(expand = c(0, 0), 
                             breaks=seq(0,9000,1000),
                             labels = function(x) format(x, big.mark = ",",
                                                         scientific = FALSE)) +
          
          theme(axis.line.x = element_line(color="black"), 
                axis.line.y = element_line(color="black"), 
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(linetype = "dashed", colour = "grey", size = .2),
                panel.grid.minor.y = element_blank(),
                panel.border = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 15),
                text = element_text(size = 15, 
                                    family = "Arial"),
                axis.text = element_text(size = 15)
          ) +
          
          labs(x = x_axis,
               y = y_axis,
               title = "Effect on the one-year proven reoffending rate again treatment size\n(all statistically significant results published to date)")
        
      }
      
    }
    
  })
}


