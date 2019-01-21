# time_graph
#
#' This function outputs the 'time before first proven reoffence' graph
#' @param data The standard result dataset
#' @param text_size Generic text size in graph
#' @param ci_text_size Text size of confidence intervals
#' @param y_title_size Text size of y-axis label
#' @param legend_text_size Text size of legend text
#' @param comp_CI_position Horizontal position of the comparison group CI label with respect to the x-axis
#' @param reoff_CI_position Horizontal position of the treatment group CI label with respect to the x-axis
#' @export


time_graph = function(data,
                      text_size,
                      ci_text_size = 5,
                      y_title_size = 11,
                      legend_text_size = 12, 
                      comp_CI_position = 0, 
                      treat_CI_position = 0) {
  
  img = png::readPNG("Icons/calendar graph.png")
  
  # Preparing the data to use ggplot on
  graphdata <- data[c(2, 8), c(1:3)]
  graphdata <- mutate(graphdata, 
                      indicator_1y_reoff_excpnds = indicator_1y_reoff_excpnds * 100,
                      freq_reoff_court_caution_1y = freq_reoff_court_caution_1y * 100) %>%
    rbind.data.frame('treat_nonreoff'=c(NA,
                                        NA,
                                        365 - graphdata$days_to_reoff_1y[1])) %>%
    rbind.data.frame('comparison_nonreoff'=c(NA,
                                          NA,
                                          365 - graphdata$days_to_reoff_1y[2])) %>%
    cbind.data.frame('reoff_1y_CI' = c(data[c("treat_ci",
                                              "comparison_ci"), 
                                            "indicator_1y_reoff_excpnds"] * 100,
                                     NA,
                                     NA)) %>%
    cbind.data.frame('reoff_freq_CI' = c(data[c("treat_ci",
                                                "comparison_ci"), 
                                              "freq_reoff_court_caution_1y"] * 100,
                                         NA,
                                         NA)) %>%
    cbind.data.frame('time_CI' = c(data[c("treat_ci",
                                        "comparison_ci"), 
                                      "days_to_reoff_1y"],
                                 NA,
                                 NA)) %>%
    mutate(group = c("Participants analysed",
                     "Comparison group",
                     "Participants analysed",
                     "Comparison group")) %>%
    mutate(reoff_group = c('Time before first proven reoffence committed in a one-year period',
                           'Time before first proven reoffence committed in a one-year period',
                           'Time after first proven reoffence committed in a one-year period',
                           'Time after first proven reoffence committed in a one-year period'))
  
  graphdata <- rbind(graphdata, blank_bar1 = as.vector(cbind(graphdata[1,1:6],
                                                             group = 'blank1',
                                                             reoff_group = 'CI holder'))) %>%
    
    rbind(blank_bar2 = as.vector(cbind(graphdata[2,1:6],
                                       group = 'blank2',
                                       reoff_group = 'CI holder')))
  
  graphdata <- graphdata[c(5,1:4,6),]
  graphdata$reoff_group <- factor(as.character(graphdata$reoff_group),  
                                  levels=c("CI holder",
                                           "Time after first proven reoffence committed in a one-year period",
                                           "Time before first proven reoffence committed in a one-year period"))
  graphdata$group <- factor(as.character(graphdata$group),  levels=c("blank2",
                                                                     "Comparison group",
                                                                     "Participants analysed",
                                                                     "blank1"))
  
  
  plot = ggplot2::ggplot(graphdata,
                         aes(y = days_to_reoff_1y,
                             x = group,
                             fill = reoff_group)) +
    
    scale_alpha_discrete(guide = FALSE) +
    
    geom_bar(stat="identity", 
             width = 0.6,
             aes(alpha = reoff_group)) +
    
    coord_flip()  +
    
    labs(x = "",
         y="Average number of days to first proven reoffence (reoffenders only)")  +
    
    scale_fill_manual(values=c("#ffffff","#3f48cc","#9ccbf2","#ffffff"), 
                      name="",
                      labels= c(' ',
                                'Time after first proven reoffence\ncommitted in a one-year period',
                                'Time before first proven reoffence\ncommitted in a one-year period',
                                ' '),
                      guide = guide_legend(reverse=TRUE)) +
    

    
    scale_x_discrete(labels = c('', 
                                stringr::str_c('Comparison\ngroup\n(', 
                                               format_num(data['comparison_number', 'days_to_reoff_1y']), 
                                               ')'),
                                
                                stringr::str_c('Participants\nanalysed\n(', 
                                               format_num(data['treat_number', 'days_to_reoff_1y']),
                                               ')'),
                                '')) +
    
    scale_y_continuous(limits=c(0, 365),
                       breaks=seq(0,365,60),
                       expand = c(0, 0)) +
    
    theme(panel.border = element_blank(),
          plot.background = element_rect ("white"),
          plot.margin = unit(c(1, 3.5, 0.5, 0), "lines"),
          panel.background = element_rect ("white"),
          panel.grid.major.x = element_line(color = "grey"),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = text_size, family = "Arial"),
          legend.text = element_text(size = legend_text_size),
          legend.position="top",
          legend.key.height = unit(1,"cm"),
          legend.key.width = unit(1,"cm"),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(family = "Arial",
                                     size = y_title_size),
          axis.line = element_line(color = "grey", linetype = "solid"),
          axis.ticks.y = element_blank(),
          aspect.ratio = 288/1360) +
    
    annotation_raster(img, xmin=2.7, xmax=3.3, ymin=0, ymax=366) +
    
    annotation_raster(img, xmin=1.7, xmax=2.3, ymin=0, ymax=366) +
    
    geom_errorbar(data=subset(graphdata, reoff_group=="CI holder"),
                  aes(ymin = days_to_reoff_1y - time_CI,
                      ymax = days_to_reoff_1y + time_CI),
                  width=0.2,
                  color = "#3f48cc")  +
    
    annotate("text",
             label = paste0("Confidence interval: ±", format(graphdata$time_CI[3],digits=1,nsmall=1)," days"),
             x = 1,
             y = graphdata$days_to_reoff_1y[3] + comp_CI_position,
             size = ci_text_size,
             color = "#3f48cc",
             family = "Arial") + 
    
    annotate("text",
             label = paste0("Confidence interval: ±", format(graphdata$time_CI[2],digits=1,nsmall=1)," days"),
             x = 4,
             y = graphdata$days_to_reoff_1y[2] + treat_CI_position,
             size = ci_text_size,
             color = "#3f48cc",
             family = "Arial") 
  
  plot
  return (plot)
}
