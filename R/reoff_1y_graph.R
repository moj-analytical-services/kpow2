# reoff_1y_graph
#
#' This function outputs the 'one-year proven reoffending rate' graph
#' @param data The standard result dataset
#' @param text_size Generic text size in graph
#' @param ci_text_size Text size of confidence intervals
#' @param y_title_size Text size of y-axis label
#' @param legend_text_size Text size of legend text
#' @param comp_CI_position Horizontal position of the comparison group CI label with respect to the x-axis
#' @param treat_CI_position Horizontal position of the treatment group CI label with respect to the x-axis
#' @export 


reoff_1y_graph = function(data, 
                          text_size = 11,
                          ci_text_size = 5,
                          y_title_size = 11,
                          legend_text_size = 12,
                          comp_CI_position = 0, 
                          treat_CI_position = 0) {

  img = png::readPNG("Icons/men graph.png")

  graphdata <- data[c(2, 8), c(1:3)]
  graphdata <- mutate(graphdata, 
                      indicator_1y_reoff_excpnds = indicator_1y_reoff_excpnds * 100,
                      freq_reoff_court_caution_1y = freq_reoff_court_caution_1y * 100) %>%
    
    rbind.data.frame("treat_nonreoff" = c(100 - graphdata$indicator_1y_reoff_excpnds[1] * 100, NA, NA)) %>%
    
    rbind.data.frame("comparison_nonreoff" = c(100 - graphdata$indicator_1y_reoff_excpnds[2] * 100, NA, NA)) %>%
    
    mutate(reoff_1y_CI = c(data[c(3, 9), c(1)] * 100, NA, NA)) %>%
    
    cbind.data.frame("reoff_freq_CI" = c(data[c(3, 9), c(2)] * 100, NA, NA)) %>%
    
    cbind.data.frame("time_CI" = c(data[c(3, 9), c(3)], NA, NA)) %>%
    
    mutate(group = c(
      "Participants analysed",
      "Comparison group",
      "Participants analysed",
      "Comparison group"
    )) %>%
    
    mutate(reoff_group = c(
      "At least one proven reoffence committed in a one-year period",
      "At least one proven reoffence committed in a one-year period",
      "No proven reoffences committed in a one-year period",
      "No proven reoffences committed in a one-year period"
    ))

  graphdata <- rbind.data.frame(graphdata, blank_bar1 = as.vector(cbind(graphdata[1,1:6],
                                                                        group = 'blank1',
                                                                        reoff_group = 'CI holder')))
  
  graphdata <- rbind.data.frame(graphdata, blank_bar2 = as.vector(cbind(graphdata[2,1:6],
                                                                        group = 'blank2',
                                                                        reoff_group = 'CI holder')))
  graphdata <- graphdata[c(5,1:4,6),]

  graphdata$reoff_group <- factor(as.character(graphdata$reoff_group),  levels=c("CI holder",
                                                                                 "No proven reoffences committed in a one-year period",
                                                                                 "At least one proven reoffence committed in a one-year period"))
  graphdata$group <- factor(as.character(graphdata$group),  levels=c("blank2",
                                                                     "Comparison group",
                                                                     "Participants analysed",
                                                                     "blank1")) ## This is in the reverse order from how you find it on the graph



  plot = ggplot2::ggplot(graphdata,aes(y = indicator_1y_reoff_excpnds,
                                       x = group,
                                       fill = reoff_group)) +

    scale_alpha_discrete(guide = FALSE) +
    
    geom_bar(stat="identity", 
             width = 0.5,
             aes(alpha = reoff_group)) +

    coord_flip() +

    labs(x = "",
         y="Number of proven reoffenders per 100 people")  +

    scale_fill_manual(values=c("#ffffff","#9ccbf2","#3f48cc","#ffffff"),
                      name="",
                      drop = FALSE,
                      labels= c(' ',
                                '   No proven reoffences committed\n   in a one-year period',
                                '   At least one proven reoffence committed          \n   in a one-year period',
                                ' '),
                      guide = guide_legend(reverse=TRUE)) +

    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10),
                       expand = c(0, 0)
    ) +

    scale_x_discrete(labels = c('', 
                                stringr::str_c('Comparison\ngroup\n(', 
                                               format_num(data['comparison_number', 'indicator_1y_reoff_excpnds']), 
                                               ')'),
                                
                                stringr::str_c('Participants\nanalysed\n(', 
                                               format_num(data['treat_number', 'indicator_1y_reoff_excpnds']),
                                               ')'),
                                '')) +

    theme(panel.border = element_blank(),
          panel.background = element_rect ("white"),
          panel.grid.major.x = element_line(color = "grey"),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect ("white"),
          text = element_text(size = text_size, family = "Arial"),
          axis.line = element_line(color = "grey", linetype = "solid"),
          legend.position="top",
          legend.key.height = unit(1,"cm"),
          legend.key.width = unit(1,"cm"),
          legend.text = element_text(size = legend_text_size),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(family = "Arial",
                                     size = y_title_size),
          plot.margin = unit(c(1, 3.5, 0.5, 0), "lines"),
          aspect.ratio = 288/1360)   +

    annotation_raster(img, xmin=2.75, xmax=3.25, ymin=0, ymax=100) +

    annotation_raster(img, xmin=1.75, xmax=2.25, ymin=0, ymax=100) +

    geom_errorbar(data=subset(graphdata, reoff_group=="CI holder"),
                  aes(ymin = indicator_1y_reoff_excpnds - reoff_1y_CI,
                      ymax = indicator_1y_reoff_excpnds + reoff_1y_CI),
                  width=0.2, alpha = 0.7,
                  color = "#3f48cc") +

    annotate("text",
             label = paste0("Confidence interval: ±", format(graphdata$reoff_1y_CI[3],digits=1,nsmall=1)," people"),
             x = 1,
             y = graphdata$indicator_1y_reoff_excpnds[3] + comp_CI_position,
             size = ci_text_size,
             color = "#3f48cc",
             family = "Arial") +

    annotate("text",
             label = paste0("Confidence interval: ±", format(graphdata$reoff_1y_CI[2],digits=1,nsmall=1)," people"),
             x = 4,
             y = graphdata$indicator_1y_reoff_excpnds[2] + treat_CI_position,
             size = ci_text_size,
             color = "#3f48cc",
             family = "Arial"
             ) 
  
  plot
  return(plot)

}

