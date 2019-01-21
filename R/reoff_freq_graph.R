# reoff_freq_graph
#
#' This function outputs the 'one-year proven reoffending rate' graph
#' @param data The standard result dataset
#' @param text_size Generic text size in graph
#' @param ci_text_size Text size of confidence intervals
#' @param y_title_size Text size of y-axis label
#' @param legend_text_size Text size of legend text
#' @param comp_CI_x_position Horizontal position of the comparison group CI label with respect to the x-axis
#' @param reoff_CI_x_position Horizontal position of the treatment group CI label with respect to the x-axis
#' @param comp_CI_y_position Vertical position of the comparison group CI label with a unit being the space between the center of the bars
#' @param reoff_CI_y_position Vertical position of the treatment group CI label with a unit being the space between the center of the bars
#' @param bar_width The width of the bars
#' @export 


reoff_freq_graph = function(data, 
                            max_x_val,
                            x_ticks,
                            text_size = 11,
                            ci_text_size = 5,
                            y_title_size = 11,
                            legend_text_size = 12, 
                            comp_CI_x_position = 0, 
                            treat_CI_x_position = 0,
                            comp_CI_y_position = 0, 
                            treat_CI_y_position = 0,
                            bar_width = 0.9) {


  img = png::readPNG("Icons/scale graph.png")

  graphdata <- data[c("treat_mean", 
                      "comparison_mean"),
                    c("indicator_1y_reoff_excpnds", 
                      "freq_reoff_court_caution_1y", 
                      "days_to_reoff_1y")]
  
  graphdata <- mutate(graphdata,
                      indicator_1y_reoff_excpnds = indicator_1y_reoff_excpnds * 100,
                      freq_reoff_court_caution_1y = freq_reoff_court_caution_1y *100) %>%    
    rbind.data.frame('treat_nonreoff' = c(100 - graphdata$indicator_1y_reoff_excpnds[1], 
                                          NA, 
                                          NA)) %>%
    
    rbind.data.frame('comparison_nonreoff' = c(100 - graphdata$indicator_1y_reoff_excpnds[2], 
                                            NA, 
                                            NA)) %>%
    
    cbind.data.frame('reoff_1y_CI' = c(data[c('treat_ci',
                                              'comparison_ci'),
                                            "indicator_1y_reoff_excpnds"] * 100,
                                       NA,
                                       NA)) %>%
    
    cbind.data.frame( 'reoff_freq_CI' = c(data[c('treat_ci',
                                                 'comparison_ci'),
                                               "freq_reoff_court_caution_1y"] * 100,
                                          NA,
                                          NA)) %>%
    
    cbind.data.frame('time_CI' = c(data[c('treat_ci',
                                          'comparison_ci'),
                                        "days_to_reoff_1y"],
                                   NA,
                                   NA))
  
  graphdata[ , 'group']<-c("Participants analysed",
                           "Comparison group",
                           "Participants analysed",
                           "Comparison group")
  
  graphdata[ , 'reoff_group']<- c('Proven reoffences committed in a one-year period',
                                  'Proven reoffences committed in a one-year period',
                                  ' ',
                                  ' ')

  graphdata <- rbind.data.frame(graphdata, blank_bar1 = as.vector(cbind(graphdata[1, 1:6],
                                                                        group = 'blank1',
                                                                        reoff_group = 'CI holder')))

  graphdata <- rbind.data.frame(graphdata, blank_bar2 = as.vector(cbind(graphdata[2, 1:6],
                                                                        group = 'blank2',
                                                                        reoff_group = 'CI holder')))

  graphdata <- graphdata[c(5, 1:4, 6),]
  graphdata$reoff_group <- factor(as.character(graphdata$reoff_group),  levels=c("CI holder",
                                                                                 " ",
                                                                                 "Proven reoffences committed in a one-year period"))
  graphdata$group <- factor(as.character(graphdata$group),  levels=c("blank2",
                                                                     "Comparison group",
                                                                     "Participants analysed",
                                                                     "blank1")) ## This is in the reverse order from how you find it on the graph
  
  testdata = subset(graphdata, reoff_group == "Proven reoffences committed in a one-year period" | reoff_group == "CI holder" )

  if (max(testdata$freq_reoff_court_caution_1y + testdata$reoff_1y_CI) > max_x_val) {
    stop("The maximum x value is too small to fit the graph onto, please make it bigger")
  }


  plot = ggplot2::ggplot(subset(graphdata, reoff_group == "Proven reoffences committed in a one-year period" | reoff_group == "CI holder" ),
                         aes(y = freq_reoff_court_caution_1y,
                             x = group,
                             fill = reoff_group)) +

    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max_x_val),
                       breaks = seq(0, max_x_val, x_ticks)) +

    scale_x_discrete(labels = c('', 
                                stringr::str_c('Comparison\ngroup\n(', 
                                               format_num(data['comparison_number', 'indicator_1y_reoff_excpnds']), 
                                               ')'),
                                
                                stringr::str_c('Participants\nanalysed\n(', 
                                               format_num(data['treat_number', 'indicator_1y_reoff_excpnds']),
                                               ')'),
                                '')) +

    scale_alpha_discrete(guide = FALSE) +
    
    geom_bar(stat="identity", 
             width = bar_width,
             aes(alpha = reoff_group)) +

    coord_flip()  +

    labs(x = "",
         y="Number of proven reoffences per 100 people")  +

    scale_fill_manual(breaks="Proven reoffences committed in a one-year period",
                      values=c("#ffffff","#9ccbff","#3f48cc","#ffffff"), name="",
                      drop = FALSE) +

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
          aspect.ratio = 288/1360)


  # plot

  y_values = ggplot_build(plot)$data[[1]]$y

  picture_width = 1.01
  ymin = 0
  ymax =  picture_width * 10

  rounded_y = ceiling(y_values/picture_width)*picture_width

  
  for (i in 1:(ceiling(rounded_y[2]/(picture_width * 10)))){
    plot = plot + annotation_raster(img,
                                    xmin = 2 - bar_width / 2,
                                    xmax = 2 + bar_width / 2,
                                    ymin = ymin + (picture_width*(10 - 0.01))*(i - 1),
                                    ymax = ymax + (picture_width*(10 - 0.01))*(i - 1)
    )
  }
  
  for (i in 1:(ceiling(rounded_y[3]/(picture_width * 10)))){
    plot = plot + annotation_raster(img,
                                    xmin = 3 - bar_width / 2,
                                    xmax = 3 + bar_width / 2,
                                    ymin = ymin + (picture_width*(10 - 0.01))*(i - 1),
                                    ymax = ymax + (picture_width*(10 - 0.01))*(i - 1)
    )
  }
  

  plot = plot + geom_errorbar(data=subset(graphdata, reoff_group == "CI holder"),
                              aes(ymin = freq_reoff_court_caution_1y - reoff_freq_CI,
                                  ymax = freq_reoff_court_caution_1y + reoff_freq_CI),
                              width = 0.2, alpha = 0.7,
                              color = "#3f48cc") +

      annotate("text",
               label = paste0("Confidence interval: ±", format(graphdata$reoff_freq_CI[3],digits=1,nsmall=1)," offences"),
               x = 1 + comp_CI_y_position,
               y = graphdata$freq_reoff_court_caution_1y[3] + comp_CI_x_position,
               size = ci_text_size,
               color = "#3f48cc",
               family = "Arial") +

      annotate("text",
               label = paste0("Confidence interval: ±", format(graphdata$reoff_freq_CI[2],digits=1,nsmall=1)," offences"),
               x = 4 + treat_CI_y_position,
               y = graphdata$freq_reoff_court_caution_1y[2] + treat_CI_x_position,
               size = ci_text_size,
               color = "#3f48cc",
               family = "Arial") 


  plot
  return (plot)
}
