#' Returns Figure 4 (Timeliness) in the Criminal Court Quarterly Bulletin 
#' @param data Simplified CREST flatfile dataset
#' @export

fig4 <- function(data){
  ggplot(data = data, aes(x = period, y = value, order = variable)) + # Select the variables
    geom_bar(stat = "identity", aes(fill = variable), position = "stack" ) + # Makes it a bar chart
    labs(colour = "") +
    guides(fill=guide_legend(title=NULL)) + # Remove legend header
    scale_fill_manual(values = c("#00ccff", "#0000cc"))+ #, "#0066ff")) + # Set line colours
    ylab("Average number of days(mean)") +
    xlab("") +
    scale_y_continuous(minor_breaks = seq(0, 250, 50),
                       breaks = seq(0, 250, 50), labels = comma) + # Format y-axis scale and numbers
    expand_limits(y = 0) + # Set y-axis minimum to 0
    theme_bw() + # Change theme to get rid of grey background
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
          panel.grid.major.x = element_blank(), text = element_text(size = 12, family = "Arial"),
          axis.line = element_line(color = "black", linetype = "solid"))
}