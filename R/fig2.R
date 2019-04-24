#' Returns Figure 2 (Total Receipts, Disposals Outstanding) in the Criminal Court Quarterly Bulletin 
#' @param data CREST RDOS dataset
#' @export

fig2 <- function(data){
  ggplot(data = data, aes(x = period, y = value, group = variable, colour = variable)) + # Select the variables
  geom_line() + # Makes it a line chart
  labs(colour = "") + # Remove legend header
  scale_color_manual(values = c("#00ccff", "#0000cc", "#0066ff")) + # Set line colours
  ylab("Number of cases") +
  xlab("") +
  scale_y_continuous(minor_breaks = seq(0, 60000, 10000),
                     breaks = seq(0, 60000, 10000), labels = comma) + # Format y-axis scale and numbers
  expand_limits(y = 0) + # Set y-axis minimum to 0
  theme_bw() + # Change theme to get rid of grey background
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        panel.grid.major.x = element_blank(), text = element_text(size = 12, family = "Arial"),
        axis.line = element_line(color = "black", linetype = "solid"))
}
