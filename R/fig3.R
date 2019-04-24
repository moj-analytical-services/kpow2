#' Returns Figure 3 (Receipt, Disposals, Outstanding by offence group) in the Criminal Court Quarterly Bulletin 
#' @param data CREST RDOS dataset
#' @export

fig3 <- function(data){

ggplot(data, aes(x = reorder(offence_group_desc, value),  y = value)) +
  geom_bar(stat = "identity", aes(fill = rdos_type), position = "dodge" ) + # Add barchart
  coord_flip() + # Make barchart horizontal
  labs(colour = "") + # Remove legend header
    scale_fill_manual(values = c("#00ccff", "#0000cc", "#0066ff"), # Set line colours
      labels = c("Disposals", "Outstanding cases at \nend of quarter", "Receipts")) +
  ylab("Number of cases") +
  xlab("") +
  scale_y_continuous(minor_breaks = seq(0, 20000, 1000),
                     breaks = seq(0, 20000, 1000)) + # Format y-axis scale and numbers
  #nexpand_limits(y = 0) + # Set y-axis minimum to 0
  theme_bw() + # Change theme to get rid of grey background
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        text = element_text(size = 12, family = "Arial"),
        axis.line = element_line(color = "black", linetype = "solid"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.3),
        legend.position = c(0.8, 0.2),
        legend.title=element_blank())
}
