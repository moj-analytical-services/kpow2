## Code to create the chevrom diagram for the JDL report

# Reads in the relevant sheet from the Excel document
chevron_df <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                   sheet = "Chevron", rows = c(1, 3:100))
)



# Function to correctly format the percentages
perc_format <- function(number) {
  
  ifelse(is.na(number), 
         NA,
         if (number == 0) {
           "0%"
         } else if (round(number * 100) == 0) {
           "<1%"
         } else {
           paste(round(number * 100), "%", sep = "")
         })
  
}

# Function to keep NAs as NAs or to format the number
ifelse_format <- function(number) {
  ifelse(is.na(number), 
         NA,
         JDLpackage:::format_num(number))
}

#Creates colums that format the total number in each stage, the dropped number, creates the percentage and formats the percentage
chevron_df %<>%
  mutate(total_format   = sapply(total, ifelse_format),
         
         dropped_format = sapply(dropped_out, ifelse_format),
         
         perc           = ifelse(is.na(dropped_out), 
                                 NA,
                                 dropped_out/total[1]),
         
         perc_format    = sapply(perc, perc_format))

# highlights the rows which have people dropping out of the stage
chevron_iter <- chevron_df %>%
  filter(is.na(dropped_out) == FALSE)

# keeps rows of those with a final group number
chevron_groups <- chevron_df %>%
  filter(is.na(final_group_number) == FALSE)

chevron_html <- str_c('<div class="chevron">',
                      '<div class="chev_text">', 
                      chevron_df$total_format[1], ' ', JDLpackage:::people_func(chevron_iter$total[1], gender), ' ', chevron_df$description[1], 
                      '</div>',
                      
                      '<div class="chev_img"><img src="', icon_file_folder, 'Chevron.png">',
                      '<div class="chev_img_text">',
                      chevron_df$total_format[1], 
                      '</div>',
                      '</div>')

for (i in 1:(nrow(chevron_iter) - 1)){
  chevron_html %<>% str_c('<div class="chev_text">', 
                          chevron_iter$dropped_format[i], ' ', JDLpackage:::people_func(chevron_iter$total[i], gender),
                          ' (', chevron_iter$perc_format[i], ') ',
                          chevron_iter$description[i],
                          '</div>',
                          
                          '<div class="chev_img"><img src="', icon_file_folder, 'Chevron.png">',
                          '<div class="chev_img_text">', 
                          chevron_iter$total_format[i],
                          '</div>',
                          '</div>')
}

chevron_html %<>% str_c('<div class="chev_text">', 
                        chevron_iter$dropped_format[nrow(chevron_iter)], ' ', JDLpackage:::people_func(chevron_iter$total[nrow(chevron_iter)], gender),
                        ' (', chevron_iter$perc_format[nrow(chevron_iter)], ') ',
                        chevron_iter$description[nrow(chevron_iter)],
                        '</div>',
                        
                        '<div class="chev_end_wrap">')

if (nrow(chevron_groups) == 0) {
  stop("There are no analyses indicated by an X in the 'Chevron' tab of the spreadsheet - you need an X beside each analysis that you want to appear in the chevrons")
} else if(nrow(chevron_groups) == 1) {
  
  chevron_html %<>% str_c('<div class="chev_end">',
                          '<div class="chev_img">',
                          '<img src="', icon_file_folder, 'Chevron.png">',
                          '<div class="chev_img_text">',
                          chevron_groups$total_format[1], 
                          '</div>',
                          '</div>',
                          '</div>',
                          
                          '<div class="chev_text">',
                          '<b>', report_info$analyses_name[2], ' treatment group:</b> ', perc_format(chevron_iter$total[nrow(chevron_iter)]/chevron_df$total[1]), ' of the participants submitted<br>',
                          '(Comparison group: ', JDLpackage::format_num(t_test_data[[1]]["comparison_number", "indicator_1y_reoff_excpnds"]), ' records)',
                          '</div>')
} else {
  
  chevron_html %<>% str_c('<div class="chev_img">',
                          '<img src="', icon_file_folder, 'Chevron.png">',
                          '<div class="chev_img_text">',
                          chevron_groups$total_format[1], '<br>',
                          '</div></div>',
                          
                          
                          '<div class="chev_text">',
                          '<b>', report_info$analyses_name[2], ' treatment group:</b> ', perc_format(chevron_iter$total[nrow(chevron_iter)]/chevron_df$total[1]), ' of the participants submitted<br>',
                          '(Comparison group: ', JDLpackage::format_num(t_test_data[[1]]["comparison_number", "indicator_1y_reoff_excpnds"]), ' records)',
                          '</div>')
  
  for (i in 2:nrow(chevron_groups)){
    chevron_html %<>% str_c('<div class="chev_end">',
                            '<div class="chev_img">',
                            '<img src="', icon_file_folder, 'Chevron.png">',
                            '<div class="chev_img_text">',
                            chevron_groups$total_format[i], '<br>',
                            '<div class="chev_group_text">',
                            '<b>', report_info$analyses_name[i+1], ' treatment group</b><br>',
                            '(Comparison group: ', JDLpackage::format_num(t_test_data[[i]]["comparison_number", "indicator_1y_reoff_excpnds"]), ' records)',
                            '</div>',
                            '</div>',
                            '</div>',
                            '</div>',
                            if (!i%%3 && i!=nrow(chevron_groups)) '<p style="margin-bottom: 75px"></p>')
  }
}




chevron_html %<>% str_c('</div></div>')
