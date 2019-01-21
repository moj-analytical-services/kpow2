## A file that creates the table in the JDL report using HTML script.
## The tables produced are the overall measurements table, the overall estimates table,  the 3 tables showing the 3 headline measures and the Table
##  of contents

#####Tables####

#################### Measurement table ####################

meas_table <- stringr::str_c("<table class='measurements_table' style='width:100%'>",
                             "<tr>", ###First row
                             "<th colspan = '3'>Overall measurements of the treatment and comparison groups</th>",
                             "</tr>",
                             
                             "<tr>", ###Second row
                             "<th>",
                             "</th>",
                             "<th class='th2'>",
                             "For <b>100</b> typical ",
                             JDLpackage:::people_func(2,gender),
                             " in the <b>treatment</b> group, the equivalent of:",
                             "</th>",
                             "<th class='th2'>",
                             "For <b>100</b> typical ",
                             JDLpackage:::people_func(2,gender),
                             " in the <b>comparison</b> group, the equivalent of:</th>",
                             "</tr>",
                             
                             "<tr>", ###Third row
                             "<td>", #Cell that contains the icons for reoffending rate
                             "<img src='", icon_file_folder, "rateofreoff.png' style = 'width: 5mm;'> <p style='margin-top:00px'></p>",
                             JDLpackage:::icon(reoff_rate_pvalue[headline_analysis],
                                               diff_reoff_rate[headline_analysis],
                                               'reoff_rate'),
                             "</td>",
                             
                             "<td>", #Cell that describes the difference in the reoffending rate between the comparison and treatment group
                             "<div class = 'b_blue'>", 
                             treat_reoff_rate_format[headline_analysis], 
                             "</div>",
                             " of the 100 ",  
                             JDLpackage:::people_func(treat_reoff_rate[headline_analysis],
                                                      gender), 
                             " committed a proven reoffence within a one-year period (a rate of ",
                             treat_reoff_rate_format[headline_analysis],
                             "%), ", 
                             
                             "<div class = 'b_blue'>",
                             if (diff_reoff_rate_format[headline_analysis] != 0)
                               paste0(diff_reoff_rate_format[headline_analysis], 
                               " ", JDLpackage:::people_func(diff_reoff_rate_format[headline_analysis], 
                                                             gender), 
                               " " , JDLpackage:::more_or_fewer(treat_reoff_rate[headline_analysis], 
                                                                 comp_reoff_rate[headline_analysis]),
                               "</div>",
                               " than in the comparison group.")
                             else
                               "similar to the comparison group.",
                             "</td>",
                             
                             "<td>", #Cell that describes the conparison reoffending rate
                             "<div class = 'b_blue'>", 
                             comp_reoff_rate_format[headline_analysis], 
                             "</div>", 
                             " of the 100 ", 
                             JDLpackage:::people_func(comp_reoff_rate[headline_analysis], 
                                                      gender), 
                             " committed a proven reoffence within a one-year period (a rate of ", 
                             comp_reoff_rate_format[headline_analysis],
                             "%).",
                             "</td>",
                             "</tr>",
                             
                             "<tr>", ###Fourth row
                             "<td>", #Cell that contains the icons for reoffending frequency
                             "<img src='", icon_file_folder, "freqofreoff.png' style = 'width: 7mm;'> <p style='margin-top:00px'></p>", 
                             JDLpackage:::icon(reoff_freq_pvalue[headline_analysis], 
                                               diff_reoff_freq[headline_analysis],
                                               'reoff_freq'),
                             "</td>",
                             
                             "<td>", #Cell that describes the difference in the reoffending frequency between the comparison and treatment group
                             "<div class = 'b_blue'>",
                             treat_reoff_freq_format[headline_analysis], 
                             "</div>",
                             " proven reoffences were committed by these 100 ", 
                             JDLpackage:::people_func(100,gender), 
                             " during the year (a frequency of ", 
                             JDLpackage:::format_num(treat_reoff_freq[headline_analysis]/100,
                                                     digits = 1), 
                             " ", 
                             JDLpackage:::offence_func(treat_reoff_freq[headline_analysis]), 
                             " per person), ", 
                             "<div class = 'b_blue'>",
                             diff_reoff_freq_format[headline_analysis], 
                             " ", 
                             JDLpackage:::offence_func(diff_reoff_freq_format[headline_analysis]), 
                             " ", 
                             JDLpackage:::more_or_fewer(treat_reoff_freq[headline_analysis], 
                                                        comp_reoff_freq[headline_analysis]), 
                             "</div>", 
                             " than in the comparison group.",
                             "</td>",
                             
                             "<td>", #Cell that describes the comparison reoffending frequency
                             "<div class = 'b_blue'>",
                             comp_reoff_freq_format[headline_analysis], 
                             "</div>", 
                             " proven reoffences were committed by these 100 ", 
                             JDLpackage:::people_func(100,gender), 
                             " during the year (a frequency of ", 
                             JDLpackage:::format_num(comp_reoff_freq[headline_analysis]/100,
                                                     digits = 1), 
                             " ", 
                             JDLpackage:::offence_func(comp_reoff_freq[headline_analysis]),
                             " per person).",
                             "</td>",
                             "</tr>")

if (treat_time_numb >= 30){
  meas_table %<>% stringr::str_c("<tr>", ###Fifth row
                                 "<td>", #Cell that contains the icons for time to reoffence
                                 "<img src='", icon_file_folder, "timetoreoff.png' style = 'width: 7mm;'> <p style='margin-top:00px'></p>",
                                 JDLpackage:::icon(time_pvalue[headline_analysis], 
                                                   diff_time[headline_analysis],
                                                   'time'),
                                 "</td>",
                                 "<td>", #Cell that describes the difference in the time to reoffence between the comparison and treatment group
                                 "<div class = 'b_blue'>",
                                 treat_time_format[headline_analysis], 
                                 "</div>", 
                                 JDLpackage:::day_func(treat_time[headline_analysis]), 
                                 " was the average time before a reoffender committed their first proven reoffence, ", 
                                 "<div class = 'b_blue'>",
                                 diff_time_format[headline_analysis], 
                                 " ", 
                                 JDLpackage:::day_func(diff_time[headline_analysis]), 
                                 " ", 
                                 JDLpackage:::earlier_or_later(treat_time[headline_analysis], 
                                                               comp_time[headline_analysis]), 
                                 "</div> than the comparison group.",
                                 "</td>",
                                 
                                 "<td>", #Cell that describes the difference in the time to reoffence in comparison group
                                 "<div class = 'b_blue'>",
                                 comp_time_format[headline_analysis], 
                                 "</div>",
                                 JDLpackage:::day_func(comp_time[headline_analysis]), 
                                 " was the average time before a reoffender committed their first proven reoffence.",
                                 "</td>",
                                 "</tr>")
} else {
  meas_table %<>% stringr::str_c("</td>",
                                 "<td colspan = '2'>", #Cell that describes the difference in the time to reoffence between the comparison and treatment group
                                 "Time to first reoffence has not been included as a headline result due to low numbers of reoffenders, which could give misleading results.",
                                 "</td>",
                                 "</tr>")
}

meas_table %<>% stringr::str_c("</table>")

############################ Estimate table ####################

est_table <- stringr::str_c("<table class='estimates_table' style='width:100%'>",
                            "<tr>", ### First line
                            "<th colspan = '2'>", # Table title
                            "Overall estimates of the impact of the intervention",
                            "</th>",
                            "</tr>",
                            
                            "<tr>", ### Second line
                            "<td>",
                            "</td>",
                            
                            "<td>", # Description of what the table shows
                            "For <b>100</b> typical ",
                            JDLpackage:::people_func(2,gender),
                            " who receive support, compared with <b>100</b> similar ",
                            JDLpackage:::people_func(2,gender),  
                            " who do not receive it:",
                            "</td>",
                            "</tr>",
                            
                            "<tr>", ### Third line
                            "<td>", # man icon
                            "<img src='", icon_file_folder, "rateofreoff.png' style = 'width: 5mm;'>",
                            "</td>",
                            
                            "<td>",
                            "The number of ",
                            JDLpackage:::people_func(2,gender),
                            " who commit a proven reoffence within one year after release could be",
                            JDLpackage:::higher_or_lower_CI(reoff_rate_CI_l[headline_analysis],
                                                            reoff_rate_CI_u[headline_analysis], 
                                                            gender_plural, 
                                                            gender),
                            if (reoff_rate_pvalue[headline_analysis] > 0.05 && is.finite(t_test_data[[headline_analysis]]['number_needed', 'indicator_1y_reoff_excpnds'])) paste(" Based on the current measurements, it is estimated that", JDLpackage:::format_num(t_test_data[[headline_analysis]]['number_needed', 'indicator_1y_reoff_excpnds']), gender_plural, "would be needed."),
                            "</td>",
                            "</tr>",
                            
                            "<tr>", ### Fourth line
                            "<td>",
                            "<img src='", icon_file_folder, "freqofreoff.png' style = 'width: 7mm;'>",
                            "</td>",
                            
                            "<td>",
                            "The number of proven reoffences committed during the year could be",
                            JDLpackage:::higher_or_lower_CI(reoff_freq_CI_l[headline_analysis], 
                                                            reoff_freq_CI_u[headline_analysis], 
                                                            'offences',
                                                            gender),
                            "</td>",
                            "</tr>",
                            
                            "<tr>", ### Fifth line
                            "<td>",
                            "<img src='", icon_file_folder, "timetoreoff.png' style = 'width: 7mm;'>",
                            "</td>")


if( treat_time_numb >= 30){
  est_table %<>% stringr::str_c("<td>",
                                "On average, the time before an offender committed their first proven reoffence could be",
                                JDLpackage:::shorter_or_longer_CI(time_CI_l[headline_analysis], 
                                                                  time_CI_u[headline_analysis], 
                                                                  'days',
                                                                  gender),
                                "</td>",
                                "</tr>")
}

est_table %<>% stringr::str_c("</table>")

############# Analysis overview ###############



analysis_overview_table <- stringr::str_c("<table class='analyses_overview' style='width:100%'>",
                                          "<tr>",
                                          "<th>Analyses</th>",
                                          "</tr>",
                                          
                                          "<tr>",
                                          "<td>",
                                          "<ol>")

for (i in 1:length(analyses_name)){
  
  if(tolower(analyses_region[i]) == "national"){
    
    joiner = "across England and Wales"
  } else {
    
    joiner = stringr::str_c("in ",  analyses_region[i])
  }
  
  analysis_overview_table <- stringr::str_c(analysis_overview_table,
                                            "<li>",
                                            "<b>", analyses_name[i], " analysis: </b>",
                                            "treament group matched to offenders ",
                                            joiner,
                                            " using demographics, criminal history and individual risks and needs.",
                                            "</li>"
  )
}

analysis_overview_table <- stringr::str_c(analysis_overview_table,
                                          "</ol>",
                                          "</td>",
                                          "</tr>",
                                          "</table>")



############# Analysis detail ###############

if (num_of_analyses > 1){
  
  analysis_detail_table <- stringr::str_c("<table class='analyses_detail' style='width:100%'>",
                                          "<tr>",
                                          "<th>Analyses</th>",
                                          "<th>Controlled for Region</th>",
                                          "<th>Treatment Group Size</th>",
                                          "<th>Comparison Group Size</th>",
                                          "<th>Reoffenders in treatment group</th>",
                                          "<th>Reoffenders in comparison group</th>",
                                          "</tr>"
  )
  
  
  for (i in 1:length(analyses_name)){
    analysis_detail_table <- stringr::str_c(analysis_detail_table,
                                            "<tr>",
                                            "<td class='analyses_detail'>", analyses_name[i], "</td>",
                                            "<td class='analyses_detail'>", ifelse(is.na(controlled_region[i]), " ", "X"), "</td>",
                                            "<td class='analyses_detail'>", treat_reoff_rate_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", comp_reoff_rate_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", treat_time_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", comp_time_numb_format[i], "</td>",
                                            "</tr>"
    )
  }
  analysis_detail_table <- stringr::str_interp(c(analysis_detail_table, "</table>"))
  
} else{
  
  analysis_detail_table <- stringr::str_c("<table class='analyses_detail' style='width:100%'>",
                                          "<tr>",
                                          "<th>Treatment Group Size</th>",
                                          "<th>Comparison Group Size</th>",
                                          "<th>Reoffenders in treatment group</th>",
                                          "<th>Reoffenders in comparison group</th>",
                                          "</tr>"
  )
  
  
  for (i in 1:length(analyses_name)){
    analysis_detail_table <- stringr::str_c(analysis_detail_table,
                                            "<tr class = 'analyses_detail_1'>",
                                            "<td class='analyses_detail'>", treat_reoff_rate_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", comp_reoff_rate_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", treat_time_numb_format[i], "</td>",
                                            "<td class='analyses_detail'>", comp_time_numb_format[i], "</td>",
                                            "</tr>"
    )
  }
  analysis_detail_table <- stringr::str_interp(c(analysis_detail_table, "</table>"))
  
  
}




#################### Reoffending rate table ####################

if(num_of_analyses != 1){
  reoff_rate_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                     "<caption>Table 1: Proportion of ", JDLpackage:::people_func(2,gender), " who committed a proven reoffence in a one-year period after support from ", organisation_name,", compared with matched comparison groups</caption>",
                                     "<tr>",
                                     "<th rowspan='2'>Analysis</th>",
                                     "<th rowspan='2'>Number in treatment<br>group</th>",
                                     "<th rowspan='2'>Number in comparison<br>group</th>",
                                     "<th colspan='5'>One-year proven reoffending rate</th>",
                                     "</tr>",
                                     "<tr>",
                                     "<th>Treatment group rate<br>(&#37;)</th>",
                                     "<th>Comparison group rate<br>(&#37;)</th>",
                                     "<th>Estimated difference <br>(&#37; points)</th>",
                                     "<th>Significant difference?	</th>",
                                     "<th>p-value</th>",
                                     "</tr>")
  
  for (i in 1:length(analyses_name)){
    reoff_rate_table <-  stringr::str_c(reoff_rate_table, "<tr>",
                                        JDLpackage:::sig_table_line(p_value = reoff_rate_pvalue[i],
                                                                    bracket = "td",
                                                                    class= "results_table",
                                                                    analyses_name[i],
                                                                    treat_reoff_rate_numb_format[i],
                                                                    comp_reoff_rate_numb_format[i],
                                                                    treat_reoff_rate_table_val[i],
                                                                    comp_reoff_rate_table_val[i],
                                                                    reoff_rate_est_diff[i],
                                                                    JDLpackage:::sig_diff(reoff_rate_pvalue[i]),
                                                                    reoff_rate_pvalue_format[i]
                                        ),
                                        "</tr>"
    )
  }
  reoff_rate_table <- stringr::str_c(reoff_rate_table, "</table>")
  
  
} else { # Analyses with one result
  reoff_rate_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                     "<caption>Table 1: Proportion of ", JDLpackage:::people_func(2,gender), " who committed a proven reoffence in a one-year period after support from ", organisation_name,", compared with a matched comparison group</caption>",
                                     "<tr>",
                                     "<th rowspan='2'>Number in treatment<br>group</th>",
                                     "<th rowspan='2'>Number in comparison<br>group</th>",
                                     "<th colspan='5'>One-year proven reoffending rate</th>",
                                     "</tr>",
                                     "<tr>",
                                     "<th>Treatment group rate<br>(&#37;)</th>",
                                     "<th>Comparison group rate<br>(&#37;)</th>",
                                     "<th>Estimated difference <br>(&#37; points)</th>",
                                     "<th>Significant difference?	</th>",
                                     "<th>p-value</th>",
                                     "</tr>")
  
  for (i in 1:length(analyses_name)){
    reoff_rate_table <-  stringr::str_c(reoff_rate_table, "<tr>",
                                        JDLpackage:::sig_table_line(p_value = reoff_rate_pvalue[i],
                                                                    bracket = "td",
                                                                    class= "results_table",
                                                                    treat_reoff_rate_numb_format[i],
                                                                    comp_reoff_rate_numb_format[i],
                                                                    treat_reoff_rate_table_val[i],
                                                                    comp_reoff_rate_table_val[i],
                                                                    reoff_rate_est_diff[i],
                                                                    JDLpackage:::sig_diff(reoff_rate_pvalue[i]),
                                                                    reoff_rate_pvalue_format[i]
                                        ),
                                        "</tr>"
    )
  }
  reoff_rate_table <- stringr::str_c(reoff_rate_table, "</table>")
  
  
  
}

#################### Reoffending frequency table ####################

if (num_of_analyses != 1) {reoff_freq_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                                              "<caption>Table 2: Number of proven reoffences committed in a one year period by ", JDLpackage:::people_func(2,gender), " who received support from ", organisation_name,", compared with matched comparison groups</caption>",
                                                              "<tr>",
                                                              "<th rowspan='2'>Analysis</th>",
                                                              "<th rowspan='2'>Number in treatment<br>group</th>",
                                                              "<th rowspan='2'>Number in comparison<br>group</th>",
                                                              "<th colspan='5'>One-year proven reoffending frequency (offences per person)</th>",
                                                              "</tr>",
                                                              "<tr>",
                                                              "<th>Treatment group frequency</th>",
                                                              "<th>Comparison group frequency</th>",
                                                              "<th>Estimated difference</th>",
                                                              "<th>Significant difference?	</th>",
                                                              "<th>p-value</th>",
                                                              "</tr>")

for (i in 1:length(analyses_name)){
  reoff_freq_table <- stringr::str_c(reoff_freq_table,"<tr class='results_table'>",
                                     JDLpackage:::sig_table_line(p_value = reoff_freq_pvalue[i],
                                                                 bracket = "td",
                                                                 class= "results_table",
                                                                 analyses_name[i], 
                                                                 treat_reoff_freq_numb_format[i], 
                                                                 comp_reoff_freq_numb_format[i], 
                                                                 treat_reoff_freq_table_val[i], 
                                                                 comp_reoff_freq_table_val[i], 
                                                                 reoff_freq_est_diff[i], 
                                                                 JDLpackage:::sig_diff(reoff_freq_pvalue[i]), 
                                                                 reoff_freq_pvalue_format[i]
                                     ),
                                     "</tr>"
  )
}
reoff_freq_table <- stringr::str_c(reoff_freq_table, "</table>")

} else {
  
  reoff_freq_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                     "<caption>Table 2: Number of proven reoffences committed in a one-year period by ", JDLpackage:::people_func(2,gender), " who received support from ", organisation_name,", compared with a matched comparison group</caption>",
                                     "<tr>",
                                     "<th rowspan='2'>Number in treatment<br>group</th>",
                                     "<th rowspan='2'>Number in comparison<br>group</th>",
                                     "<th colspan='5'>One-year proven reoffending frequency (offences per person)</th>",
                                     "</tr>",
                                     "<tr>",
                                     "<th>Treatment group frequency</th>",
                                     "<th>Comparison group frequency</th>",
                                     "<th>Estimated difference</th>",
                                     "<th>Significant difference?	</th>",
                                     "<th>p-value</th>",
                                     "</tr>")
  
  for (i in 1:length(analyses_name)){
    reoff_freq_table <- stringr::str_c(reoff_freq_table,"<tr class='results_table'>",
                                       JDLpackage:::sig_table_line(p_value = reoff_freq_pvalue[i],
                                                                   bracket = "td",
                                                                   class= "results_table",
                                                                   treat_reoff_freq_numb_format[i], 
                                                                   comp_reoff_freq_numb_format[i], 
                                                                   treat_reoff_freq_table_val[i], 
                                                                   comp_reoff_freq_table_val[i], 
                                                                   reoff_freq_est_diff[i], 
                                                                   JDLpackage:::sig_diff(reoff_freq_pvalue[i]), 
                                                                   reoff_freq_pvalue_format[i]
                                       ),
                                       "</tr>"
    )
  }
  reoff_freq_table <- stringr::str_c(reoff_freq_table, "</table>")
  
}
#################### Time to reoffence ####################

## Rest of the tables are only included if there are more than 30 reoffenders

if (num_of_analyses != 1) {
  time_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                               "<caption>",                             
                               "Table 3: Average time to first proven reoffence in a one-year period for people who received support from ", organisation_name,", compared with matched comparison groups",
                               "</caption>",
                               "<tr>",
                               "<th rowspan='2'>Analysis</th>",
                               "<th rowspan='2'>Number in treatment<br>group</th>",
                               "<th rowspan='2'>Number in comparison<br>group</th>",
                               "<th colspan='5'>Average time to first proven reoffence in a one-year period, for reoffenders only (days)</th>",
                               "</tr>",
                               
                               "<tr>",
                               "<th>Treatment group time</th>",
                               "<th>Comparison group time</th>",
                               "<th>Estimated difference</th>",
                               "<th>Significant difference?</th>",
                               "<th>p-value</th>",
                               "</tr>")
  
  for (i in 1:length(analyses_name)){
    time_table <- stringr::str_c(time_table, "<tr >",
                                 JDLpackage:::sig_table_line(p_value = time_pvalue[i],
                                                             bracket = "td",
                                                             class = "results_table",
                                                             analyses_name[i],
                                                             treat_time_numb_format[i],
                                                             comp_time_numb_format[i],
                                                             treat_time_table_val[i],
                                                             comp_time_table_val[i],
                                                             time_est_diff[i],
                                                             JDLpackage:::sig_diff(time_pvalue[i]),
                                                             time_pvalue_format[i]
                                 ),
                                 "</tr>"
    )
  }
  time_table <- stringr::str_c(time_table, "</table>")
  
} else {
  time_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                               "<caption>",                             
                               "Table 3: Average time to first proven reoffence in a one-year period for people who received support from ", organisation_name,", compared with a matched comparison group",
                               "</caption>",
                               "<tr>",
                               "<th rowspan='2'>Number in treatment<br>group</th>",
                               "<th rowspan='2'>Number in comparison<br>group</th>",
                               "<th colspan='5'>Average time to first proven reoffence in a one-year period, for reoffenders only (days)</th>",
                               "</tr>",
                               
                               "<tr>",
                               "<th>Treatment group time</th>",
                               "<th>Comparison group time</th>",
                               "<th>Estimated difference</th>",
                               "<th>Significant difference?</th>",
                               "<th>p-value</th>",
                               "</tr>")
  
  for (i in 1:length(analyses_name)){
    time_table <- stringr::str_c(time_table, "<tr >",
                                 JDLpackage:::sig_table_line(p_value = time_pvalue[i],
                                                             bracket = "td",
                                                             class = "results_table",
                                                             treat_time_numb_format[i],
                                                             comp_time_numb_format[i],
                                                             treat_time_table_val[i],
                                                             comp_time_table_val[i],
                                                             time_est_diff[i],
                                                             JDLpackage:::sig_diff(time_pvalue[i]),
                                                             time_pvalue_format[i]
                                 ),
                                 "</tr>"
    )
  }
  time_table <- stringr::str_c(time_table, "</table>")
  
  
}
#################### Court outcome rate ####################

if (num_of_analyses != 1) {court_outcome = c("Indictable", "Either way", "Summary")

court_rate_table <- stringr::str_c("<table class='results_table_court' style='width:100%'>",
                                   "<caption>Table 4: Proportion of ", JDLpackage:::people_func(2,gender), " supported by ", 
                                   organisation_name,
                                   " with first proven reoffence in a one-year period by court outcome, compared with similar non-participants (reoffenders only)</caption>",
                                   "<tr>",
                                   "<th rowspan='2'>Analysis</th>",
                                   "<th rowspan='2'>Number in treatment<br>group</th>",
                                   "<th rowspan='2'>Number in comparison<br>group</th>",
                                   "<th colspan='5'>One-year proven reoffending rate by court outcome of first reoffence, for reoffenders only</th>",
                                   "</tr>",
                                   
                                   "<tr>",
                                   "<th>Court outcome</th>",
                                   "<th>Treatment group rate<br>(&#37;)</th>",
                                   "<th>Comparison group rate<br>(&#37;)</th>",
                                   "<th>Estimated difference<br>(&#37; points)</th>",
                                   "<th>Significant difference?</th>",
                                   "<th>p-value</th>",
                                   "</tr>")

for (i in 1:length(analyses_name)){
  
  np_10_court_rate = np_10_marker[[i]][ ,c("severe_ITS1_1y_reoff_excpnds",
                                           "severe_ITS2_1y_reoff_excpnds",
                                           "severe_ITS3_1y_reoff_excpnds")]
  
  if(sum(np_10_court_rate) == 0){
    
    court_rate_table <- stringr::str_c("")
    
  } else {
    
    p = 0
    
    for (j in 1:3){
      
      if (np_10_court_rate[j] == FALSE) { #Doesn't print if np isn't greater than 10
        
        next
        
      } else{
        
        if (p == 0) { #Loop to check whether they need to print the region name
          court_rate_table <- stringr::str_c(court_rate_table,
                                             "<tr>",
                                             "<td>", analyses_name[i], "</td>",
                                             "<td>", treat_court_rate_numb_format[i, j], "</td>",
                                             "<td>", comp_court_rate_numb_format[i, j], "</td>")
        } else {
          court_rate_table <- stringr::str_c(court_rate_table,
                                             "<tr>",
                                             "<td></td>",
                                             "<td></td>",
                                             "<td></td>")
        }
        
        court_rate_table <- stringr::str_c(court_rate_table,
                                           JDLpackage:::sig_table_line(p_value = court_rate_pvalue[i, j],
                                                                       bracket = "td",
                                                                       class = "results_table_court",
                                                                       court_outcome[j],
                                                                       treat_court_rate_table_val[i, j],
                                                                       comp_court_rate_table_val[i, j],
                                                                       court_rate_est_diff[i, j],
                                                                       JDLpackage:::sig_diff(court_rate_pvalue[i, j]),
                                                                       court_rate_pvalue_format[i, j]
                                                                       
                                           ),
                                           "</tr>"
        )
        
        p = p + 1
        
      }
      
    }
  }
}
court_rate_table <- stringr::str_c(court_rate_table, "</table>")

} else {
  
  court_outcome = c("Indictable", "Either way", "Summary")
  
  court_rate_table <- stringr::str_c("<table class='results_table_court' style='width:100%'>",
                                     "<caption>Table 4: Proportion of ", JDLpackage:::people_func(2,gender), " supported by ", 
                                     organisation_name,
                                     " with first proven reoffence in a one-year period by court outcome, compared with similar non-participants (reoffenders only)</caption>",
                                     "<tr>",
                                     "<th rowspan='2'>Number in treatment<br>group</th>",
                                     "<th rowspan='2'>Number in comparison<br>group</th>",
                                     "<th colspan='5'>One-year proven reoffending rate by court outcome of first reoffence, for reoffenders only</th>",
                                     "</tr>",
                                     
                                     "<tr>",
                                     "<th>Court outcome</th>",
                                     "<th>Treatment group rate<br>(&#37;)</th>",
                                     "<th>Comparison group rate<br>(&#37;)</th>",
                                     "<th>Estimated difference<br>(&#37; points)</th>",
                                     "<th>Significant difference?</th>",
                                     "<th>p-value</th>",
                                     "</tr>")
  
  for (i in 1:length(analyses_name)){
    
    np_10_court_rate = np_10_marker[[i]][ ,c("severe_ITS1_1y_reoff_excpnds",
                                             "severe_ITS2_1y_reoff_excpnds",
                                             "severe_ITS3_1y_reoff_excpnds")]
    
    if(sum(np_10_court_rate) == 0){
      
      court_rate_table <- stringr::str_c("")
      
    } else {
      
      p = 0
      
      for (j in 1:3){
        
        if (np_10_court_rate[j] == FALSE) { #Doesn't print if np isn't greater than 10
          
          next
          
        } else{
          
          if (p == 0) { #Loop to check whether they need to print the region name
            court_rate_table <- stringr::str_c(court_rate_table,
                                               "<tr>",
                                               "<td>", treat_court_rate_numb_format[i, j], "</td>",
                                               "<td>", comp_court_rate_numb_format[i, j], "</td>")
          } else {
            court_rate_table <- stringr::str_c(court_rate_table,
                                               "<tr>",
                                               "<td></td>",
                                               "<td></td>")
          }
          
          court_rate_table <- stringr::str_c(court_rate_table,
                                             JDLpackage:::sig_table_line(p_value = court_rate_pvalue[i, j],
                                                                         bracket = "td",
                                                                         class = "results_table_court",
                                                                         court_outcome[j],
                                                                         treat_court_rate_table_val[i, j],
                                                                         comp_court_rate_table_val[i, j],
                                                                         court_rate_est_diff[i, j],
                                                                         JDLpackage:::sig_diff(court_rate_pvalue[i, j]),
                                                                         court_rate_pvalue_format[i, j]
                                                                         
                                             ),
                                             "</tr>"
          )
          
         
          p = p + 1
          
        }
        
      }
    }
  }
  court_rate_table <- stringr::str_c(court_rate_table, "</table>")
  
}

#################### Court outcome frequency ####################

if (num_of_analyses != 1) {court_freq_table <- stringr::str_c("<table class='results_table_court' style='width:100%'>",
                                                              "<caption>Table 5: Number of proven reoffences in a one-year period by court outcome for ", JDLpackage:::people_func(2,gender), " supported by ", organisation_name, ", compared with similar non-participants (reoffenders only)</caption>",
                                                              "<tr>",
                                                              "<th rowspan='2'>Analysis</th>",
                                                              "<th rowspan='2'>Number in treatment<br>group</th>",
                                                              "<th rowspan='2'>Number in comparison<br>group</th>",
                                                              "<th colspan='5'>One-year proven reoffending frequency by court outcome, for reoffenders only</th>",
                                                              "</tr>",
                                                              
                                                              "<tr>",
                                                              "<th>Court outcome</th>",
                                                              "<th>Treatment group frequency</th>",
                                                              "<th>Comparison group frequency</th>",
                                                              "<th>Estimated difference</th>",
                                                              "<th>Significant difference?</th>",
                                                              "<th>p-value</th>",
                                                              "</tr>") 

for (i in 1:length(analyses_name)){
  
  np_10_court_freq = np_10_marker[[i]][ ,c("severe_ITS1_reoff_court_caution_1y",
                                           "severe_ITS2_reoff_court_caution_1y",
                                           "severe_ITS3_reoff_court_caution_1y")]
  
  if(sum(np_10_court_freq) == 0){
    
    court_freq_table <- stringr::str_c("")
    
  } else {
    
    p = 0
    
    for (j in 1:3){
      
      if (np_10_court_freq[j] == FALSE) { #Doesn't print if np isn't greater than 10
        
        next
        
      } else{
        
        if (p == 0) { #Loop to check whether they need to print the region name
          court_freq_table <- stringr::str_c(court_freq_table,
                                             "<tr>",
                                             "<td>", analyses_name[i], "</td>",
                                             "<td>", treat_court_freq_numb_format[i, j], "</td>",
                                             "<td>", comp_court_freq_numb_format[i, j], "</td>")
        } else {
          court_freq_table <- stringr::str_c(court_freq_table,
                                             "<tr>",
                                             "<td></td>",
                                             "<td></td>",
                                             "<td></td>")
        }
        
        court_freq_table <- stringr::str_c(court_freq_table,
                                           JDLpackage:::sig_table_line(p_value = court_freq_pvalue[i, j],
                                                                       bracket = "td",
                                                                       class = "results_table_court",
                                                                       court_outcome[j],
                                                                       treat_court_freq_table_val[i, j],
                                                                       comp_court_freq_table_val[i, j],
                                                                       court_freq_est_diff[i, j],
                                                                       JDLpackage:::sig_diff(court_freq_pvalue[i, j]),
                                                                       court_freq_pvalue_format[i, j]
                                                                       
                                           ),
                                           "</tr>"
        )
        
        p = p + 1
        
      }
      
    }
  }
}

court_freq_table <- stringr::str_c(court_freq_table, "</table>")
} else {
  court_freq_table <- stringr::str_c("<table class='results_table_court' style='width:100%'>",
                                     "<caption>Table 5: Number of proven reoffences in a one-year period by court outcome for ", JDLpackage:::people_func(2,gender), " supported by ", organisation_name, ", compared with similar non-participants (reoffenders only)</caption>",
                                     "<tr>",
                                     "<th rowspan='2'>Number in treatment<br>group</th>",
                                     "<th rowspan='2'>Number in comparison<br>group</th>",
                                     "<th colspan='5'>One-year proven reoffending frequency by court outcome, for reoffenders only</th>",
                                     "</tr>",
                                     
                                     "<tr>",
                                     "<th>Court outcome</th>",
                                     "<th>Treatment group frequency</th>",
                                     "<th>Comparison group frequency</th>",
                                     "<th>Estimated difference</th>",
                                     "<th>Significant difference?</th>",
                                     "<th>p-value</th>",
                                     "</tr>") 
  
  for (i in 1:length(analyses_name)){
    
    np_10_court_freq = np_10_marker[[i]][ ,c("severe_ITS1_reoff_court_caution_1y",
                                             "severe_ITS2_reoff_court_caution_1y",
                                             "severe_ITS3_reoff_court_caution_1y")]
    
    if(sum(np_10_court_freq) == 0){
      
      court_freq_table <- stringr::str_c("")
      
    } else {
      
      p = 0
      
      for (j in 1:3){
        
        if (np_10_court_freq[j] == FALSE) { #Doesn't print if np isn't greater than 10
          
          next
          
        } else{
          
          if (p == 0) { #Loop to check whether they need to print the region name
            court_freq_table <- stringr::str_c(court_freq_table,
                                               "<tr>",
                                               "<td>", treat_court_freq_numb_format[i, j], "</td>",
                                               "<td>", comp_court_freq_numb_format[i, j], "</td>")
          } else {
            court_freq_table <- stringr::str_c(court_freq_table,
                                               "<tr>",
                                               "<td></td>",
                                               "<td></td>")
          }
          
          court_freq_table <- stringr::str_c(court_freq_table,
                                             JDLpackage:::sig_table_line(p_value = court_freq_pvalue[i, j],
                                                                         bracket = "td",
                                                                         class = "results_table_court",
                                                                         court_outcome[j],
                                                                         treat_court_freq_table_val[i, j],
                                                                         comp_court_freq_table_val[i, j],
                                                                         court_freq_est_diff[i, j],
                                                                         JDLpackage:::sig_diff(court_freq_pvalue[i, j]),
                                                                         court_freq_pvalue_format[i, j]
                                                                         
                                             ),
                                             "</tr>"
          )
          
          p = p + 1
          
        }
        
      }
    }
  }
  
  court_freq_table <- stringr::str_c(court_freq_table, "</table>")
  
}

#################### Custody rate ####################

if (num_of_analyses != 1) {custody_rate_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                                                "<caption>Table 6: Proportion of ", JDLpackage:::people_func(2,gender), " who received a custodial sentence for their first proven reoffence after support from ", organisation_name, ", compared with similar non-participants (reoffenders only)</caption>",
                                                                "<tr>",
                                                                "<th rowspan='2'>Analysis</th>",
                                                                "<th rowspan='2'>Number in treatment<br>group</th>",
                                                                "<th rowspan='2'>Number in comparison<br>group</th>",
                                                                "<th colspan='5'>One-year rate of custodial sentencing, for reoffenders only</th>",
                                                                "</tr>",
                                                                
                                                                "<tr>",
                                                                "<th>Treatment group rate<br>(&#37;)</th>",
                                                                "<th>Comparison group rate<br>(&#37;)</th>",
                                                                "<th>Estimated difference<br>(&#37; points)</th>",
                                                                "<th>Significant difference?</th>",
                                                                "<th>p-value</th>",
                                                                "</tr>")

for (i in 1:length(analyses_name)){
  custody_rate_table <- stringr::str_c(custody_rate_table, "<tr>",
                                       JDLpackage:::sig_table_line(p_value = custody_rate_pvalue[i],
                                                                   bracket = "td",
                                                                   class = "results_table",
                                                                   analyses_name[i],
                                                                   treat_custody_rate_numb_format[i],
                                                                   comp_custody_rate_numb_format[i],
                                                                   treat_custody_rate_table_val[i],
                                                                   comp_custody_rate_table_val[i],
                                                                   custody_rate_est_diff[i],
                                                                   JDLpackage:::sig_diff(custody_rate_pvalue[i]),
                                                                   custody_rate_pvalue_format[i]
                                       ),
                                       "</tr>"
  )
}
custody_rate_table <- stringr::str_c(custody_rate_table, "</table>")

} else {
  
  custody_rate_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                       "<caption>Table 6: Proportion of ", JDLpackage:::people_func(2,gender), " who received a custodial sentence for their first proven reoffence after support from ", organisation_name, " compared with similar non-participants (reoffenders only)</caption>",
                                       "<tr>",
                                       "<th rowspan='2'>Number in treatment<br>group</th>",
                                       "<th rowspan='2'>Number in comparison<br>group</th>",
                                       "<th colspan='5'>One-year rate of custodial sentencing, for reoffenders only</th>",
                                       "</tr>",
                                       
                                       "<tr>",
                                       "<th>Treatment group rate<br>(&#37;)</th>",
                                       "<th>Comparison group rate<br>(&#37;)</th>",
                                       "<th>Estimated difference<br>(&#37; points)</th>",
                                       "<th>Significant difference?</th>",
                                       "<th>p-value</th>",
                                       "</tr>")
  
  for (i in 1:length(analyses_name)){
    custody_rate_table <- stringr::str_c(custody_rate_table, "<tr>",
                                         JDLpackage:::sig_table_line(p_value = custody_rate_pvalue[i],
                                                                     bracket = "td",
                                                                     class = "results_table",
                                                                     treat_custody_rate_numb_format[i],
                                                                     comp_custody_rate_numb_format[i],
                                                                     treat_custody_rate_table_val[i],
                                                                     comp_custody_rate_table_val[i],
                                                                     custody_rate_est_diff[i],
                                                                     JDLpackage:::sig_diff(custody_rate_pvalue[i]),
                                                                     custody_rate_pvalue_format[i]
                                         ),
                                         "</tr>"
    )
  }
  custody_rate_table <- stringr::str_c(custody_rate_table, "</table>")
  
  
}
#################### Custody frequency ####################
if (num_of_analyses != 1){
  custody_freq_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                       "<caption>Table 7: Number of custodial sentences received in a one-year period by ", JDLpackage:::people_func(2,gender), " who received support from ", organisation_name, ", compared to similar non-participants (reoffenders only)</caption>",
                                       "<tr>",
                                       "<th rowspan='2'>Analysis</th>",
                                       "<th rowspan='2'>Number in treatment<br>group</th>",
                                       "<th rowspan='2'>Number in comparison<br>group</th>",
                                       "<th colspan='5'>One-year frequency of custodial sentencing, for reoffenders only (sentences per person)</th>",
                                       "</tr>",
                                       
                                       "<tr>",
                                       "<th>Treatment group frequency</th>",
                                       "<th>Comparison group frequency</th>",
                                       "<th>Estimated difference</th>",
                                       "<th>Significant difference?</th>",
                                       "<th>p-value</th>",
                                       "</tr>")
  
  for (i in 1:length(analyses_name)){
    custody_freq_table <- stringr::str_c(custody_freq_table, "<tr>",
                                         JDLpackage:::sig_table_line(p_value = custody_freq_pvalue[i],
                                                                     bracket = "td",
                                                                     class = "results_table",
                                                                     analyses_name[i],
                                                                     treat_custody_freq_numb_format[i],
                                                                     comp_custody_freq_numb_format[i],
                                                                     treat_custody_freq_table_val[i],
                                                                     comp_custody_freq_table_val[i],
                                                                     custody_freq_est_diff[i],
                                                                     JDLpackage:::sig_diff(custody_freq_pvalue[i]),
                                                                     custody_freq_pvalue_format[i]
                                         ),
                                         "</tr>"
    )
  }
  custody_freq_table <- stringr::str_c(custody_freq_table, "</table>")
  
} else {
  
  custody_freq_table <- stringr::str_c("<table class='results_table' style='width:100%'>",
                                       "<caption>Table 7: Number of custodial sentences received in a one-year period by ", JDLpackage:::people_func(2,gender), " who received support from ", organisation_name, ", compared to similar non-participants (reoffenders only)</caption>",
                                       "<tr>",
                                       "<th rowspan='2'>Number in treatment<br>group</th>",
                                       "<th rowspan='2'>Number in comparison<br>group</th>",
                                       "<th colspan='5'>One-year frequency of custodial sentencing, for reoffenders only (sentences per person)</th>",
                                       "</tr>",
                                       
                                       "<tr>",
                                       "<th>Treatment group frequency</th>",
                                       "<th>Comparison group frequency</th>",
                                       "<th>Estimated difference</th>",
                                       "<th>Significant difference?</th>",
                                       "<th>p-value</th>",
                                       "</tr>")
  
  for (i in 1:length(analyses_name)){
    custody_freq_table <- stringr::str_c(custody_freq_table, "<tr>",
                                         JDLpackage:::sig_table_line(p_value = custody_freq_pvalue[i],
                                                                     bracket = "td",
                                                                     class = "results_table",
                                                                     treat_custody_freq_numb_format[i],
                                                                     comp_custody_freq_numb_format[i],
                                                                     treat_custody_freq_table_val[i],
                                                                     comp_custody_freq_table_val[i],
                                                                     custody_freq_est_diff[i],
                                                                     JDLpackage:::sig_diff(custody_freq_pvalue[i]),
                                                                     custody_freq_pvalue_format[i]
                                         ),
                                         "</tr>"
    )
  }
  custody_freq_table <- stringr::str_c(custody_freq_table, "</table>")
  
  
}



#################### Table of Contents ####################


table_of_contents <- stringr::str_c("<table class = 'ToC'>")

for (i in 2:nrow(toc_df)){
  table_of_contents %<>% stringr::str_c("<tr style = 'height: 40px;'>",
                                        "<td>",
                                        toc_df$toc_section_name[i],
                                        "</td>",
                                        
                                        "<td>",
                                        toc_df$toc_page_num[i],
                                        "</td>",
                                        "</tr>"
  )
}

table_of_contents %<>% stringr::str_c("</table>")

#################### Profile of the treatment group ####################


treat_table <- str_c("<div class = 'treatment_profile'>",
                     
                     "<div class = 'treatment_profile1'>",
                     
                     "<div style = 'text-align: center; font-weight: bold;'>",
                     "Participants included in analysis<br>",
                     "(",
                     treat_reoff_rate_numb_format[headline_analysis],
                     " offenders",
                     if (num_of_analyses>1) paste0(" in ",analyses_name[headline_analysis]," analysis"),
                     ")",
                     "</div>",
                     
                     "<br>",
                     "<ul>",
                     "<li>", sex_sentence, "</li>",
                     "<li>", ethnicity_sentence,"</li>",
                     "<li>", uk_sentence, "</li>",
                     "<br>",
                     "<li>", age_sentence, "</li>",
                     opt_list,
                     "</ul>",
                     "</div>")
