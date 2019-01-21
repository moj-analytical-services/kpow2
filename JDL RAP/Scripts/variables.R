########################################################## Packages##########################################################

library("JDLpackage", lib.loc = "~/R/library")

packages <- c(
  "markdown",
  "ggplot2",
  "png",
  "dplyr",
  "glue",
  "magrittr",
  "stringr",
  "s3tools",
  "devtools",
  "tabulizer"
)

sapply(packages,
       require,
       character.only = TRUE
)

####################################################################################################################
#-----------------------------------------------Variables to change------------------------------------------------#
####################################################################################################################



########################################################## Data files##########################################################

data_file_folder <- ""
icon_file_folder <- "Icons/"
if (!exists("excel_file")) excel_file <- "JDL info one.xlsx"



####################################################################################################################
#------------------------------------------------------------------------------------------------------------------#
####################################################################################################################

# Reading in excel sheet
info_wb <- openxlsx::loadWorkbook(paste0(
  data_file_folder,
  excel_file
))

# name of tabs in Excel file
info_sheet_names <- info_wb$sheet_names

# Report information
report_info <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                    sheet = "Report information")
)

# Paragraphs
paragraphs <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                    sheet = "Paragraphs",
                                                   rows = c(1, 3:100))
)

# Chevron diagram information
chevron_df <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                   sheet = "Chevron", rows = c(1, 3:100))
)


# Chevron diagram information
toc_df <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                               sheet = "Table of Contents",
                                               rowNames = FALSE)
)
                            
# variable names
variable_lookup <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                        sheet = "Variables names",
                                                        na.strings = c("","NA"))
)

# variable names
oasys_vars <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                        sheet = "OASys variables",
                                                        na.strings = c("","NA"))
)

# Paragraphs to be included
sig_sentences_df <- as.data.frame(openxlsx::readWorkbook(info_wb,
                                                      sheet = "Significant sentences",
                                                      na.strings = c("","NA"))
)

###Variables from Report information tab

# Number of analyses to publish
num_of_analyses <- as.numeric(report_info$num_of_analyses[2])

# The analysis names as they will appear in report tables
analyses_name <- na.omit(report_info$analyses_name[-1])

# Whether the analysis is controlled regionally 
controlled_region <- report_info$controlled_region[-1][1:length(analyses_name)]

if (length(analyses_name) != num_of_analyses) {
  stop("Make sure that you have put the same number of result names as number of analyses")
}

if (anyDuplicated(analyses_name)) {
  stop("All result names must be unique")
}

# The analysis names as they will appear in report tables
analyses_region <- na.omit(report_info$analyses_region[-1])


headline_analysis <- as.numeric(report_info$headline_analysis[2])


### Meta data about Excel sheet

# Determines where the tabs are that signify the start of the results
t_test_marker <- grep("t-test_marker", info_sheet_names)
stand_diff_marker <- grep("standard_difference_marker", info_sheet_names)

# reading in tabs holding t-test data
t_test_tabs <- info_sheet_names [(t_test_marker + 1):(t_test_marker + num_of_analyses)]

# reading in tabs holding standard differences data
std_diff_tabs <- info_sheet_names [(stand_diff_marker + 1):(stand_diff_marker + num_of_analyses)]


t_test_data <- list()
for (i in t_test_tabs) { # creates a list containing all the t-test results

  t_test_data[[i]] <- as.data.frame(openxlsx::readWorkbook(paste0(data_file_folder, excel_file),
                                                           sheet = i,
                                                           na.strings = "Inf",
                                                           rowNames = TRUE,
                                                           colNames = TRUE)
  )
}


std_diffs <- list()
for (i in std_diff_tabs) { # creates a list containing all the standard deviation results

  std_diffs[[i]] <- as.data.frame(openxlsx::readWorkbook(paste0(data_file_folder, excel_file),
                                                         sheet = i,
                                                         na.strings = c("NA", "Inf"),
                                                         colNames = TRUE)
  )
}

reoff_30_marker <- list() #indicator to show if the analysis has more than 30 participants
for (i in t_test_tabs) {

  if(t_test_data[[i]]['treat_number', 'days_to_reoff_1y'] < 30){

    reoff_30_marker[i] = 0
  } else {

    reoff_30_marker[i] = 1
  }
}

np_10_marker <- list() #indicator to show if the np value is greater than 10
for (i in t_test_tabs) {

  np_10_marker[[i]] <- ifelse(t_test_data[[i]]['np_treat', ] < 10,
                              FALSE,
                              TRUE)
}

if(sum(is.na(std_diff_tabs)) != 0 ){
  stop("The number of standard difference tabs doesn't match the number of results you have stated")
}

if (sum(t_test_tabs == "standard_difference_marker") == 1){
  stop("The number of t-test tabs doesn't match the number of results you have stated")
}

######### INPUTS #############
# From the report information tab
gender <- report_info$gender[2]
gender_plural <- JDLpackage:::people_func(2, gender)
gender_singular <- JDLpackage:::people_func(1, gender)
organisation_name <- report_info$name[2]
time_period <- tolower(report_info$time_period[2])
month <- report_info$month[2]
year <- report_info$year[2]

# From the drop out data tab
num_people_start <- JDLpackage:::format_num(chevron_df$total[1])
num_people_end <- JDLpackage:::format_num(na.omit(chevron_df$total)[length(na.omit(chevron_df$description))])
num_people_diff <- JDLpackage:::format_num(na.omit(chevron_df$total)[length(na.omit(chevron_df$description))] - chevron_df$total[1])


####### Checking variable names ########


wanted_names <- c("indicator_1y_reoff_excpnds",
                 "freq_reoff_court_caution_1y",
                 "days_to_reoff_1y",
                 "severe_ITS1_1y_reoff_excpnds",
                 "severe_ITS2_1y_reoff_excpnds",
                 "severe_ITS3_1y_reoff_excpnds",
                 "severe_ITS1_reoff_court_caution_1y",
                 "severe_ITS2_reoff_court_caution_1y",
                 "severe_ITS3_reoff_court_caution_1y",
                 "custody_for_reoff_1y_excPNDs",
                 "freq_prison_for_reoff_1y_excpnds")

if (length(wanted_names)==0 || any(!wanted_names %in% sig_sentences_df$Variable))
  stop("The required variable(s) ",
      paste(wanted_names[!wanted_names %in% sig_sentences_df$Variable], collapse=", "),
      " are not in the 'Significant sentences' tab of the spreadsheet")

for (i in t_test_tabs) {
  
  logical_names <- wanted_names %in%  names(t_test_data[[i]])
  
  if(sum(logical_names) != length(logical_names)) {
    
    stop("The variable(s) ",
         paste(wanted_names[!logical_names],
               collapse = ", "),
         " are not recognised by this script.")
  }
}

## Testing row names in t-test tabs

wanted_rows <- c("treat_number",
                 "treat_mean",
                 "treat_ci",
                 "treat_lower",
                 "treat_upper",
                 "np_treat",
                 "comparison_number",
                 "comparison_mean",
                 "comparison_ci",
                 "comparison_lower",
                 "comparison_upper",
                 "np_comparison",
                 "difference_means",
                 "difference_ci",
                 "difference_lower",
                 "difference_upper",
                 "difference_p",
                 "number_needed") 

for (i in t_test_tabs) {
  
  logical_names <- wanted_rows %in% row.names(t_test_data[[i]])
  
  if(sum(logical_names) != length(logical_names)) {
    
    stop("The row names(s) ",
         paste(row.names(t_test_data[[i]]),
               collapse = ", "),
         " are not recognised by this script.")
  }
}

for (i in t_test_tabs) {
  t_test_data[[i]] <- t_test_data[[i]][rownames(t_test_data[[i]]) %in% wanted_rows, colnames(t_test_data[[i]]) %in% wanted_names]
} # Remove any rows and columns that are not required from the t-test results

if (any(lapply(as.data.frame(t(variable_lookup)), function(r){anyNA(r) && !all(is.na(r))})))
  stop("Some variable name information is missing")

variable_lookup_vars <- unlist(subset(variable_lookup, 
                                        !is.na(names) & variables != 'min_age' & variables != 'max_age',
                                        select = variables))
  
logical_vars <- variable_lookup_vars %in% std_diffs[[headline_analysis]]$X1
  
if(sum(!logical_vars) > 0) {
  
  stop(paste("The standardised difference variable", if (sum(!logical_vars)>1) "s", " ",
    paste(variable_lookup_vars[!logical_vars],
      collapse = ", "),
    " in the 'Variable names' tab ", if (sum(!logical_vars)>1) "are" else "is", " not in the '",
    std_diff_tabs[headline_analysis], 
    "' standardised difference results tab", sep=""))
}


########################################################## Defining variables##########################################################


variables <- c(
  ### Reoffending rate
  "treat_reoff_rate",             # The reoffending rate for the treatment group
  "treat_reoff_rate_format",      # The reoffending rate for the treatment group to the nearest integer
  "comp_reoff_rate",              # The reoffending rate for the comparison group
  "comp_reoff_rate_format",       # The reoffending rate for the comparison group to the nearest integer
  "diff_reoff_rate",              # The difference in the reoffending rate between the comparison and treatment group
  "diff_reoff_rate_format",       # The difference in the reoffending rate between the comparison and treatment group to the nearest integer

  "reoff_rate_pvalue",            # The p-value for the difference in reoffending rate
  "reoff_rate_pvalue_format",     # The p-value for the difference in reoffending rate to the nearest 2dp
  
  "reoff_rate_CI_l",              # the lower confidence interval
  "reoff_rate_CI_l_format",       # the lower confidence interval to 1dp
  "reoff_rate_CI_u",              # the upper confidence interval
  "reoff_rate_CI_u_format",       # the lower confidence interval to 1dp
  
  "treat_reoff_rate_numb",        # Number of people in the treatment group
  "treat_reoff_rate_numb_format", # Number of people in the treatment group formatted with commas
  "comp_reoff_rate_numb",         # Number of people in the comparison group
  "comp_reoff_rate_numb_format",  # Number of people in the comparison group formatted with commas
  "treat_reoff_rate_table_val",   # The reoffending rate for the treatment group to the nearest integer
  "comp_reoff_rate_table_val",    # The reoffending rate for the comparison group to the nearest integer
  "reoff_rate_est_diff",          # A phrase that summarised the confidence interval

  ## Reoffending frequency
  "treat_reoff_freq",	            # The reoffending frequency for the treatment group
  "treat_reoff_freq_format",	    # The reoffending frequency for the treatment group to the nearest integer
  "comp_reoff_freq",	            # The reoffending frequency for the comparison group
  "comp_reoff_freq_format",	      # The reoffending frequency for the comparison group to the nearest integer
  "diff_reoff_freq",            	# The difference in the reoffending frequency between the comparison and treatment group
  "diff_reoff_freq_format",	      # The difference in the reoffending frequency between the comparison and treatment group to the nearest integer
  
  "reoff_freq_pvalue",	          # The p-value for the difference in reoffending frequency
  "reoff_freq_pvalue_format",	    # The p-value for the difference in reoffending frequency to the nearest 2dp
  
  "reoff_freq_CI_l",	            # the lower confidence interval
  "reoff_freq_CI_l_format",	      # the lower confidence interval to 1dp
  "reoff_freq_CI_u",	            # the upper confidence interval
  "reoff_freq_CI_u_format",	      # the lower confidence interval to 1dp
  
  "treat_reoff_freq_numb",	      # Number of people in the treatment group
  "treat_reoff_freq_numb_format",	# Number of people in the treatment group formatted with commas
  "comp_reoff_freq_numb",	        # Number of people in the comparison group
  "comp_reoff_freq_numb_format",  # Number of people in the comparison group formatted with commas
  "treat_reoff_freq_table_val",	  # The reoffending frequency for the treatment group to the nearest integer
  "comp_reoff_freq_table_val",	  # The reoffending frequency for the comparison group to the nearest integer
  "reoff_freq_est_diff",	        # A phrase that summarised the confidence interval
  
  ## Time to reoffend
  "treat_time",	                  # The time to reoffence for the treatment group
  "treat_time_format",	          # The time to reoffence for the treatment group to the nearest integer
  "comp_time",	                  # The time to reoffence for the comparison group
  "comp_time_format",	            # The time to reoffence for the comparison group to the nearest integer
  "diff_time",	                  # The difference in the time to reoffence between the comparison and treatment group
  "diff_time_format",	            # The difference in the time to reoffence between the comparison and treatment group to the nearest integer
  
  "time_pvalue",	                # The p-value for the difference in time to reoffence
  "time_pvalue_format",	          # The p-value for the difference in time to reoffence to the nearest 2dp
  
  "time_CI_l",	                  # the lower confidence interval
  "time_CI_l_format",	            # the lower confidence interval to 1dp
  "time_CI_u",	                  # the upper confidence interval
  "time_CI_u_format",	            # the lower confidence interval to 1dp
  
  "treat_time_numb",	            # Number of people in the treatment group
  "treat_time_numb_format",	      # Number of people in the treatment group formatted with commas
  "comp_time_numb",	              # Number of people in the comparison group
  "comp_time_numb_format",	      # Number of people in the comparison group formatted with commas
  "treat_time_table_val",	        # The time to reoffence for the treatment group to the nearest integer
  "comp_time_table_val",	        # The time to reoffence for the comparison group to the nearest integer
  "time_est_diff",	              # A phrase that summarised the confidence interval
  
  ## custody rate
  "treat_custody_rate",              # The custody rate for the treatment group
  "treat_custody_rate_format",       # The custody rate for the treatment group to the nearest integer
  "comp_custody_rate",               # The custody rate for the comparison group
  "comp_custody_rate_format",        # The custody rate for the comparison group to the nearest integer
  "custody_rate_CI_l",               # the lower confidence interval
  "custody_rate_CI_l_format",        # the lower confidence interval to 1dp
  "custody_rate_CI_u",               # the upper confidence interval
  "custody_rate_CI_u_format",        # the lower confidence interval to 1dp
  "custody_rate_pvalue",             # The p-value for the difference in time to reoffence
  "custody_rate_pvalue_format",      # The p-value for the difference in time to reoffence to the nearest 2dp
  "treat_custody_rate_numb",         # Number of people in the treatment group
  "treat_custody_rate_numb_format",  # Number of people in the treatment group formatted with commas
  "comp_custody_rate_numb",          # Number of people in the comparison group
  "comp_custody_rate_numb_format",   # Number of people in the comparison group formatted with commas
  "treat_custody_rate_table_val",    # The custody rate for the treatment group to the nearest integer
  "comp_custody_rate_table_val",     # The custody rate for the comparison group to the nearest integer
  "custody_rate_est_diff",           # A phrase that summarised the confidence interval
  
  "treat_custody_freq",              # The custody frequency for the treatment group
  "treat_custody_freq_format",       # The custody frequency for the treatment group to the nearest integer
  "comp_custody_freq",               # The custody frequency for the comparison group
  "comp_custody_freq_format",        # The custody frequency for the comparison group to the nearest integer
  "custody_freq_CI_l",               # the lower confidence interval
  "custody_freq_CI_l_format",        # the lower confidence interval to 1dp
  "custody_freq_CI_u",               # the upper confidence interval
  "custody_freq_CI_u_format",        # the lower confidence interval to 1dp
  "custody_freq_pvalue",             # The p-value for the difference in time to reoffence
  "custody_freq_pvalue_format",      # The p-value for the difference in time to reoffence to the nearest 2dp
  "treat_custody_freq_numb",         # Number of people in the treatment group
  "treat_custody_freq_numb_format",  # Number of people in the treatment group formatted with commas
  "comp_custody_freq_numb",          # Number of people in the comparison group
  "comp_custody_freq_numb_format",   # Number of people in the comparison group formatted with commas
  "treat_custody_freq_table_val",	   # The custody frequency for the treatment group to the nearest integer
  "comp_custody_freq_table_val",	   # The custody frequency for the comparison group to the nearest integer
  "custody_freq_est_diff"            # A phrase that summarised the confidence interval
  
)

sapply(variables,
       assign,
       value = vector("numeric", num_of_analyses),
       envir = globalenv())


variables1 <- c("treat_court_rate",              # The court rate for the treatment group
                "treat_court_rate_format",       # The court rate for the treatment group to the nearest integer
                "comp_court_rate",               # The court rate for the comparison group
                "comp_court_rate_format",        # The court rate for the comparison group to the nearest integer
                "court_rate_CI_l",               # the lower confidence interval
                "court_rate_CI_l_format",        # the lower confidence interval to 1dp
                "court_rate_CI_u",               # the upper confidence interval
                "court_rate_CI_u_format",        # the lower confidence interval to 1dp
                "court_rate_pvalue",             # The p-value for the difference in time to reoffence
                "court_rate_pvalue_format",      # The p-value for the difference in time to reoffence to the nearest 2dp
                "treat_court_rate_numb",         # Number of people in the treatment group
                "treat_court_rate_numb_format",  # Number of people in the treatment group formatted with commas
                "comp_court_rate_numb",          # Number of people in the comparison group
                "comp_court_rate_numb_format",   # Number of people in the comparison group formatted with commas
                "treat_court_rate_table_val",	   # The court rate for the treatment group to the nearest integer
                "comp_court_rate_table_val",	   # The court rate for the comparison group to the nearest integer
                "court_rate_est_diff",           # A phrase that summarised the confidence interval

                "treat_court_freq",              # The court frequency for the treatment group
                "treat_court_freq_format",       # The court frequency for the treatment group to the nearest integer
                "comp_court_freq",               # The court frequency for the comparison group
                "comp_court_freq_format",        # The court frequency for the comparison group to the nearest integer
                "court_freq_CI_l",               # the lower confidence interval
                "court_freq_CI_l_format",        # the lower confidence interval to 1dp
                "court_freq_CI_u",               # the upper confidence interval
                "court_freq_CI_u_format",        # the lower confidence interval to 1dp
                "court_freq_pvalue",             # The p-value for the difference in time to reoffence
                "court_freq_pvalue_format",      # The p-value for the difference in time to reoffence to the nearest 2dp
                "treat_court_freq_numb",         # Number of people in the treatment group
                "treat_court_freq_numb_format",  # Number of people in the treatment group formatted with commas
                "comp_court_freq_numb",          # Number of people in the comparison group
                "comp_court_freq_numb_format",   # Number of people in the comparison group formatted with commas
                "treat_court_freq_table_val",	   # The court frequency for the treatment group to the nearest integer
                "comp_court_freq_table_val",	   # The court frequency for the comparison group to the nearest integer
                "court_freq_est_diff")           # A phrase that summarised the confidence interval

lapply(variables1,
       assign,
       value = data.frame(ITS1 = vector("numeric", num_of_analyses),
                    ITS2 = vector("numeric", num_of_analyses),
                    ITS3 = vector("numeric", num_of_analyses)),
       envir = globalenv())


for (i in 1:num_of_analyses) {
  temp_t_test_data <- t_test_data[[i]]

  ########################################################## People committing a proven reoffence rate ##########################################################
  treat_reoff_rate[i] <- temp_t_test_data["treat_mean", "indicator_1y_reoff_excpnds"] * 100 # Rate of reoffending for the treatment group
  treat_reoff_rate_format[i] <- JDLpackage:::format_num(treat_reoff_rate[i])

  comp_reoff_rate[i] <- temp_t_test_data["comparison_mean", "indicator_1y_reoff_excpnds"] * 100 # Rate of reoffending for the comparison group
  comp_reoff_rate_format[i] <- JDLpackage:::format_num(comp_reoff_rate[i])

  diff_reoff_rate[i] <- temp_t_test_data["difference_means", "indicator_1y_reoff_excpnds"] * 100 # Difference in reoffending rate between both groups
  diff_reoff_rate_format[i] <- JDLpackage:::format_num(diff_reoff_rate[i])

  reoff_rate_pvalue[i] <- temp_t_test_data["difference_p", "indicator_1y_reoff_excpnds"] # p-value of the difference in the reoffending rate
  reoff_rate_pvalue_format[i] <- JDLpackage:::format_num(reoff_rate_pvalue[i],
                                                        digits = 2,
                                                        p_value = TRUE)

  reoff_rate_CI_l[i] <- temp_t_test_data["difference_lower", "indicator_1y_reoff_excpnds"] * 100 # Lower confidence interval of the difference in reoffending rate
  reoff_rate_CI_u[i] <- temp_t_test_data["difference_upper", "indicator_1y_reoff_excpnds"] * 100 # Upper confidence interval of the difference in reoffending rate

  treat_reoff_rate_numb[i] <- temp_t_test_data["treat_number", "indicator_1y_reoff_excpnds"] # number of people in the treatment group
  treat_reoff_rate_numb_format[i] <- JDLpackage:::format_num(treat_reoff_rate_numb[i])

  comp_reoff_rate_numb[i] <- temp_t_test_data["comparison_number", "indicator_1y_reoff_excpnds"] # number of people in the comparison group
  comp_reoff_rate_numb_format[i] <- JDLpackage:::format_num(comp_reoff_rate_numb[i])

  treat_reoff_rate_table_val[i] <- JDLpackage:::format_num(treat_reoff_rate[i],
                                                          digits = 0)
  comp_reoff_rate_table_val[i] <- JDLpackage:::format_num(comp_reoff_rate[i],
                                                         digits = 0)

  reoff_rate_CI_l_format[i] <- JDLpackage:::format_num(reoff_rate_CI_l[i],
                                                      digits = 0,
                                                      allow_neg = TRUE)
  reoff_rate_CI_u_format[i] <- JDLpackage:::format_num(reoff_rate_CI_u[i],
                                                      digits = 0,
                                                      allow_neg = TRUE)
  reoff_rate_est_diff[i] <- paste0(if (reoff_rate_CI_l_format[i]!="-0") reoff_rate_CI_l_format[i] else "0", " to ", if (reoff_rate_CI_u_format[i]!="-0") reoff_rate_CI_u_format[i] else "0")

  ########################################################## Proven reoffences variables##########################################################

  treat_reoff_freq[i] <- temp_t_test_data["treat_mean", "freq_reoff_court_caution_1y"] * 100 # Frequency of reoffending for the treatment group
  treat_reoff_freq_format[i] <- JDLpackage:::format_num(treat_reoff_freq[i])
  
  comp_reoff_freq[i] <- temp_t_test_data["comparison_mean", "freq_reoff_court_caution_1y"] * 100 # Frequency of reoffending for the Comparison group
  comp_reoff_freq_format[i] <- JDLpackage:::format_num(comp_reoff_freq[i])
  
  diff_reoff_freq[i] <- temp_t_test_data["difference_means", "freq_reoff_court_caution_1y"] * 100 # Difference in reoffending frequency between both groups
  diff_reoff_freq_format[i] <- JDLpackage:::format_num(diff_reoff_freq[i])

  reoff_freq_pvalue[i] <- temp_t_test_data["difference_p", "freq_reoff_court_caution_1y"] # p-value of the difference in the reoffending frequency
  reoff_freq_pvalue_format[i] <- JDLpackage:::format_num(reoff_freq_pvalue[i],
                                                        digits = 2,
                                                        p_value = TRUE)
  
  reoff_freq_CI_l[i] <- temp_t_test_data["difference_lower", "freq_reoff_court_caution_1y"] * 100 # Lower confidence interval of the difference in reoffending frequency
  reoff_freq_CI_u[i] <- temp_t_test_data["difference_upper", "freq_reoff_court_caution_1y"] * 100 # Upper confidence interval of the difference in reoffending frequency

  treat_reoff_freq_numb[i] <- temp_t_test_data["treat_number", "freq_reoff_court_caution_1y"] # Number of people in the treatment group
  treat_reoff_freq_numb_format[i] <- JDLpackage:::format_num(treat_reoff_freq_numb[i])
  
  comp_reoff_freq_numb[i] <- temp_t_test_data["comparison_number", "freq_reoff_court_caution_1y"] # Number of people in the comparison group
  comp_reoff_freq_numb_format[i] <- JDLpackage:::format_num(comp_reoff_freq_numb[i])
  
  treat_reoff_freq_table_val[i] <- JDLpackage:::format_num(treat_reoff_freq[i]/100,
                                                          digits = 2)
  comp_reoff_freq_table_val[i] <- JDLpackage:::format_num(comp_reoff_freq[i]/100,
                                                         digits = 2)
  
  reoff_freq_CI_l_format[i] <- JDLpackage:::format_num(reoff_freq_CI_l[i]/100,
                                                      digits = 2,
                                                      allow_neg = TRUE)
  reoff_freq_CI_u_format[i] <- JDLpackage:::format_num(reoff_freq_CI_u[i]/100,
                                                      digits = 2,
                                                      allow_neg = TRUE)
  
  reoff_freq_est_diff[i] <- paste0(if (reoff_freq_CI_l_format[i]!="-0.00") reoff_freq_CI_l_format[i] else "0.00", " to ", if (reoff_freq_CI_u_format[i]!="-0.00") reoff_freq_CI_u_format[i] else "0.00")

  ########################################################## Average time between reoffences##########################################################

  treat_time[i] <- temp_t_test_data["treat_mean", "days_to_reoff_1y"] # Days to first reoffence for the treatment group
  treat_time_format[i] <- JDLpackage:::format_num(treat_time[i])

  comp_time[i] <- temp_t_test_data["comparison_mean", "days_to_reoff_1y"] # Days to first reoffence for the treatment group
  comp_time_format[i] <- JDLpackage:::format_num(comp_time[i])

  diff_time[i] <- temp_t_test_data["difference_means", "days_to_reoff_1y"] # Difference in days to first offence between both groups
  diff_time_format[i] <- JDLpackage:::format_num(diff_time[i])

  time_pvalue[i] <- temp_t_test_data["difference_p", "days_to_reoff_1y"] # p-value of the difference in days to first offence frequency
  time_pvalue_format[i] <- JDLpackage:::format_num(time_pvalue[i],
                                                  digits = 2,
                                                  p_value = TRUE)

  time_CI_l[i] <- temp_t_test_data["difference_lower", "days_to_reoff_1y"] # Lower confidence interval of the difference in days to first offence
  time_CI_u[i] <- temp_t_test_data["difference_upper", "days_to_reoff_1y"] # Upper confidence interval of the difference in days to first offence

  treat_time_numb[i] <- temp_t_test_data["treat_number", "days_to_reoff_1y"] # Number of people in the treatment group
  treat_time_numb_format[i] <- JDLpackage:::format_num(treat_time_numb[i])
  
  comp_time_numb[i] <- temp_t_test_data["comparison_number", "days_to_reoff_1y"] # Number of people in the comparison group
  comp_time_numb_format[i] <- JDLpackage:::format_num(comp_time_numb[i])
  
  treat_time_table_val[i] <- JDLpackage:::format_num(treat_time[i],
                                                    digits = 0)
  comp_time_table_val[i] <- JDLpackage:::format_num(comp_time[i],
                                                   digits = 0)
  
  time_CI_l_format[i] <- JDLpackage:::format_num(time_CI_l[i],
                                                digits = 0,
                                                allow_neg = TRUE)
  time_CI_u_format[i] <- JDLpackage:::format_num(time_CI_u[i],
                                                digits = 0,
                                                allow_neg = TRUE)
  
  time_est_diff[i] <- paste0(if (time_CI_l_format[i]!="-0") time_CI_l_format[i] else "0", " to ", if (time_CI_u_format[i]!="-0") time_CI_u_format[i] else "0")

  ########################################################## Court outcome rate ##########################################################

  treat_court_rate[i, 1:3] <- temp_t_test_data["treat_mean", c("severe_ITS1_1y_reoff_excpnds",
                                                               "severe_ITS2_1y_reoff_excpnds",
                                                               "severe_ITS3_1y_reoff_excpnds")] * 100 # Reoffending rate by court outcome for the treatment group
  treat_court_rate_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                            treat_court_rate[i, ],
                                            MoreArgs = list(digits = 1))

  comp_court_rate[i, 1:3] <- temp_t_test_data["comparison_mean", c("severe_ITS1_1y_reoff_excpnds",
                                                                "severe_ITS2_1y_reoff_excpnds",
                                                                "severe_ITS3_1y_reoff_excpnds")] * 100 # Reoffending rate by court outcome for the treatment group
  comp_court_rate_format[i, 1:3] <-  mapply(JDLpackage:::format_num,
                                            comp_court_rate[i, ],
                                            MoreArgs = list(digits = 1))

  court_rate_pvalue[i, 1:3] <- temp_t_test_data["difference_p", c("severe_ITS1_1y_reoff_excpnds",
                                                                  "severe_ITS2_1y_reoff_excpnds",
                                                                  "severe_ITS3_1y_reoff_excpnds")] # p-value of the difference in reoffending rate by court outcome
  court_rate_pvalue_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                             court_rate_pvalue[i, ],
                                             MoreArgs = list(digits = 2,
                                                             p_value = TRUE))

  court_rate_CI_l[i, 1:3] <- temp_t_test_data["difference_lower", c("severe_ITS1_1y_reoff_excpnds",
                                                                    "severe_ITS2_1y_reoff_excpnds",
                                                                    "severe_ITS3_1y_reoff_excpnds")] * 100 # Lower confidence interval of the reoffending rate by court outcome
  court_rate_CI_u[i, 1:3] <- temp_t_test_data["difference_upper", c("severe_ITS1_1y_reoff_excpnds",
                                                                    "severe_ITS2_1y_reoff_excpnds",
                                                                    "severe_ITS3_1y_reoff_excpnds")] * 100 # Upper confidence interval of the reoffending rate by court outcome

  court_rate_CI_l_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                           court_rate_CI_l[i, ],
                                           MoreArgs = list(digits = 0,
                                                           allow_neg = TRUE))
  court_rate_CI_u_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                           court_rate_CI_u[i, ],
                                           MoreArgs = list(digits = 0,
                                                           allow_neg = TRUE))
  court_rate_est_diff[i, 1:3] <- paste0(ifelse(court_rate_CI_l_format[i, 1:3]!="-0",court_rate_CI_l_format[i, 1:3],"0"), " to ", ifelse(court_rate_CI_u_format[i, 1:3]!="-0",court_rate_CI_u_format[i, 1:3],"0"))

  treat_court_rate_numb[i, 1:3] <- temp_t_test_data["treat_number", c("severe_ITS1_1y_reoff_excpnds",
                                                                      "severe_ITS2_1y_reoff_excpnds",
                                                                      "severe_ITS3_1y_reoff_excpnds")] # Number of people in the treatment group
  treat_court_rate_numb_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                                 treat_court_rate_numb[i, ])

  comp_court_rate_numb[i, 1:3] <- temp_t_test_data["comparison_number", c("severe_ITS1_1y_reoff_excpnds",
                                                                       "severe_ITS2_1y_reoff_excpnds",
                                                                       "severe_ITS3_1y_reoff_excpnds")] # Number of people in the comparison group
  comp_court_rate_numb_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                                comp_court_rate_numb[i, ])
  
  treat_court_rate_table_val[i, 1:3] <- mapply(JDLpackage:::format_num, treat_court_rate[i, ],
                                                           MoreArgs=list(digits = 0))
  comp_court_rate_table_val[i, 1:3] <- mapply(JDLpackage:::format_num,comp_court_rate[i, ],
                                                          MoreArgs=list(digits = 0))

  ########################################################## Court outcome freq ##########################################################

  treat_court_freq[i, 1:3] <- temp_t_test_data["treat_mean", c("severe_ITS1_reoff_court_caution_1y",
                                                               "severe_ITS2_reoff_court_caution_1y",
                                                               "severe_ITS3_reoff_court_caution_1y")] * 100 # Reoffending frequency by court outcome for the treatment group
  treat_court_freq_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                            treat_court_freq[i, ],
                                            MoreArgs = list(digits = 1))
  comp_court_freq[i, 1:3] <- temp_t_test_data["comparison_mean", c("severe_ITS1_reoff_court_caution_1y",
                                                                "severe_ITS2_reoff_court_caution_1y",
                                                                "severe_ITS3_reoff_court_caution_1y")] * 100 # Reoffending frequency by court outcome for the comparison group
  comp_court_freq_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                           comp_court_freq[i, ],
                                           MoreArgs = list(digits = 1))

  court_freq_pvalue[i, 1:3] <- temp_t_test_data["difference_p", c("severe_ITS1_reoff_court_caution_1y",
                                                                  "severe_ITS2_reoff_court_caution_1y",
                                                                  "severe_ITS3_reoff_court_caution_1y")] # p-value of the difference in reoffending frequency by court outcome
  court_freq_pvalue_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                             court_freq_pvalue[i, ],
                                             MoreArgs = list(digits = 2,
                                                             p_value = TRUE))

  court_freq_CI_l[i, 1:3] <- temp_t_test_data["difference_lower", c("severe_ITS1_reoff_court_caution_1y",
                                                                    "severe_ITS2_reoff_court_caution_1y",
                                                                    "severe_ITS3_reoff_court_caution_1y")] * 100 # Lower confidence interval of the reoffending frequency by court outcome
  court_freq_CI_u[i, 1:3] <- temp_t_test_data["difference_upper", c("severe_ITS1_reoff_court_caution_1y",
                                                                    "severe_ITS2_reoff_court_caution_1y",
                                                                    "severe_ITS3_reoff_court_caution_1y")] * 100 # Upper confidence interval of the reoffending frequency by court outcome
  court_freq_CI_l_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                           court_freq_CI_l[i, ]/100,
                                           MoreArgs = list(digits = 2,
                                                           allow_neg = TRUE))
  court_freq_CI_u_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                           court_freq_CI_u[i, ]/100,
                                           MoreArgs = list(digits = 2,
                                                           allow_neg = TRUE))
  court_freq_est_diff[i, 1:3] <- paste0(ifelse(court_freq_CI_l_format[i, 1:3]!="-0.00",court_freq_CI_l_format[i, 1:3],"0.00"), " to ", ifelse(court_freq_CI_u_format[i, 1:3]!="-0.00",court_freq_CI_u_format[i, 1:3],"0.00"))

  treat_court_freq_numb[i, 1:3] <- temp_t_test_data["treat_number", c("severe_ITS1_reoff_court_caution_1y",
                                                                      "severe_ITS2_reoff_court_caution_1y",
                                                                      "severe_ITS3_reoff_court_caution_1y")] # Number of people in the treatment group
  treat_court_freq_numb_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                                 treat_court_freq_numb[i, ])
  comp_court_freq_numb[i, 1:3] <- temp_t_test_data["comparison_number", c("severe_ITS1_reoff_court_caution_1y",
                                                                       "severe_ITS2_reoff_court_caution_1y",
                                                                       "severe_ITS3_reoff_court_caution_1y")] # Number of people in the comparison group
  comp_court_freq_numb_format[i, 1:3] <- mapply(JDLpackage:::format_num,
                                                comp_court_freq_numb[i, ])
  treat_court_freq_table_val[i, 1:3] <- mapply(JDLpackage:::format_num, treat_court_freq[i, ]/100,
                                                           MoreArgs=list(digits = 2))
  comp_court_freq_table_val[i, 1:3] <- mapply(JDLpackage:::format_num, comp_court_freq[i, ]/100,
                                                          MoreArgs=list(digits = 2))

  ########################################################## Custody sentencing rate ##########################################################


  treat_custody_rate[i] <- temp_t_test_data["treat_mean", "custody_for_reoff_1y_excPNDs"] * 100 # Custody sentencing rate for the treatment group
  treat_custody_rate_format[i] <- JDLpackage:::format_num(treat_custody_rate[i],
                                                         digits = 1)

  comp_custody_rate[i] <- temp_t_test_data["comparison_mean", "custody_for_reoff_1y_excPNDs"] * 100 # Custody sentencing rate for the comparison group
  comp_custody_rate_format[i] <- JDLpackage:::format_num(comp_custody_rate[i],
                                                        digits = 1)

  custody_rate_pvalue[i] <- temp_t_test_data["difference_p", "custody_for_reoff_1y_excPNDs"] # p-value of the difference in custody sentencing rate
  custody_rate_pvalue_format[i] <- JDLpackage:::format_num(custody_rate_pvalue[i],
                                                          digits = 2,
                                                          p_value = TRUE)


  custody_rate_CI_l[i] <- temp_t_test_data["difference_lower", "custody_for_reoff_1y_excPNDs"] * 100 # Lower confidence interval of the custody sentencing rate
  custody_rate_CI_u[i] <- temp_t_test_data["difference_upper", "custody_for_reoff_1y_excPNDs"] * 100 # Upper confidence interval of the custody sentencing rate
  custody_rate_CI_l_format[i] <- JDLpackage:::format_num(custody_rate_CI_l[i],
                                                        digits = 0,
                                                        allow_neg = TRUE)
  custody_rate_CI_u_format[i] <- JDLpackage:::format_num(custody_rate_CI_u[i],
                                                        digits = 0,
                                                        allow_neg = TRUE)
  custody_rate_est_diff[i] <- paste0(if (custody_rate_CI_l_format[i]!="-0") custody_rate_CI_l_format[i] else "0", " to ", if (custody_rate_CI_u_format[i]!="-0") custody_rate_CI_u_format[i] else "0")

  treat_custody_rate_numb[i] <- temp_t_test_data["treat_number", "custody_for_reoff_1y_excPNDs"] # Number of people in the treatment group
  treat_custody_rate_numb_format[i] <- JDLpackage:::format_num(treat_custody_rate_numb[i])
  comp_custody_rate_numb[i] <- temp_t_test_data["comparison_number", "custody_for_reoff_1y_excPNDs"] # Number of people in the comparison group
  comp_custody_rate_numb_format[i] <- JDLpackage:::format_num(comp_custody_rate_numb[i])
  
  treat_custody_rate_table_val[i] <- JDLpackage:::format_num(treat_custody_rate[i],
                                                           digits = 0)
  comp_custody_rate_table_val[i] <- JDLpackage:::format_num(comp_custody_rate[i],
                                                          digits = 0)


  ########################################################## Custody sentencing frequency ##########################################################


  treat_custody_freq[i] <- temp_t_test_data["treat_mean", "freq_prison_for_reoff_1y_excpnds"] * 100 # Custody sentencing frequency for the treatment group
  treat_custody_freq_format[i] <-  JDLpackage:::format_num(treat_custody_freq[i],
                                                          digits = 1)
  comp_custody_freq[i] <- temp_t_test_data["comparison_mean", "freq_prison_for_reoff_1y_excpnds"] * 100 # Custody sentencing frequency for the treatment group
  comp_custody_freq_format[i] <-  JDLpackage:::format_num(comp_custody_freq[i],
                                                         digits = 1)

  custody_freq_pvalue[i] <- temp_t_test_data["difference_p", "freq_prison_for_reoff_1y_excpnds"] # p-value of the difference in custody sentencing frequency
  custody_freq_pvalue_format[i] <-  JDLpackage:::format_num(custody_freq_pvalue[i],
                                                           digits = 2,
                                                           p_value = TRUE)

  custody_freq_CI_l[i] <- temp_t_test_data["difference_lower", "freq_prison_for_reoff_1y_excpnds"] * 100 # Lower confidence interval of the custody sentencing frequency
  custody_freq_CI_u[i] <- temp_t_test_data["difference_upper", "freq_prison_for_reoff_1y_excpnds"] * 100 # Upper confidence interval of the custody sentencing frequency
  custody_freq_CI_l_format[i] <-  JDLpackage:::format_num(custody_freq_CI_l[i]/100,
                                                         digits = 2,
                                                         allow_neg = TRUE)
  custody_freq_CI_u_format[i] <- JDLpackage:::format_num(custody_freq_CI_u[i]/100,
                                                        digits = 2,
                                                        allow_neg = TRUE)
  custody_freq_est_diff[i] <- paste0(if (custody_freq_CI_l_format[i]!="-0.00") custody_freq_CI_l_format[i] else "0.00", " to ", if (custody_freq_CI_u_format[i]!="-0.00") custody_freq_CI_u_format[i] else "0.00")

  treat_custody_freq_numb[i] <- temp_t_test_data["treat_number", "freq_prison_for_reoff_1y_excpnds"] # Number of people in the treatment group
  treat_custody_freq_numb_format[i] <- JDLpackage:::format_num(treat_custody_freq_numb[i])
  comp_custody_freq_numb[i] <- temp_t_test_data["comparison_number", "freq_prison_for_reoff_1y_excpnds"] # Number of people in the comparison group
  comp_custody_freq_numb_format[i] <-  JDLpackage:::format_num(comp_custody_freq_numb[i])
  
  treat_custody_freq_table_val[i] <- JDLpackage:::format_num(treat_custody_freq[i]/100,
                                                           digits = 2)
  comp_custody_freq_table_val[i] <- JDLpackage:::format_num(comp_custody_freq[i]/100,
                                                          digits = 2)


  remove(temp_t_test_data)
}

############################### Rounding box appearance ###############################
# treat_reoff_rate_format
# comp_reoff_rate_format
# diff_reoff_rate_format

  
rounding_flag = sum(
  rounding_phrase(c(treat_reoff_rate_format, comp_reoff_rate_format), diff_reoff_rate_format, "Difference"),
  rounding_phrase(c(treat_reoff_freq_format, comp_reoff_freq_format), diff_reoff_freq_format, "Difference"),
  rounding_phrase(c(treat_time_format, comp_time_format), diff_time_format, "Difference"))
  
overall_rounding_phrase <- ifelse(rounding_flag == 0,
       "",
       "<b><div class = 'sig_results'>Please note totals may not appear to equal the sum of the component parts due to rounding.</div></b>")  
  
  


########################################################## Significant results ##########################################################

sig_indicator = list() # List to hold the information needed to create the bullet points
sig_vec <- list()
sig_list <- c()
sig_marker = 0
num_sig <- 0

for (i in 1: length(t_test_tabs)){ # iterating across each set of results in the list
   # Marker to indicate how many lines from the currently results have been printed
  
  tabname <- t_test_tabs[i]
  sig_indicator[[analyses_name[i]]] <-  data.frame(colnames(t_test_data[[i]]), stringsAsFactors = FALSE)
  colnames(sig_indicator[[i]]) = "vars"

  # Signals a 1 where the p-value is significant
  sig_indicator[[i]][ ,'ind'] <- as.vector(ifelse(t_test_data[[i]]['difference_p', ] <= 0.05 & treat_time_numb[i] >= 30, 1, 0))
  
  # Signals whether it is an increase or decrease using -1 or 1
  sig_indicator[[i]][ ,'size'] <- as.vector(ifelse(t_test_data[[i]]['difference_means', ] < 0, -1, 1))

  sig_indicator[[i]] %<>%
    filter(np_10_marker[[i]] == T) %>%
    filter(ind == 1) %>%
    left_join(sig_sentences_df, by = c("vars" = "Variable")) #Filters to leave those with an np greater than 10, p value less than or equal to 0.05, and then joins on the sentences related to each one
  
  if (nrow(sig_indicator[[i]]) == 0) next #breaks if the results for that group has no significant results

  for (j in 1:nrow(sig_indicator[[i]])){

    change_ind <- order(c(sig_indicator[[i]]$size[j], 0)) # orders 

    if (sig_indicator[[i]]$join_word[j] == 'less'){

      sig_indicator[[i]]$middle[j] <- JDLpackage:::more_or_less(change_ind[1], 
                                                               change_ind[2])

    } else if (sig_indicator[[i]]$join_word[j] == 'fewer'){

      sig_indicator[[i]]$middle[j] <- JDLpackage:::more_or_fewer(change_ind[1], 
                                                                change_ind[2])

    } else if (sig_indicator[[i]]$join_word[j] == 'earlier'){

      sig_indicator[[i]]$middle[j] <- JDLpackage:::earlier_or_later(change_ind[1], 
                                                                   change_ind[2])

    }
  }

  sig_indicator[[i]]$sentence <- stringr::str_c("<b>", # starts the sections where it is bold, (ends within half_2)
                                                sig_indicator[[i]]$half_1, 
                                                sig_indicator[[i]]$middle, 
                                                sig_indicator[[i]]$half_2, 
                                                sep = " ")

  sig_vec[[i]] <- (sig_indicator[[i]]$sentence)
  sig_marker <- sig_marker + ifelse(length(sig_vec[[i]]) == 0, 0, 1)
}


if (sig_marker > 0){ #doesn't run if there are no significant sentences
  
  sig_list <- stringr::str_c("")
  
  
  for (i in 1:length(sig_vec)){ # builds up the list of sentences to be published
      
    if (nrow(sig_indicator[[i]]) == 0) next #doesn't run loop if there are no significnat results
    
    sig_list %<>% stringr::str_c("<b>",
                                 names(sig_indicator)[i],
                                 "</b><br style = 'margin-bottom: 0px;'>",
                                 "<ul style = 'padding-bottom: 15px;'>")
    
    for (j in 1:length(sig_vec[[i]])){
      
      sig_list %<>% stringr::str_c("<li style = 'padding-top: 10px;'>",
                                   sig_vec[[i]][j],
                                   "<br></li>")
      
      num_sig <- num_sig + 1
    }
    
    sig_list %<>% stringr::str_c("</ul>")
  }
  
  sig_list %<>% stringr::str_c("</div>")
}

########################################################## Treatment group profile ####################################################################

# Participants included in analysis
# (XXXX offenders - national analysis)
# • Gender statistics
# • Ethnicity statistics
# • UK national statistics

# • Age statistics

# • List of other statistics that should be included



profile_joined <- left_join(x = variable_lookup,
                            y = std_diffs[[headline_analysis]],
                            by = c("variables" = "X1"))

section_names <- unique(variable_lookup$section)

###Age
mean_age <- paste0("(average age ",
                   round(profile_joined$weighted_means_treat[profile_joined$variables == "age_index"]),
                   ")")

age_sentence <- str_c("Aged ",
                      profile_joined$name[profile_joined$variables=='min_age'],
                      " to ",
                      profile_joined$name[profile_joined$variables=='max_age'],
                      " years at the beginning of their one-year period ",
                      mean_age
)

#extracting and formatting the treatment group means
profile_results <- cbind.data.frame(section = profile_joined$section,
                                    names = profile_joined$names,
                                    perc = round(profile_joined$weighted_means_treat*100),
                                    stringsAsFactors = FALSE) %>%
  subset(section != 'Age')


#makes the percentages phrase for the bullet points
profile_perc <- cbind.data.frame(section = profile_results$section,
                                 perc = paste0(profile_results$names,
                                               " ",
                                               round(profile_results$perc),
                                               "%"),
                                 stringsAsFactors = FALSE)

#combines and pastes together the percentage phrase for each section and puts it together in a vector eg "Male 24%, Female 76%"
profile_line <- c()
for (i in section_names){
  profile_line[i] <- paste(subset(profile_perc,
                                  profile_perc$section == i)$perc,
                           collapse = ", ")
}

sex_sentence <- profile_line["Sex"]
ethnicity_sentence <- profile_line["Ethnicity"]
uk_sentence <- profile_line["UK national"]




### Remaining categories
# Selects only the remaining sentneces that haven't been used yet
optional_sections <- subset(profile_perc, !(section %in% c("Sex",
                                                           "Ethnicity",
                                                           "UK national"))
                            )

opt_section_names <- unique(optional_sections$section)





opt_perc <- cbind.data.frame(section = profile_results$section,
                                  perc = paste0(profile_results$names,
                                                " <span style = 'float: right;'>",
                                                round(profile_results$perc),
                                                "%</span>"),
                                  stringsAsFactors = FALSE) %>% 
  subset(section %in% opt_section_names)  # creates a dataframe with the sections and the lines in that section to be printed

opt_list <- ""

if (nrow(opt_perc) != 0) {
  
  for (i in opt_section_names){
    
    temp_list <- paste0("<li style=\"margin-left:-26px;\">",
                       subset(opt_perc,
                              section == i)[,2], 
                       "</li>") %>%
      paste(collapse = "")
    
    opt_list %<>% stringr::str_c("<br>",
                               "<li>",
                               i,":</li>",
                               "<ul>",
                               temp_list,
                               "</ul>"
                               )
  }
  
}



profile_flag = 0

section_names_100 <- section_names[section_names != 'Age']

for (i in 1:length(section_names_100)){
  
  summation = profile_results %>%
    filter(section == section_names_100[i] ) %>%
    select(perc) %>%
    sum()
  
  if(summation != 100) profile_flag = profile_flag + 1
  
}

profile_rounding <- ifelse(profile_flag ==0,
                           "",
                           "<b><div class = 'sig_results'>Please note totals may not appear to equal the sum of the component parts due to rounding.</div></b>")  

############################## List of OASys variables used in analysis

std_diffs_names <- grep("^S[0-9]{1,2}Q.?", std_diffs[[headline_analysis]][ ,1], value = TRUE)

oasys_id <- gsub("_.*$", '', std_diffs_names) %>% unique

oasys_id <- ifelse(gsub("Q.*$", '', oasys_id) == 'S4', oasys_id, gsub("Q.*$", '', oasys_id)) %>% 
  unique %>%
  as.data.frame

colnames(oasys_id) = 'ID'

oasys_groups <- left_join(oasys_id, oasys_vars, 'ID') %>%
  select(oasys_group) %>%
  unique

oasys_list <- collapse(oasys_groups[ ,1], ', ', last = ' and ')



#################### List of the OASys veriables that were significatn and their percentage ############

some_sig_oasys_list <- na.omit(paragraphs$part2_some_sig_oasys_list)

some_sig_oasys <- "<ul><li style = 'margin-bottom:5px;'>"

some_sig_oasys %<>% stringr::str_c(stringr::str_c(some_sig_oasys_list,  collapse = "</li><li style = margin-bottom:5px;'>"))

some_sig_oasys %<>% stringr::str_c(stringr::str_c("</li></ul>"))

#################### List describing the matching of the variables ##################

oasys_matched_list <- na.omit(paragraphs$part2_oasys_matched_list)

oasys_matched <- "<ul><li style = 'margin-bottom:5px;'>"

oasys_matched %<>% stringr::str_c(stringr::str_c(oasys_matched_list,  collapse = "</li><li style = margin-bottom:5px;'>"))

oasys_matched %<>% stringr::str_c(stringr::str_c("</li></ul>"))
