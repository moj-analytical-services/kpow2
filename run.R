## The code to run in order to create the Knife Possession RAP report

###################### Installing packages ##############################

#Install packages used to merge PDFs
library("devtools")
library("dplyr")

#Upload excel file per each publication 
excel_file <- "knifecrime.xlsx"

#Install JDL package
github_pat_generated <- openxlsx::readWorkbook(excel_file,
                                               sheet = "Report information")$github_code %>% na.omit

devtools::install_github("moj-analytical-services/Knife-Crime", auth_token = github_pat_generated)

devtools::install_github("ropensci/tabulizerjars")
devtools::install_github("ropensci/tabulizer")

packages <- c(
  "markdown",
  "ggplot2",
  "openxlsx",
  "png",
  "dplyr",
  "magrittr",
  "stringr",
  "s3tools",
  "devtools"
)

sapply(packages,
       install.packages
)


require('rmarkdown')


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Sets working directory to match current file
############################ Installing Arial ###########################
#
#mainDir <- "~"
#subDir <- ".fonts"

#dir.create(file.path(mainDir, subDir))
#file.copy(from = 'arial.ttf',
#          to = file.path(mainDir, subDir))


###################### Creating HTML outputs ######################
rmarkdown_files <- c("Knife Crime.Rmd")

html_files <- c("HTML output/Knife Crime.html")

lapply(rmarkdown_files,
       render, output_format="html_document",
       output_dir = "HTML output/")

lapply(html_files, 
       browseURL)


# Save the above document as a PDF onto your DOM1 machine (use print preview option in your browser) 
#  with the same name, reupload the pdfs to the 'PDFs' folder
# !! Make sure you print preview first to get the page orientation correct

# To do just one at a time 
rmarkdown::render("Knife Crime.Rmd", output_format="html_document",
                  output_dir = "HTML output/")
browseURL("HTML output/Knife Crime.html")


###################### Merging the PDFs into one file ######################
#tabulizer::merge_pdfs(c("PDFs/JDL part 1.pdf", "PDFs/Graphs.pdf", "PDFs/JDL part 2.pdf"), "PDFs/JDL report.pdf")
