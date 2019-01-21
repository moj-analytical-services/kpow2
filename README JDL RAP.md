# JDL_RAP

The Reproducible Analytical Pipeline for producing the Justice Data Lab's quarterly reports.

*Copying the GitHub repository:*

1. Go to the [JDL-reports](https://github.com/moj-analytical-services/JDL-reports) repo and follow the steps listed in the README there


*To run:*  

1.	Update the Excel document with metadata, key paragraphs and results from your analysis, and upload it to the Platform
2.	Open the `run.R` file and run the `Installing packages` and `Installing Arial` sections (up line 43) 
3.	Moving the graph's confidence intervals:
i.	Then run the `Moving confidence intervals` sections (lines 46-50) - this should open the HTML in your web browser.
ii.	Then open the `Graphs.Rmd` file and change the numbers on lines 23 to 34 to position the labels, save the file, and run the previous step again until you are satisfied with the outcome.
4.	Then create the HTML outputs using the `run.R` file (lines 40 to 54)
5.	Then for each of the outputs, select print preview, check that the orientation of the paper is correct 
6.	Using the previews, update table of content page numbers in the Excel file, and rerun steps 4 and 5
7.	Print the HTML files, selecting `Microsoft Print to PDF`, and then save wherever is convenient on your computer, with the file names `JDL part 1.pdf`, `Graphs.pdf` and `JDL part 2.pdf`
8.	Upload the PDFs into the PDFs folder
9.	Run the last line in the `run.R` file and that will create the final PDF of the report in the main folder.

### Where things are created

<table>
<tr><th>Decriptions</th><th>Variable name</th><th>File names</th></tr>
<tr><td>Overall measurement table </td><td> <code>meas_table</code></td> <td rowspan = "13"><code>Scripts/tables.R</code></td></tr>
<tr><td>Overall estimate table</td><td> <code>est_table</code></td></tr>
<tr><td>Table of contents</td><td><code>table_of_contents</code></td></tr>
<tr><td>Overview of analysis table (showing the region that is being matched to) </td><td><code>analyses_overview_table</code> </td></tr>
<tr><td>Analyses detail table (number in comparison and treatment groups)</td><td><code>analysis_detail_table</code> </td></tr>
<tr><td>Reoffending rate table </td><td><code>reoff_rate_table</code> </td></tr>
<tr><td>Reoffending frequency table </td><td><code>reoff_freq_table</code> </td></tr>
<tr><td>Time to reoffence table </td><td><code>time_table</code> </td></tr>
<tr><td>Court outcome rate table </td><td><code>court_rate_table</code> </td></tr>
<tr><td>Court outcome frequency table</td><td> <code>court_freq_table</code> </td></tr>
<tr><td>Custody rate table </td><td><code>custody_rate_table</code> </td></tr>
<tr><td>Custody frequency table </td><td><code>custody_freq_table</code> </td></tr>
<tr><td>Profile of the treatment group (<code>sex_sentence</code>, <code>ethnicity_sentence</code>, <code>age_sentence</code> and <code>opt_list</code> all created in <code>Scripts/Variables.R</code>)</td><td> <code>treat_table </code> </td></tr>
<tr><td colspan = "3">&nbsp;</td></tr>

<tr><td> Significant results</td><td> <code>sig_list<code></td><td rowspan = "5"><code>Scripts/variables.R</code></td></tr>
<tr><td> Box that indicates round may be needed </td><td> <code>profile_rounding</code></td></tr>
<tr><td> List of OASys groups that were used in the analysis </td><td> <code>oasys_list</code></td></tr>
<tr><td> List of OASys variables that had some significance </td><td> <code>some_sig_oasys</code></td></tr>
<tr><td> List decribing how the OASys were matched </td><td> <code>oasys_matched</code></td></tr>
</table>
