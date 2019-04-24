

# courtstats

## Contents
* [What is this repo for?](#what-is-this-repo-for)
* [Using the package](#using-the-package)
* [Package infrastructure](#package-infrastructure)

## What is this repo for?

This repo contains code for an R package to help generate the latest statistics on courts in England and Wales

This package is predominanty used to do all the heavy lifting for the Criminal Court Statistics Quarterly [RAP](https://github.com/moj-analytical-services/CCSQ_RAP)

## Using the package

If you just want to make use of the package, do the following:

* Create a Github [Public Access Token (PAT)](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
* Assign this value to a variable `GITHUB_PAT`
* Run `devtools::install_github("moj-analytical-services/courtstats", auth_token = GITHUB_PAT)`
* Load the package using `library(courtstats)`

You will now have access to all the functions in the library, using the following syntax:

Example: to use FUNCTION from the asdrap package, type `courtstats::FUNCTION()`

## Package Infrastructure
A whistle stop tour of the package for those who aren't as familiar with R/package development 

### R/
This is where most of the code which does the analysis lives. All the code is functions, only some of which are avaiable when you install the package.

### man/
This is where the package documentation that can usually be found running ?function_name 

### tests/
The code here simply tests the functions in the package are working as expected. None of this code does any of the analysis the package was designed for, instead acting as quality assurance for the functions that have been created to do the analysis.

### README.md
Contains the text for this document in Markdown format. This is the sole documentation for the package, other than occasional comments in the source code. The source code should be transparent enough that it is best to read the source code to understand how functions in the package work.

### NAMESPACE
Is a config file that should not be updated manually - instead use roxygen comments in the code. http://r-pkgs.had.co.nz/namespace.html

### DESCRIPTION
Is a config file. Most fields are self explanatory. http://r-pkgs.had.co.nz/description.html
