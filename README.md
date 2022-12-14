# BlockmodelingGUI
Sister project: *[A GUI for Generalised Blockmodeling](https://github.com/FATelarico/GUI-Generalised-Blockmodeling)*
![image](https://user-images.githubusercontent.com/100512813/158572361-844a64ee-5784-4c82-a164-bf9bdcc917c0.png)

## Short Description
This R package provides some useful tools for Offering an accessible GUI for generalised blockmodeling of single-relation, one-mode networks. The user can execute blockmodeling without having to write a line code by using the app's visual helps. Moreover, there are several ways to visualisations networks and their partitions. Finally, the results can be exported as if they were produced by writing code.

The app’s many option and east of use make blockmodeling and basic network analysis in R easier to execute and understand than via command line. In fact, the GUI requires no coding experience, allowing researchers and laymen to access visualisations and information about blockmodeling in a simple way. The breadth of network analyses than can be executed is arguably not too extended, but the focus when it comes to computation is here on blockmodeling. Thus making, the app could find an apt use in a graduate class in quantitative methods in social science as well.

## Installation
To install the GUI directly from R, run the following code:
1. Check if you need to install the `devtools` package
  `if(!require(devtools))install.packages('devtools')` 
2. Install the package from this repository
  `devtools::install_github('FATelarico/BlockmodelingGUI')`
  
## Acknowledgements
![image](https://www.arrs.si/lib/img/arrs-logo-en.gif)

The development of this package is financially supported by the Slovenian Research Agency (www.arrs.gov.si) within the research project J5-2557 (Comparison and evaluation of different approaches to blockmodeling dynamic networks by simulations with application to Slovenian co-authorship networks).

## Functions in detail

### The app supports different types of inputs:

![image](https://user-images.githubusercontent.com/100512813/159125221-be31c181-a0bb-4399-b410-16f45cb9cfc9.png)


|                  |*File type*|*Extension*      |*R function*                  |*Package*          |
|:-----------------|:----------|:----------------|:-----------------------------|:------------------|
|Adjacency matrix  |plain/text |.txt; .csv; .tab |``read.delim()``              |``base``           |
|Edge list         |plain/text |.txt; .csv; .tab |``read.delim()``              |``base``           |
|Incidence matrix  |plain/text |.txt; .csv; .tab |``read.delim()``              |``base``           |
|Pajek matrix      |Pajek      |.mat             |``loadmatrix()``              |``blockmodelling`` |
|Pajek network     |Pajek      |.net             |``loadmatrix()``              |``blockmodelling`` |

### Advanced inputs

|                  |*File type*|*Extension*      |*R function*                  |*Package*          |
|:-----------------|:----------|:----------------|:-----------------------------|:------------------|
|Clustering result |R          |.RDS             |``readRDS()``                 |``base``           |
|Preset block-model|R          |.RDS; .RData     |``readRDS()``; ``load()``     |``base``           |

## RCMD Check
==> devtools::check()

i Updating BlockmodelingGUI documentation
i Loading BlockmodelingGUI
── Checking ───────────────────────────────────────────── BlockmodelingGUI ──
Setting env vars:
• _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
── R CMD check ──────────────────────────────────────────────────────────────
─  using log directory ‘/tmp/RtmpaU8PvL/BlockmodelingGUI.Rcheck’
─  using R version 4.2.2 Patched (2022-11-10 r83330)
─  using platform: x86_64-pc-linux-gnu (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✔  checking for file ‘BlockmodelingGUI/DESCRIPTION’
─  this is package ‘BlockmodelingGUI’ version ‘1.8.4’
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies (2.2s)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files ...
✔  checking for hidden files and directories ...
✔  checking for portable file names
✔  checking for sufficient/correct file permissions
✔  checking whether package ‘BlockmodelingGUI’ can be installed (6.6s)
✔  checking installed package size ...
✔  checking package directory
✔  checking for future file timestamps ...
✔  checking DESCRIPTION meta-information ...
✔  checking top-level files ...
✔  checking for left-over files
✔  checking index information
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (2.1s)
✔  checking whether the package can be loaded with stated dependencies (1.8s)
✔  checking whether the package can be unloaded cleanly (2s)
✔  checking whether the namespace can be loaded with stated dependencies (2s)
✔  checking whether the namespace can be unloaded cleanly (2.2s)
✔  checking loading without being on the library search path (2.2s)
✔  checking dependencies in R code (1.9s)
✔  checking S3 generic/method consistency (3s)
✔  checking replacement functions (2s)
✔  checking foreign function calls (2s)
✔  checking R code for possible problems (9.2s)
✔  checking Rd files ...
✔  checking Rd metadata ...
✔  checking Rd line widths ...
✔  checking Rd cross-references ...
✔  checking for missing documentation entries (2s)
✔  checking for code/documentation mismatches (5.8s)
✔  checking Rd \usage sections (2.9s)
✔  checking Rd contents ...
✔  checking for unstated dependencies in examples ...
✔  checking examples (2.9s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ────────────────────────── BlockmodelingGUI 1.8.4 ────
Duration: 55.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
