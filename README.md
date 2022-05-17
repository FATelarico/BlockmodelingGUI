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

## The app supports different types of inputs:

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
-- Building ---------- BlockmodelingGUI --
Setting env vars:
* CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
* CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
* CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
* CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
* CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
* CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
------------------------------------------
v  checking for file 'F:\Testi\Scuola\MA\2021 Ljubljana\Work\02. App\BlockmodelingGUI/DESCRIPTION'
-  preparing 'BlockmodelingGUI':
v  checking DESCRIPTION meta-information ... 
-  checking for LF line-endings in source and make files and shell scripts
-  checking for empty or unneeded directories
     NB: this package now depends on R (>= 3.5.0)
     WARNING: Added dependency on R >= 3.5.0 because serialized objects in
     serialize/load version 3 cannot be read in older versions of R.
     File(s) containing such objects:
       'BlockmodelingGUI/inst/apps/Sample.rds'
-  building 'BlockmodelingGUI_1.8.3.0000.tar.gz'
   
-- Checking ---------- BlockmodelingGUI --
Setting env vars:
* _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
* _R_CHECK_CRAN_INCOMING_       : FALSE
* _R_CHECK_FORCE_SUGGESTS_      : FALSE
* NOT_CRAN                      : true
-- R CMD check ---------------------------
-  using log directory 'F:/Testi/Scuola/MA/2021 Ljubljana/Work/02. App/BlockmodelingGUI.Rcheck' (360ms)
-  using R version 4.1.2 (2021-11-01)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: ISO8859-1
-  using options '--no-manual --as-cran'
v  checking for file 'BlockmodelingGUI/DESCRIPTION' ... 
-  this is package 'BlockmodelingGUI' version '1.8.3.0000'
-  package encoding: UTF-8
v  checking package namespace information ... 
v  checking package dependencies (3s)
v  checking if this is a source package ...
v  checking if there is a namespace
v  checking for executable files (519ms)
v  checking for hidden files and directories ... 
v  checking for portable file names ... 
v  checking whether package 'BlockmodelingGUI' can be installed (2.6s)
v  checking installed package size ... 
v  checking package directory
v  checking for future file timestamps ... 
v  checking DESCRIPTION meta-information (426ms)
v  checking top-level files ...
v  checking for left-over files
v  checking index information
v  checking package subdirectories ... 
v  checking R files for non-ASCII characters ... 
v  checking R files for syntax errors ... 
v  checking whether the package can be loaded ... 
v  checking whether the package can be loaded with stated dependencies ... 
v  checking whether the package can be unloaded cleanly ... 
v  checking whether the namespace can be loaded with stated dependencies ... 
v  checking whether the namespace can be unloaded cleanly (412ms)
v  checking loading without being on the library search path (475ms)
v  checking dependencies in R code (811ms)
v  checking S3 generic/method consistency (712ms)
v  checking replacement functions ... 
v  checking foreign function calls ... 
v  checking R code for possible problems (3.4s)
v  checking Rd files ... 
v  checking Rd metadata ... 
v  checking Rd line widths ... 
v  checking Rd cross-references ... 
v  checking for missing documentation entries ... 
v  checking for code/documentation mismatches (718ms)
v  checking Rd \usage sections (917ms)
v  checking Rd contents ... 
v  checking for unstated dependencies in examples ... 
-  checking examples ... NONE
v  checking for non-standard things in the check directory
v  checking for detritus in the temp directory
   
   
 R CMD check results  BlockmodelingG
Duration: 19.5s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
