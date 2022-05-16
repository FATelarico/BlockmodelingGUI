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
