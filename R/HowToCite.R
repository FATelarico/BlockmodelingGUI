#' Show citation info
#'
#' @param ... Options to pass to cat()
#' @return Returns information on how to cite the app.
#' @export
#' @examples
#' HowToCite() # to see information about citations

HowToCite <- function(...){
  cat(paste('To cite this app in any publication please cite the app and the package "blockmodeling" as follows, plus (at least) one of the articles below:\n',
            'This app/package:',
            '\t - Telarico, Fabio Ashtar, and Ale\u0161 \u017Diberna. GUI for the Generalised Blockmodeling of Valued Networks (version 1.8.3). R. Ljubljana (Slovenia): Faculty of Social Sciences (FDV) at the University of Ljubljana, 2022. https://doi.org/10.5281/zenodo.6554608.\n',
            'Package "blockmodeling" by Ale\u0161 \u017Diberna:',
            '\t - \u017Diberna, Ale\u0161. Blockmodeling: Generalized and Classical Blockmodeling of Valued Networks (version 1.0.5), 2021. https://CRAN.R-project.org/package=blockmodeling.',
            '\t - Matja\u0161i\u010D, Miha, Marjan Cugmas, and Ale\u0161 \u017Diberna. \'Blockmodeling: An R Package for Generalized Blockmodeling\'. Advances in Methodology and Statistics 17, no. 2 (1 July 2020): 49\u201366. https://doi.org/10.51936/uhir1119.\n',
            'Methods:',
            '\t - Doreian, Patrick, Vladimir Batagelj, and Anuska Ferligoj. Generalized Blockmodeling. Cambridge University Press, 2005.',
            '\t - \u017Diberna, Ale\u0161. \'Generalized Blockmodeling of Sparse Networks\'. Advances in Methodology and Statistics 10, no. 2 (1 July 2013). https://doi.org/10.51936/orxk5673.',
            '\t - \u017Diberna, Ale\u0161. \'Generalized Blockmodeling of Valued Networks\'. Social Networks 29, no. 1 (January 2007): 105\u201326. https://doi.org/10.1016/j.socnet.2006.04.002.',sep = '\n'
  ))
}
