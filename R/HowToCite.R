#' Show citation info
#'
#' @param ... Options to pass to cat()
#'
#' @export
HowToCite <- function(...){
  cat(paste('To cite this app in any publication please cite the app and the package "blockmodeling" as follows, plus (at least) one of the articles below:\n',
            'This app/package:',
            '\t - Telarico, Fabio Ashtar, and Aleš Žiberna. GUI for the Generalised Blockmodeling of Valued Networks (version 1.8.3). R. Ljubljana (Slovenia): Faculty of Social Sciences (FDV) at the University of Ljubljana, 2022. https://doi.org/10.5281/zenodo.6554608.\n',
            'Package "blockmodeling" by Aleš Žiberna:',
            '\t - Žiberna, Aleš. Blockmodeling: Generalized and Classical Blockmodeling of Valued Networks (version 1.0.5), 2021. https://CRAN.R-project.org/package=blockmodeling.\n',
            'Articles:',
            '\t - Doreian, Patrick, Vladimir Batagelj, and Anuska Ferligoj. Generalized Blockmodeling. Cambridge University Press, 2005.',
            '\t - Matjašič, Miha, Marjan Cugmas, and Aleš Žiberna. ‘Blockmodeling: An R Package for Generalized Blockmodeling’. Preprint. SocArXiv, 8 June 2021. https://doi.org/10.31235/osf.io/b8cxp.',
            '\t - Žiberna, Aleš. ‘Generalized Blockmodeling of Sparse Networks’. Advances in Methodology and Statistics 10, no. 2 (1 July 2013). https://doi.org/10.51936/orxk5673.',
            '\t - Žiberna, Aleš. ‘Generalized Blockmodeling of Valued Networks’. Social Networks 29, no. 1 (January 2007): 105–26. https://doi.org/10.1016/j.socnet.2006.04.002.',sep = '\n'
  ))
}
