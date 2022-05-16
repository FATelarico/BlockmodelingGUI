#' Get citation files
#'
#' @param filetype Type of blibliographic file
#'
#' @export
#'
GetCitationFile <- function(filetype=c('ris','BibText')) {
  if(filetype=='ris'){
    file.copy(from = './R/citations.ris',to = getwd(),
              overwrite =
                askYesNo('Should the command overwrite existing files?',T))
    cat('RIS file saved in the working directory')
  } else if(filetype=='BibText'){
    file.copy(from = './R/citations.bib',to = getwd(),
              overwrite =
                askYesNo('Should the command overwrite existing files?',T))
    cat('BibText file saved in the working directory')
  } else {errorCondition('File type not selected correctly. Select one of "ris" or "BibText"!')}
}
