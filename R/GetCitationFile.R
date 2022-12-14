#' Get citation files
#'
#' @param filetype Type of bibliographic file can be either \code{'ris'} for RIS files of \code{'BibText'} for BibText files.
#' @param folder Folder where to save the file. Defaults to \code{'getwd()'}.
#' @param overwrite Boolean. Whether to overwrite namesake files in the target folder.
#'
#' @return Copies the selected type of bibliographic file to the working folder.
#' @export
#' @examples
#' GetCitationFile('ris') # to get the .ris file in the working folder



GetCitationFile <- function(filetype=c('ris','BibText'),folder=getwd(),overwrite=F) {
  if(filetype!='ris'&&filetype!='BibText'){
    stop('File type not selected correctly. Select one of "ris" or "BibText"!')
  } else {
    if(filetype=='ris'){
      file.copy(from = './inst/extdata/citations.ris',to = folder,
                overwrite = overwrite)
    } else if(filetype=='BibText'){
      file.copy(from = './inst/extdata/citations.bib',to = folder,
                overwrite = overwrite)
    }
    cat(paste('File', filetype, 'saved correctly in', folder,sep = ' '))
  }
}
