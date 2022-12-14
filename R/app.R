#' Launch Shiny App
#'
#' @param ... arguments to pass to shiny::runApp
#' @return Runs the app.
#' 
#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import blockmodeling
#' @import htmlwidgets
#' @import shinythemes

#' @export
#' @examples
#' \dontrun{
#' app() # to run the app without special options
#' app(launch.browser = T) # to run the app in a new browser session
#' }

app <- function(...) {
  appDir <- system.file(paste0('apps/BlockmodelingGUI.R'), package = 'BlockmodelingGUI')
  if (appDir == '') stop('Could not find the app. Try re-installing BlockmodelingGUI!', call. = FALSE)
  shiny::runApp(appDir, ...)
}
