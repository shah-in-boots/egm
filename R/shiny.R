#' R-based Waveform and Annotation Viewer and Editor
#'
#' The `rWAVE()` function calls a `{shiny}` application that is built for the
#' evaluation of WFDB objects and their corresponding annotations.
#'
#' @returns There is nothing returned directly from calling this function. It invokes the [shiny::runApp()] function to call the built-in `rWAVE` viewer.
#' @import shiny
#' @export
rWAVE <- function() {

	appDir <- system.file("shiny-apps", "rWAVE", package = "EGM")
	if (appDir == "") {
		stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
	}
	shiny::runApp(appDir, display.mode = "normal")

}
