design <- function(){
  appDir <- system.file("Design", package="modest")
  if(appDir==""){
    stop("Could not find example directory. Try re-installing `modest`.", call.=FALSE)
  }
  shiny::runApp(appDir, launch.browser=TRUE, display.mode="normal")
}