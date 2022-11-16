#' Data uploading function
#'
#' @description The function allows for simple loading of user data and converting NA values into null.
#' @usage LoadData()
#' @returns  A data frame uploaded by the user containing a list of taxa in the first column, and abundance or presence/absence data in following columns, with sample names in the column header.
#' @examples
#' Uploading data containing species of Central Europe. Saved as "Europe".
#'
#'Europe<-LoadData()
#'
#' Uploading data containing species of South Africa. Saved as "Africa".
#'
#'Africa<-LoadData()

LoadData<-function(){
  cat(paste("Copy data into clipboard"))
  cat("\n")
  invisible(readline(prompt="Press [enter] to continue"))
  tryCatch(silent=T,
           expr = {
             x<-read.table("clipboard", h=T, as.is=T, sep = "\t")
             message("Dataset successfully uploaded.")
           },
           error = function(e){
             message("Dataset inaccurately uploaded, Try again.")
             print(e)
           },
           warning = function(w){
             message("Warning! Dataset inaccurately uploaded. Try again.")
             print(w)
           },
           finally = {
             x[is.na(x)]<-0
             cat(paste("Number of species: ",nrow(x),"\n"))
             cat(paste("Number of samples: ", ncol(x)-1,"\n"))
          }
  )
  print(x)
  x
}
