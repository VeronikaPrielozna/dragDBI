#' Data uploading function
#'
#' @description The function allows for simple loading of user data and converting NA values into
#' null.
#' @usage LoadData()
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#'
#' @returns A data frame uploaded by the user containing a list of taxa in the first column, and
#' abundance or presence/absence data in following columns, with sample names in the column header.
#' @examples
#' # Uploading data containing species of Czech Republic. Saved as "Czech".
#'
#' \dontrun{
#' Czech <- LoadData()
#'
#' # Uploading data containing species of South Africa. Saved as "Africa".
#'
#' Africa <- LoadData()
#' }
#'
#' @export LoadData


LoadData<-function(){
  cat(paste("Copy data into clipboard"))
  cat("\n")
  message("Warning! Species must be in rows and samples in columns!")
  invisible(readline(prompt="Press [enter] to continue"))
  tryCatch(silent=T,
           expr = {
             x<-read.table("clipboard", header = TRUE, as.is = TRUE, sep = "\t")
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
  return(x)
}
