#' Uploading function for user-defined DBI checklist
#'
#' @description The function allows for uploading a user-defined checklist of odonates with a
#' user-defined set of DBI values.
#' @usage LoadDBI()
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#'
#' @returns A data frame uploaded by user containing a list of taxa in the first column and
#' user-defined DBI values in the second column.
#' @examples
#'
#' \dontrun{
#' # Uploading user-defined DBI checklist. Saved as "DBI_UD".
#'
#' DBI_UD <- LoadDBI()
#' }
#'
#' @export LoadDBI


LoadDBI<-function(){
  cat("Copy data into clipboard.\n")
  message("If you use your own DBI values, please feel free to share them via email (verca.prielozna@gmail.com) to help improve the package.")
  invisible(readline(prompt="Press [enter] to continue"))
  tryCatch(silent=T,
           expr = {
             DBI_UD<-read.table("clipboard", header = TRUE, as.is = TRUE, sep = "\t")
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
             colnames(DBI_UD)<-c("Species", "DBI")
             cat(paste("Number of species:",nrow(DBI_UD),"\n"))
           }
  )
  return(DBI_UD)
}
