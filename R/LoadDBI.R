#' Uploading function for user-defined DBI checklist
#'
#' @param attrib
#'
#' @return
#' @export
#'
#' @examples
#' # Uploading user-defind DBI checklist. Saved as DBI_MD.
#'
#' DBI_MD<-LoadDBI()

LoadDBI<-function(attrib=T){
  print("Copy data into clipboard")
  invisible(readline(prompt="Press [enter] to continue"))
  tryCatch(silent=T,
           expr = {
             DBI_MD<-read.table("clipboard", h=T, as.is=T, sep = "\t")
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
             if (attrib==T){
               attr(DBI_MD, "Number of species")<-nrow(DBI_MD)
               print("Attributes of table")
               print(attributes(DBI_MD))
             }
             cat(paste("Your data:"))
             cat("\n")
             print(DBI_MD)
             message("You can continue.")
           }
  )
  DBI_MD
}
