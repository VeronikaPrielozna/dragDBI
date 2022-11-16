#' Uploading function for user-defined DBI checklist
#' @description The function allows for uploading a user-defined checklist of odonates with a user-defined set of DBI values.
#' @usage LoadDBI()
#' @value A data frame uploaded by user containing a list of taxa in the first column and user-defined DBI values in the second column.
#' @examples
#' # Uploading user-defined DBI checklist. Saved as "DBI_UD".
#'
#' DBI_UD<-LoadDBI()


LoadDBI<-function(){
  print("Copy data into clipboard")
  invisible(readline(prompt="Press [enter] to continue"))
  tryCatch(silent=T,
           expr = {
             DBI_UD<-read.table("clipboard", h=T, as.is=T, sep = "\t")
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
             cat(paste("Your data: ","\n"))
           }
  )
  print(DBI_UD)
  DBI_UD
}
