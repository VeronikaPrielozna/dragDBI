#' Data uploading function
#'
#' @param attrib
#' @param na2null
#'
#' @return
#' @export
#'
#' @examples
#' # Uploading data containing species of Central Europe. Saved as x_CE.
#'
#' x_CE<-LoadData()
#'
#'
#' # Uploading data containing species of South Africa. Saved as x_SA.
#'
#' x_SA<-LoadData()

LoadData<-function(attrib=T, na2null=T){

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
             message("You can continue.")
           }
  )

  if(na2null==T) x[is.na(x)]<-0
  attr(x, "Number of species")<-nrow(x)
  attr(x, "Number of samples")<-ncol(x)-1
  if (attrib==T){
    print("Attributes of table")
    print(attributes(x))
  }
  x
}
