#' Unification of users’ dataset and DBI data
#'
#' @param df
#' @param DBI_val
#' @param DBI_UD
#'
#' @return
#' @export
#'
#' @examples
#' # Unification of Highway stormwater and control ponds dataset and Central European checklist with DBI values. Saved as DBI_Data.
#'
#' DBI_Data<-UniteData(Data, DBI_val = "CE")

UniteData<-function(df, DBI_val, DBI_UD){
  table_user<-df
  table_user[,1]<-gsub("[_]", " ", table_user[,1])

  if (DBI_val=="CE"){
    table_package<-DBI_CE
  }

  if(DBI_val=="SA"){
    table_package<-DBI_SA
  }

  if(DBI_val=="UD"){
    table_package<-DBI_UD
    table_spec<-table_package[,1]
    COLname<-c("Species", "DBI")
    COLnum1<-0
    COLnum2<-2
  }

  if(DBI_val!="UD"){
    table_spec<-table_package$Species
    COLname<-c("Distribution", "Threat", "Sensitivity", "TOTAL")
    COLnum1<-3
    COLnum2<-4
  }


  for (j in 1:nrow(table_user)){
    if (substr(table_user[j,1], nchar(table_user[j,1]), nchar(table_user[j,1]))==" "){
      table_user[j,1]<-substring(table_user[j,1],1, nchar(table_user[j,1])-1) # odstranění mezery
    }

    table_user[j,1] %in% table_spec
    if ((table_user[j,1] %in% table_spec)==F){
      vector_druhy<-vector()
      cat(paste("The species",table_user[j,1], "has error in name."))
      cat("\n")
      cat(paste("Choose species from the table."))
      split <- strsplit(table_user[j,1], " ")
      now<-sapply(split , length) # number of words, počet slov v názvu

      prvni<-substr(table_user[j,1], 1, 1)
      pos2<-unlist(gregexpr(' ', table_user[j,1]))[1] + 1
      druhe<-substr(table_user[j,1], pos2, pos2)

      for (m in 1:length(table_package[,1])) {
        nazev<-table_package[m,1]
        Prvni<-substr(nazev, 1, 1)==prvni
        if (Prvni==T){
          Druhy<-unlist(gregexpr(' ', nazev))[1]+1
          Druhy<-substr(nazev, Druhy, Druhy)==druhe
          if (Druhy==T){
            stejny_druh<-nazev
            vector_druhy<-c(vector_druhy,stejny_druh)
          }
        }
      }
      tabulka<-as.data.frame(as.matrix(vector_druhy))
      colnames(tabulka)<-c("Species")
      cat("\n")
      print(tabulka)
      cat("\n")
      answ=readline(prompt="Choosen: ")
      table_user[j,1]<-tabulka[answ,]
    }
    table_user[j,COLname]<-table_package[match(table_user[j,1], table_package[,1]), COLname]
  }
  table_user<-table_user[, c(1,(ncol(table_user)-COLnum1):ncol(table_user), 2:(ncol(table_user)-COLnum2))]

  if (DBI_val=="UD"){
    na_val<-which(table_user[,2]=="NA")
  }

  if (DBI_val!="UD"){
    na_val<-which(table_user[,5]=="NA")
  }

  if(length(na_val)>0){
    for (m in na_val){
      cat(paste("DBI value of species", table_user[m, 1], "is NA, propably because of small amount of data."))
      cat("\n")
      message("Therefor this species will be removed.")
      table_user<-table_user[-(na_val),]
    }
  }
  cat(" ")
  cat("\n")

  Species<-as.factor(table_user[,1])
  table_user<-as.data.frame(sapply(table_user[2:ncol(table_user)], as.numeric))
  table_user<-data.frame(Species, table_user)
  print(table_user)
  table_user
}
