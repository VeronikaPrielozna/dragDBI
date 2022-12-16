#' Unification   of user dataset and DBI data
#'
#' @description The function allows for quality-checking of the correctness of species naming, and automated assignment of DBI values to the user dataset based on the comparison with checklists with DBI values for Central European or South African dragonflies. It checks the list of taxa present in the sample dataset against the checklist of taxa within the package to identify any non-scoring taxa in the samples (or spelling mistakes).
#' @usage UniteData(df, DBI_val, DBI_UD)
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param DBI_val Indicates checklist which should be used for comparison. ‘CE’ indicates the Central European checklist with DBI values. ‘SA’ indicates the South African checklist with DBI values. ‘UD’ indicates user defined/uploaded checklist.
#' @param DBI_UD In case, that ‘UD’ is defined for the type, the name of user-loaded data frame should be specified here.
#'
#' @returns A data frame consisting of user dataset and checklist of DBI values. In case that ‘CE’ or ‘SA’ is defined for the type, there will be also columns with values of distribution, threat, and sensitivity of the species.
#' @examples
#' # Unification of Highway stormwater and control ponds dataset and Central European checklist with DBI values. Saved as "StormwatersDBI".
#'
#' StormwatersDBI<-UniteData(Stormwaters, DBI_val = "CE")
#'
#' # Unification of species of South Africa with South African checklist with DBI values. Saved as "AfricaDBI".
#'
#' AfricaDBI<-UniteData(Africa, DBI_val = "SA")
#'
#' # Unification of users data with users checklist with DBI values, uploaded by "LoadDBI" function.
#'
#' UserDataDBI<-UniteData(UserData, DBI_val = "UD", DBI_UD)
#'
#' @export UniteData
#'

UniteData<-function(df, DBI_val, DBI_UD){
  ms<-"This species is not in DBI dataset. Therefor this it will be removed."
  miss<-vector()
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
    if(ncol(table_package)==2){
      COLnum<-1
    }
    COLnum1<-0
    COLnum2<-2
  }
  if(DBI_val!="UD"){
    table_spec<-table_package$Species
    COLname<-c("Distribution", "Threat", "Sensitivity", "TOTAL")
    COLnum<-0
    COLnum1<-3
    COLnum2<-4
  }
  for (j in 1:nrow(table_user)){
    if (substr(table_user[j,1], nchar(table_user[j,1]), nchar(table_user[j,1]))==" "){
      table_user[j,1]<-substring(table_user[j,1],1, nchar(table_user[j,1])-1)
    }

    table_user[j,1] %in% table_spec
    if ((table_user[j,1] %in% table_spec)==F){
      vector_druhy<-vector()
      cat(paste("The species",table_user[j,1], "has error in name."))
      cat("\n")
      cat(paste("Choose probable species from the table."))
      split <- strsplit(table_user[j,1], " ")
      now<-sapply(split , length)

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
      tabulka<-rbind(tabulka, c("None!"))
      colnames(tabulka)<-c("Species")
      cat("\n")
      print(tabulka)
      cat("\n")
      answ=readline(prompt="Choosen: ")

      if (!(answ %in% 1:nrow(tabulka))){
        stop('Your answer is not between 1 and ', nrow(tabulka), '.')
      }


      if(tabulka[answ,]==c("None!")) {
        miss<-c(miss,table_user[j,1])
        table_user[j,1]<-"NA"
      }

      else{
        table_user[j,1]<-tabulka[answ,]
      }
    }
    table_user[j,COLname]<-table_package[match(table_user[j,1], table_package[,1]), COLname]
  }

  if (length(miss)>0){
    message("Those species were not in the checklist:","\n")
    for (o in 1:length(miss)){
      message(paste("\t\t\t\t\t",miss[o]))
    }
    message("Therefor, they will be removed.","\n")
    table_user<-table_user[-which(table_user[,1]=="NA"),]
  }


  na_val<-which(table_user$TOTAL=="NA")

  if(length(na_val)>0){
    for (m in 1:length(na_val)){
      message(paste("DBI value of species", table_user[m,1], "is NA, propably because of small amount of data.", "\n",
                    "Therefor this species will be removed."))
      cat("\n")
      table_user<-table_user[-(na_val),]
    }
  }

  cat("\n")
  Species<-as.factor(table_user[,1])
  table_user<-as.data.frame(sapply(table_user[2:ncol(table_user)], as.numeric))
  table_user<-data.frame(Species, table_user)
  table_user<-table_user[, c(1,((ncol(table_user)-COLnum1)):ncol(table_user), 2:((ncol(table_user))-COLnum2+COLnum))]
  return(table_user)
}
