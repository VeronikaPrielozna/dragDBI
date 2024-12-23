#' Standard DBI scores for odonate communities
#'
#' @description Calculates sum or mean of DBI for odonate community samples.
#' @usage StandardDBI(df, UD = F, type = "mean", data = "DBI", plot = F)
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#'
#' @param df A data frame created by ‘UniteData’ function, containing a list of taxa in the first
#' column, followed by the columns with the values of distribution, threat and sensitivity subindices
#' (in case of using a pre-set checklist via ‘UniteData’ function), column of DBI values, and columns
#' of abundances with sample names in the rows.
#' @param UD Logical, true in case of user uploaded checklist is used. By default, false.
#' @param type Indicates if sum of DBI (‘sum’) or mean of DBI (‘mean’) should be calculated. By
#' default, the mean of DBI (‘mean’) is used.
#' @param data In case that user checklist is not used in ‘UniteData’ function, the ‘StandardDBI’
#' function is able to calculate with DBI values (‘DBI’), values of sensitivity (‘SENS’), threat
#' (‘THR’) and distribution (‘DIST’).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is not
#' rendered.
#'
#' @returns A data frame consisting of a column of index values with samples in rows.
#' @examples
#' # For this function, you must have a data frame created by "UniteData".
#'
#' # Unification of Highway stormwater and control ponds dataset and Central European checklist
#' # with DBI values. Saved as "StormwatersDBI".
#' \dontrun{
#' StormwatersDBI <- UniteData(Stormwaters, DBI_val = "CE")
#'
#' # "StormwatersDBI" is then the input of the "StandardDBI" function.
#'
# Calculate the sum DBI score type for the Highway stormwater and control ponds dataset.
#'
#' StandardDBI(StormwatersDBI, type = "sum")
#'
#' # Calculate the sum DBI score type for the user data with own DBI checklist and plotted results.
#'
#' StandardDBI(UserDataDBI, UD = T, type = "sum", plot = T)
#'}
#' @export StandardDBI


StandardDBI<-function(df, UD=F, type="mean", data="DBI", plot=F){
  table1<-matrix(nrow = 1, ncol = 1)
  table_cal<-matrix(nrow = 1)
  if(UD==F){
    fcol<-6
    if (data=="DBI"){
      DBI_VAL<-df$TOTAL
      sname<-"DBI"
    }
    if (data=="SENS"){
      DBI_VAL<-df$Sensitivity
      sname<-"SENS"
    }

    if (data=="THR"){
      DBI_VAL<-df$Threat
      sname<-"THR"
    }

    if (data=="DIST"){
      DBI_VAL<-df$Distribution
      sname<-"DIST"
    }
  }

  if(UD==T){
    fcol<-3
    if (data=="DBI"){
      DBI_VAL<-df$DBI
      sname<-"DBI"
    }
  }
  DBI_VAL<-as.numeric(DBI_VAL)
  if(type=="sum"){
    for (i in fcol:ncol(df)){
      j<-nrow(df)-sum(df[,i]==0)
      k<-which('0' != df[,i])
      dbi.calc<-sum(as.numeric(DBI_VAL[k]))
      table_cal<-rbind(dbi.calc)
      table1<-cbind(table1,table_cal)
    }
    zname<-"Sum"
    table1<-round(table1,1)
    yval<-7
  }
  if(type=="mean"){
    for (i in fcol:ncol(df)){
      j<-nrow(df)-sum(df[,i]==0)
      k<-which('0' != df[,i])
      dbi.calc<-mean(as.numeric(DBI_VAL[k]))
      table_cal<-rbind(dbi.calc)
      table1<-cbind(table1,table_cal)
    }
    zname<-"Mean"
    table1<-round(table1,2)
    yval<-0.5
  }
  rname<-paste(zname,sname)
  rname<- gsub(" ", "", rname)

  table1<-table1[,2:ncol(table1), drop=F]
  table1<-as.data.frame(table1)
  table2<-t(table1)
  rownames(table2)<-colnames(df[fcol:ncol(df)])
  table2<-as.data.frame(table2)
  colnames(table2)<-rname

  if(plot==T){
    par(mfrow=c(1,1), mar=c(4,4,1,1))
    barplot(as.vector(table2[,1]), ylim = c(0,(max(table2[,1]) + yval)),
    las = 2, ylab = rname, font.main = 1, names.arg = rownames(table2))
  }
  return(table2)
}
