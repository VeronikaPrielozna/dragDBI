#' Standard DBI values for dragonfly communities
#'
#' @param df
#' @param UD
#' @param type
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' # Calculate the sum DBI index for the Highway stormwater and control ponds dataset.
#'
#' StandardDBI(DBI_Data, type = "sum")
#'
#' # Calculate the mean DBI index for the Highway stormwater and control ponds dataset.
#'
#' StandardDBI(DBI_Data, type = "mean")

StandardDBI<-function(df, UD=F, type, data="DBI", plot=F){
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

  if(plot==T){
    par(mfrow=c(1,1), mar=c(4,4,1,1))
    barplot(table2, ylim = c(0,max(table2) + yval), las = 2, ylab = rname, font.main = 1, names.arg = colnames(table2))
  }

  table2<-as.data.frame(table2)
  colnames(table2)<-rname
  print(table2)
  table2
}
