#' Main function calculating set of DBI values for dragonfly communities
#'
#' @param df
#' @param DBI_val
#' @param DBI_UD
#' @param NAval
#' @param sim
#'
#' @return
#' @export
#'
#' @examples
#' # Calculate set of DBI values for the Highway stormwater and control ponds dataset
#'
#' CalculateDBI(DBI_Data, DBI_val = "CE")

CalculateDBI<-function(df, DBI_val, DBI_UD, NAval=F, sim=10000){
  hist_names<-as.vector(names(df))

  if (DBI_val=="CE"){
    table_package<-DBI_CE$TOTAL[-which(DBI_CE$TOTAL=="NA")]
    table_package<-as.integer(table_package)
    table_user<-df$TOTAL
  }

  if(DBI_val=="SA"){
    table_package<-DBI_SA$TOTAL
    table_user<-df$TOTAL
  }

  if(DBI_val=="UD"){
    table_user<-df$DBI
    if (NAval==T){
      table_package<-DBI_UD$DBI[-which(DBI_UD$DBI=="NA")]
      table_package<-as.integer(table_package)
    }
    if (NAval==F){
      table_package<-DBI_UD$DBI
    }
    COLnum1<-2
    COLnum2<-3
  }

  if(DBI_val!="UD"){
    COLnum1<-5
    COLnum2<-6
  }

  table1<-matrix(nrow = 3, ncol = 2)
  table_cal<-matrix(nrow = 3)

  table2<-matrix(nrow = 2, ncol = 2)

  table_cal1<-matrix(nrow = 2)
  decr<-sort(table_package,decreasing=T)
  incr<-sort(table_package,decreasing=F)
  vec.nase.dbi<-vector()

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))
    mean.dbi<-mean(as.numeric(table_user[k]))
    mean.dbi<-round(mean.dbi,3)

    vec1<-replicate(sim, sum(sample(table_package, prob = 1/(2^table_package), j, F)))

    nase.dbi<-round(length(vec1[vec1<sum.dbi])/(length(vec1)),3)
    vec.nase.dbi<-c(vec.nase.dbi,nase.dbi)
    table_cal<-rbind(sum.dbi, mean.dbi, nase.dbi)
    table1<-cbind(table1,table_cal)
  }

  for (i in 1:(ncol(df)-COLnum1)){
      i<-i+COLnum1
      j<-nrow(df)-sum(df[,i]==0)
      k<-which('0' != df[,i])
      k<-table_user[k]
      sum.DBI<-sum(as.numeric(table_user[k]))
      potDBI<-(sum(vec.nase.dbi)/sum(decr[1:length(k)]))
      trupotDBI<-((sum(vec.nase.dbi)-sum(incr[1:length(k)]))/(sum(decr[1:length(k)])-sum(incr[1:length(k)])))
      Pmax<-sum(decr[1:length(k)])
      Pmin<-sum(incr[1:length(k)])

      table_cal1<-rbind(potDBI, trupotDBI)
      table2<-cbind(table2,table_cal1)
    }
  table1<-as.data.frame(table1)
  table1<-rbind(table1, table2[1,], table2[2,])
  table1<-table1[,3:ncol(table1)]

  colnames(table1)<-colnames(df[,COLnum2:ncol(df)])
  rownames(table1)<-c("SumDBI", "MeanDBI", "PermDBIpot", "PotDBI", "TrueDBIpot")
  table1<-round(table1,3)
  cat("Calculated set of DBI values","\n")
  t(table1)
}
