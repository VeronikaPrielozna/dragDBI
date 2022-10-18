#' Main function calculating set of DBI values for dragonfly communities
#'
#' @param df
#' @param DBI_val
#' @param DBI_UD
#' @param NAval
#' @param sim
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
#' # Calculate set of DBI values for the Highway stormwater and control ponds dataset
#'
#' CalculateDBI(DBI_Data, DBI_val = "CE", plot = T)

CalculateDBI<-function(df, DBI_val, DBI_UD, NAval=F, sim=10000, plot=T){
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

  c1<-c(1:3)
  c2<-c((length(table_package)-2):length(table_package))
  table1<-matrix(nrow = 3, ncol = 2)
  table_cal<-matrix(nrow = 3)

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))
    mean.dbi<-mean(as.numeric(table_user[k]))
    mean.dbi<-round(mean.dbi,3)

    if (j %in% c1 | j %in% c2){
      vec1<-apply(combn(table_package,length(k)), 2, sum)
    }
    else {
      vec1<-replicate(sim, sum(sample(table_package, prob = 1/(2^table_package), j, F)))

    }
    nase.dbi<-round(length(vec1[vec1<=sum.dbi])/(length(vec1)),3)
    table_cal<-rbind(sum.dbi, mean.dbi, nase.dbi)
    table1<-cbind(table1,table_cal)

    if(plot==T){
      hist(vec1, main = paste("Permutational DBI and potential for", hist_names[i]), breaks = 20, xlim = c(0,max(vec1)+8),
           cex.main = 1, xlab = " ", ylab = " ")
      title(ylab="Frequency", line = 3, cex = 1)
      abline(v=sum.dbi, lwd=3)
    }
  }
  table1<-table1[,3:ncol(table1)]
  colnames(table1)<-colnames(df[,COLnum2:ncol(df)])

  cat("Calculated set of DBI valuess","\n")
  print(table1)
  table1
}
