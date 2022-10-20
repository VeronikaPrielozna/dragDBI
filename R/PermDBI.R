#' Permutational DBI and potential for dragonfly communities
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
#' # Calculate the permutational DBI potential for the Highway stormwater and control ponds dataset, comparing the given community to 10 000 randomly assembled communities.
#'
#' Perm(DBI_Data, DBI_val = "CE", sim = 10000)

PermDBI<-function(df, DBI_val, DBI_UD, NAval=T, sim=10000, plot=F){
  hist_names<-as.vector(names(df))

  table1<-matrix(nrow = 1, ncol = 1)
  table_cal<-matrix(nrow = 1)

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

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))

    if (j %in% c1 | j %in% c2){
      vec1<-apply(combn(table_package,j), 2, sum)
    }
    else {
      vec1<-replicate(sim, sum(sample(table_package,j, prob = 1/(table_package+1), F)))


    }
    nase.dbi<-round(length(vec1[vec1<=sum.dbi])/(length(vec1)),3)
    table_cal<-rbind(nase.dbi)
    table1<-cbind(table1,table_cal)
    if(plot==T){
      hist(vec1, main = paste("Permutational DBI and potential for", hist_names[i]), breaks = 20, xlim = c(0,max(vec1)+8),
           cex.main = 1, xlab = " ", ylab = " ")
      title(ylab="Frequency", line = 3, cex = 1)
      abline(v=sum.dbi, lwd=3)
    }
  }
  table1<-table1[1,2:ncol(table1)]
  table1<-as.vector(table1)
  table2<-matrix(nrow = 1, ncol = length(table1))
  table2<-rbind(table2,table1)
  colnames(table2)<-colnames(df[COLnum2:ncol(df)])
  table2<-table2[-1,]
  cat("Permutational DBI and potential","\n")
  print(table2)
  table2
}
