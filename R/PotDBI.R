#' DBI potential for dragonfly communities
#'
#' @param df
#' @param DBI_val
#' @param DBI_UD
#' @param NAval
#' @param sim
#' @param plot
#' @param arrow
#'
#' @return
#' @export
#'
#' @examples

PotDBI<-function(df, DBI_val, DBI_UD, NAval=F, sim=10000, plot=T, arrow=T){
  if (DBI_val=="CE"){
    table_package<-DBI_CE$TOTAL[-which(DBI_CE$TOTAL=="NA")]
    table_package<-as.integer(table_package)
    table_user<-df$TOTAL
  }

  if (DBI_val=="SA"){
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

  table1<-matrix(nrow = 5, ncol = 2)
  table_cal<-matrix(nrow = 4)
  decr<-sort(table_package,decreasing=T)
  incr<-sort(table_package,decreasing=F)
  vec.nase.dbi<-vector()

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))
    if (j %in% c1 | j %in% c2){
      vec1<-apply(combn(table_package,j), 2, sum)
    }
    else {
      vec1<-replicate(sim, sum(sample(table_package, prob = 1/(2^table_package), j, F)))
    }
    nase.dbi<-round(length(vec1[vec1<=sum.dbi])/(length(vec1)),3)
    vec.nase.dbi<-c(vec.nase.dbi,nase.dbi)
  }

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.DBI<-sum(vec.nase.dbi)
    potDBI<-(sum(vec.nase.dbi)/sum(decr[1:length(k)]))
    trupotDBI<-((sum(vec.nase.dbi)-sum(incr[1:length(k)]))/(sum(decr[1:length(k)])-sum(incr[1:length(k)])))
    Pmax<-sum(decr[1:length(k)])
    Pmin<-sum(incr[1:length(k)])

    table_cal<-rbind(sum.DBI, potDBI, trupotDBI, Pmax, Pmin)
    table1<-cbind(table1,table_cal)
  }
  table1<-table1[,3:ncol(table1)]
  table1<-round(table1,4)
  colnames(table1)<-colnames(df[,COLnum2:ncol(df)])

  if (plot == T){
    par(mfrow=c(1,1), mar=c(4,4,2,1))
    minP<-min(table1[5,])
    maxP<-max(table1[4,])
    posgr = barplot(as.matrix(table1[1,]), plot = F)
    plot(NULL,ylim = c(minP,maxP),xlim = c(1,ncol(table1))
         , xlab = "", xaxt = "n", ylab = "potDBI")

    points(c(1:ncol(table1)), table1[2,], pch = 16)

    axis(1,at=1:ncol(table1),lab=colnames(table1),las=2)

    if (arrow == T){
      for (i in 1:ncol(table1)){
        arrows(i,table1[5,i],i,table1[4,i],angle=90,code=3,length=0.08)
      }
    }
  }

  print("Tabulka pro DBI potencial a true DBI potential")
  print(table1)
}
