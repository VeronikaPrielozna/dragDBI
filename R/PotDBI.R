#' DBI potential for dragonfly communities
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

PotDBI<-function(df, DBI_val, DBI_UD, type="def", NAval=F, plot=F){
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

  table1<-matrix(nrow = 5, ncol = 2)
  table_cal<-matrix(nrow = 4)
  decr<-sort(table_package,decreasing=T)
  incr<-sort(table_package,decreasing=F)

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.DBI<-sum(as.numeric(table_user[k]))
    potDBI<-(sum.DBI/sum(decr[1:length(k)]))
    trupotDBI<-((sum.DBI-sum(incr[1:length(k)]))/(sum(decr[1:length(k)])-sum(incr[1:length(k)])))
    Pmax<-sum(decr[1:length(k)])
    Pmin<-sum(incr[1:length(k)])
    table_cal<-rbind(sum.DBI, potDBI, trupotDBI, Pmax, Pmin)
    table1<-cbind(table1,table_cal)
  }
  table1<-table1[,3:ncol(table1)]
  table1<-as.data.frame(table1)
  colnames(table1)<-colnames(df[,COLnum2:ncol(df)])

  if (plot == T){
    par(mfrow=c(1,1), mar=c(4,4,2,1))
    minP<-table1[5,]
    maxP<-table1[4,]
    posgr = barplot(as.matrix(table1[1,]), plot = F)
    plot(NULL,ylim = c(min(minP),max(maxP)),xlim = c(1,length(table1))
         , xlab = "", xaxt = "n", ylab = "Sum of DBI")
    points(c(1:ncol(table1)), table1[1,], pch = 16)
    axis(1,at=1:ncol(table1),lab=colnames(table1),las=2)

    if(length(minP)==1){
      arrows(1,minP[,1],1,maxP[,1],angle=90,code=3,length=0.08)
    }

    if(length((minP)>1)){
      for (i in 1:ncol(table1)){
        arrows(i,minP[,i],i,maxP[,i],angle=90,code=3,length=0.08)
      }
    }
    table1<-round(table1,4)
  }
  table2<-t(table1)
  table2<-table2[,c(2,3,1,4,5), drop=F]

  if (type=="def"){
    table3<-table2
    CN<-c("pDBI", "tpDBI", "SumDBI", "MaxsumDBI", "MinsumDBI")
    print(table3)
  }

  if(type=="tpDBI"){
    table3<-table2[,2, drop=F]
    CN<-"tpDBI"
  }

  if(type=="pDBI"){
    table3<-table2[,1,drop=F]
    CN<-"pDBI"
  }

  COLnam<-as.vector(colnames(df))
  rownames(table3)<-c(COLnam[COLnum2:ncol(df)])
  colnames(table3)<-CN

  print(table3)
  table3
}








