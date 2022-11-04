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

  if(ncol(df)==3){
    ROWnum<-0
  }

  else{
    RoWnum<-1
  }

  table1<-matrix(nrow = 5, ncol = 2)
  table_cal<-matrix(nrow = 4)
  decr<-sort(table_package,decreasing=T)
  incr<-sort(table_package,decreasing=F)
  vec.nase.dbi<-vector()


  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    k<-table_user[k]
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

    else{
      for (i in 1:ncol(table1)){
        arrows(i,minP[i],i,maxP[i],angle=90,code=3,length=0.08)
      }
    }
    table1<-round(table1,4)
  }

  table2<-matrix(ncol=ncol(table1))
  table2<-cbind(table1[2,], table1[3,], table1[1,], table1[4,], table1[5,])
  colnames(table2)<-c("pDBI", "tpDBI", "SumDBI", "MaxsumDBI", "MinsumDBI")

  if (type=="def"){
    # table3<-t(table2)
    table3<-table2
  }

  if(type=="tpDBI"){
    table3<-table2[2,]
    # table3<-as.data.frame(table3)
    colnames(table3)<-"tpDBI"
  }

  if(type=="pDBI"){
    table3<-table2[1,]
    # table3<-as.data.frame(table3)
    colnames(table3)<-"pDBI"
  }
  COLnam<-as.vector(colnames(df))
  rownames(table3)<-COLnam[COLnum2:ncol(df)]
  print(table3)
  table3
}

