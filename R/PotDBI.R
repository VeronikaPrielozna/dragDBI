#' DBI potential for odonate communities
#'
#' @description Calculates DBI potential and real DBI potential for odonate community samples.
#' @usage PotDBI(df, DBI_val, DBI_UD, type = "def", NAval = F, plot = F)
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#' @importFrom utils data
#'
#' @param df A data frame created by ‘UniteData’ function, containing a list of taxa in the first
#' column, followed by the columns with the values of distribution, threat and sensitivity subindices
#' (in case of using a pre-set checklist via ‘UniteData’ function), column of DBI values, and columns
#' of abundances with sample names in the rows.
#' @param type Indicates if ‘rpDBI’, i.e., real DBI potential or ‘pDBI’, i.e., DBI potential should
#' be calculated. By default, DBI potential, real DBI potential, sum of DBI and probable maximum value
#' of sum of DBI are calculated.
#' @param DBI_val Indicates the checklist to be used for comparison. ‘SA’ refers to the South African
#' checklist with DBI values (Samways et al., 2016). ‘AU’ refers to the Austrian checklist, ‘CHE’ to
#' the Swiss checklist, ‘CZ’ to the Czech checklist, ‘DE’ to the German checklist, ‘PL’ to the Polish
#' checklist, ‘SK’ to the Slovak checklist, and ‘SLO’ to the Slovenian checklist, all with DBI values
#' (Bílková et al., submitted). ‘UD’ refers to a user-defined or uploaded checklist.
#' @param DBI_UD In case that ‘UD’ is defined for the type, the name of user-loaded data frame should
#' be specified here.
#' @param NAval Logical, true in case of NA values in user-defined checklist.
#' @param plot Should a plot displaying a range between probable maximum and minimum values of sum of
#' DBI and position of sum of DBI in the range be plotted? By default, the plot is not rendered.
#'
#' @returns A data frame consisting of a column of index values with samples in rows.
#' @examples
#' # For this function, you must have a data frame created by "UniteData".
#'
#' # Unification of Highway stormwater and control ponds dataset and Czech checklist with DBI values.
#' # Saved as "StormwatersDBI".
#' \dontrun{
#' StormwatersDBI <- UniteData(Stormwaters, DBI_val = "CZ")
#'
#' # "StormwatersDBI" is then the input of the "CalculateDBI" function.
#'
#' # Calculate the real DBI potential for the Highway stormwater and control ponds dataset.
#'
#' PotDBI(StormwatersDBI, DBI_val = "CZ", type = "rpDBI")
#'
#' # Calculate the real DBI potential for the user data with own DBI checklist and plotted results.
#'
#' PotDBI(UserDataDBI, DBI_val = "UD", DBI_UD, type = "rpDBI", plot = T)
#'}
#'
#' @export PotDBI


PotDBI<-function(df, DBI_val, DBI_UD, type="def", NAval=F, plot=F){
  data(list = c("DBI_SA", "DBI_CE", "DBI_CEC"), package = "dragDBI", envir = environment())

  if (DBI_val=="SA"){
    table_package<-DBI_SA$TOTAL
    table_user<-df$TOTAL
  }

  if(DBI_val %in% c("AU", "CHE", "CZ", "DE", "PL", "SK", "SLO")) {
    DBI_CEC <- DBI_CEC[, !names(DBI_CEC) %in% "Author"]
    DBI_CEC <- split(DBI_CEC, DBI_CEC$Country)
    DBI_CEC <- lapply(DBI_CEC, function(tab) {
      tab <- tab[, !names(tab) %in% "Country"]
      return(tab)
    })
    DBI_CEC<-DBI_CEC[[DBI_val]]
    table_package<-DBI_CEC$TOTAL[-which(DBI_CEC$TOTAL=="NA")]
    table_package<-as.integer(table_package)
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
    axis(1,at=1:ncol(table1),labels=colnames(table1),las=2)

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
    CN<-c("DBIpot", "RealDBIpot", "SumDBI", "MaxsumDBI", "MinsumDBI")
  }

  if(type=="rpDBI"){
    table3<-table2[,2, drop=F]
    CN<-"RealDBIpot"
  }

  if(type=="pDBI"){
    table3<-table2[,1,drop=F]
    CN<-"DBIpot"
  }

  COLnam<-as.vector(colnames(df))
  rownames(table3)<-c(COLnam[COLnum2:ncol(df)])
  colnames(table3)<-CN
  table3<-round(table3, 5)
  return(table3)
}








