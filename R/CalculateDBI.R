#' Main function calculating set of DBI score types  for odonate communities
#'
#' #' @description Calculates sum of DBI, mean of DBI, DBI potential, real DBI potential, and
#' permutational DBI potential for odonate community samples.
#' @usage CalculateDBI(df, DBI_val, DBI_UD, NAval=F, sim=10000)
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#' @importFrom utils data
#'
#' @param df A data frame created by ‘UniteData’ function, containing a list of taxa in the first
#' column, followed by the columns with the values of distribution, threat and sensitivity
#' subindices of DBI (in case of using a pre-set checklist via ‘UniteData’ function), column of
#' DBI values, and columns of abundances with sample names in the rows.
#' @param DBI_val Indicates the checklist to be used for comparison. ‘SA’ refers to the South
#' African checklist with DBI values (Samways et al., 2016). ‘AU’ refers to the Austrian checklist,
#' ‘CHE’ to the Swiss checklist, ‘CZ’ to the Czech checklist, ‘DE’ to the German checklist, ‘PL’ to
#' the Polish checklist, ‘SK’ to the Slovak checklist, and ‘SLO’ to the Slovenian checklist, all with
#' DBI values (Bílková et al., submitted). ‘UD’ refers to a user-defined or uploaded checklist.
#' @param DBI_UD In case that ‘UD’ is defined for the type, the name of user-loaded data frame
#' should be specified here.
#' @param NAval Logical, true in case of NA values in user-defined checklist.
#' @param sim The number of simulations identifies how many permutations should be made to
#' randomly assemble communities with the same species richness as the community in question.
#' The probability weight for each DBI is set as 2^-DBI, i.e., a species with a DBI higher by one
#' unit has half the probability of being selected into a random community than a species with a
#' lower DBI value.
#'
#' @returns A data frame consisting of the columns of score types (sum of DBI, mean of DBI, DBI
#' potential, real DBI potential, and permutational DBI potential) with samples in rows.
#' @examples
#' # For this function, you must have a data frame created by "UniteData".
#'
#' # Unification of Highway stormwater and control ponds dataset and Czech checklist with DBI
#' # values. Saved as "StormwatersDBI".
#'
#' \dontrun{
#' StormwatersDBI <- UniteData(Stormwaters, DBI_val = "CZ")
#'
#' # "StormwatersDBI" is then the input of the “CalculateDBI” function.
#'
#' # Calculate set of DBI score types for the Highway stormwater and control ponds dataset.
#' # Saved as "StormwatersCAL".
#'
#' StormwatersCAL <- CalculateDBI(StormwatersDBI, DBI_val = "CZ")
#'
#' # Then you can filter the specific calculation (column).
#'
#' StormwatersCAL$PermDBI
#'}
#'
#' \dontrun{
#' # Calculate set of DBI score types for species from South Africa. Saved as "AfricaCAL".
#'
#' AfricaCAL <- CalculateDBI(AfricaDBI, DBI_val = "SA")
#'
#' # Calculate set of DBI score types for species from Austria. Saved as "AustriaCAL".
#'
#' AustriaCAL <- CalculateDBI(Austria, DBI_val = "AU")
#'
#' # Calculate set of DBI score types for species from user data. In this case the input of
#' # CalculateDBI" function is also users DBI checklist ("DBI_UD"). Saved as "UserDataCAL".
#'
#' UserDataCAL <- CalculateDBI(UserDataDBI, DBI_val = "UD", DBI_UD)
#'}
#' @export CalculateDBI


CalculateDBI<-function(df, DBI_val, DBI_UD, NAval=F, sim=10000){
  data(list = c("DBI_SA", "DBI_CE", "DBI_CEC"), package = "dragDBI", envir = environment())
  hist_names<-as.vector(names(df))

  if(DBI_val=="SA"){
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

  table1<-matrix(nrow = 3, ncol = 2)
  table_cal<-matrix(nrow = 3)

  table2<-matrix(nrow = 2, ncol = 2)

  table_cal1<-matrix(nrow = 2)
  decr<-sort(table_package,decreasing=T)
  incr<-sort(table_package,decreasing=F)

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))
    mean.dbi<-mean(as.numeric(table_user[k]))
    mean.dbi<-round(mean.dbi,3)

    vec1<-replicate(sim, sum(sample(table_package, prob = 1/(2^table_package), j, F)))

    nase.dbi<-length(vec1[vec1<sum.dbi])/(length(vec1))
    table_cal<-rbind(sum.dbi, mean.dbi, nase.dbi)
    table1<-cbind(table1,table_cal)
  }

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.DBI<-sum(as.numeric(table_user[k]))
    potDBI<-(sum.DBI/sum(decr[1:length(k)]))
    trupotDBI<-((sum.DBI-sum(incr[1:length(k)]))/(sum(decr[1:length(k)])-sum(incr[1:length(k)])))

    Pmax<-sum(decr[1:length(k)])
    Pmin<-sum(incr[1:length(k)])

    table_cal1<-rbind(potDBI, trupotDBI)
    table2<-cbind(table2,table_cal1)

  }
  table1<-rbind(table1,table2)
  table1<-t(table1)

  table1<-table1[3:nrow(table1), ,drop=F]
  COLnam<-as.vector(colnames(df))
  rownames(table1)<-c(COLnam[COLnum2:ncol(df)])
  colnames(table1)<-c("SumDBI", "MeanDBI", "PermDBIpot", "PotDBI", "RealDBIpot")

  table1<-round(table1,3)
  table1<-as.data.frame(table1)
  return(table1)
}
