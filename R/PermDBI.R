#' Permutational DBI potential for odonate communities
#'
#' @description Calculates permutational DBI potential for odonate community samples.
#' @usage PermDBI(df, DBI_val, DBI_UD, NAval = T, sim = 10000, plot = F)
#'
#' @importFrom graphics abline arrows axis barplot hist par points title
#' @importFrom utils read.table
#' @importFrom utils data
#'
#' @param df A data frame created by ‘UniteData’ function, containing a list of taxa in the first
#' column, followed by the columns with the values of distribution, threat and sensitivity subindices
#' (in case of using a pre-set checklist via ‘UniteData’ function), column of DBI values, and columns
#' of abundances with sample names in the rows.
#' @param DBI_val Indicates the checklist to be used for comparison. ‘SA’ refers to the South African
#' checklist with DBI values (Samways et al., 2016). ‘AU’ refers to the Austrian checklist, ‘CHE’ to
#' the Swiss checklist, ‘CZ’ to the Czech checklist, ‘DE’ to the German checklist, ‘PL’ to the Polish
#' checklist, ‘SK’ to the Slovak checklist, and ‘SLO’ to the Slovenian checklist, all with DBI values
#' (Bílková et al., submitted). ‘UD’ refers to a user-defined or uploaded checklist.
#' @param DBI_UD In case that ‘UD’ is defined for the type, the name of user-loaded data frame should
#' be specified here.
#' @param NAval Logical, true in case of NA values in user-defined checklist.
#' @param sim The number of simulations identifies how many permutations should be made to randomly
#' assemble communities with the same species richness as the community in question.
#' @param plot Should a histogram for randomly assembled communities be plotted for each compared
#' community, completed with a vertical abline representing the given community? By default, the plot
#' is not rendered.
#'
#' @returns A data frame consisting of a column of index values with samples in rows.
#' @examples
#' # For this function, you must have a data frame created by "UniteData".
#'
#' # Unification of Highway stormwater and control ponds dataset and Czech checklist with DBI values.
#' # Saved as "StormwatersDBI".
#'
#' \dontrun{
#' StormwatersDBI <- UniteData(Stormwaters, DBI_val = "CZ")
#'
#' # "StormwatersDBI" is then the input of the “CalculateDBI” function.
#'
#' # Calculate the permutational DBI potential for the Highway stormwater and control ponds dataset,
#' # comparing the given community to 10,000 randomly assembled communities.
#'
#' PermDBI(StormwaterDBI, DBI_val = "CZ")
#'
#' # Calculate the permutational DBI potential for the user data with own DBI checklist and plotted
#' # results.
#'
#' PermDBI(UserDataDBI, DBI_val = "UD", DBI_UD, plot = T)
#' }
#'
#' @export PermDBI


PermDBI<-function(df, DBI_val, DBI_UD, NAval=T, sim=10000, plot=F){
  data(list = c("DBI_SA", "DBI_CE", "DBI_CEC"), package = "dragDBI", envir = environment())
  hist_names<-as.vector(names(df))

  table1<-matrix(nrow = 1, ncol = 1)
  table_cal<-matrix(nrow = 1)

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

  for (i in 1:(ncol(df)-COLnum1)){
    i<-i+COLnum1
    j<-nrow(df)-sum(df[,i]==0)
    k<-which('0' != df[,i])
    sum.dbi<-sum(as.numeric(table_user[k]))
    vec1<-replicate(sim, sum(sample(table_package,j, prob = 1/(2^table_package), F)))
    nase.dbi<-round(length(vec1[vec1<sum.dbi])/(length(vec1)),3)
    table_cal<-rbind(nase.dbi)
    table1<-cbind(table1,table_cal)
    if(plot==T){
      hist(vec1, main = paste("Permutational DBI and potential for", hist_names[i]), breaks = 20, xlim = c(0,max(vec1)+8),
           cex.main = 1, xlab = " ", ylab = " ")
      title(ylab="Frequency", line = 3, cex = 1)
      abline(v=sum.dbi, lwd=3)
    }
  }

  table1<-table1[,2:ncol(table1), drop=F]
  table1<-as.data.frame(table1)
  table2<-t(table1)
  rownames(table2)<-colnames(df[COLnum2:ncol(df)])
  colnames(table2)<-"PermDBI"
  return(table2)
}
