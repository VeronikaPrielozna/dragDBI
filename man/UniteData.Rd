% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UniteData.R
\name{UniteData}
\alias{UniteData}
\title{Unification   of user dataset and DBI data}
\usage{
UniteData(df, DBI_val, DBI_UD)
}
\arguments{
\item{df}{A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).}

\item{DBI_val}{Indicates checklist which should be used for comparison. ‘CE’ indicates the Central European checklist with DBI values. ‘SA’ indicates the South African checklist with DBI values. ‘UD’ indicates user defined/uploaded checklist.}

\item{DBI_UD}{In case, that ‘UD’ is defined for the type, the name of user-loaded data frame should be specified here.}
}
\value{
A data frame consisting of user dataset and checklist of DBI values. In case that ‘CE’ or ‘SA’ is defined for the type, there will be also columns with values of distribution, threat, and sensitivity of the species.
}
\description{
The function allows for quality-checking of the correctness of species naming, and automated assignment of DBI values to the user dataset based on the comparison with checklists with DBI values for Central European or South African dragonflies. It checks the list of taxa present in the sample dataset against the checklist of taxa within the package to identify any non-scoring taxa in the samples (or spelling mistakes).
}
\examples{
# Unification of Highway stormwater and control ponds dataset and Central European checklist with DBI values. Saved as "StormwatersDBI".

StormwatersDBI<-UniteData(Stormwaters, DBI_val = "CE")

# Unification of species of South Africa with South African checklist with DBI values. Saved as "AfricaDBI".

AfricaDBI<-UniteData(Africa, DBI_val = "SA")

# Unification of users data with users checklist with DBI values, uploaded by "LoadDBI" function.

UserDataDBI<-UniteData(UserData, DBI_val = "UD", DBI_UD)

}
