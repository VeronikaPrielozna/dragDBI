% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculateDBI.R
\name{CalculateDBI}
\alias{CalculateDBI}
\title{Main function calculating set of DBI score types  for odonate communities}
\usage{
CalculateDBI(df, DBI_val, DBI_UD, NAval=F, sim=10000)
}
\arguments{
\item{df}{A data frame created by ‘UniteData’ function, containing a list of taxa in the first column, followed by the columns with the values of distribution, threat and sensitivity subindices of DBI (in case of using  a pre-set checklist via ‘UniteData’ function), column of DBI values, and columns of abundances with sample names in the rows.}

\item{DBI_val}{Indicates checklist which should be used for comparison. ‘CE’ indicates the Central European checklist with DBI values. ‘SA’ indicates the South African checklist with DBI values. ‘UD’ indicates user defined/uploaded checklist.}

\item{DBI_UD}{In case that ‘UD’ is defined for the type, the name of user-loaded data frame should be specified here.}

\item{NAval}{Logical, true in case of NA values in user-defined checklist.}

\item{sim}{The number of simulations identifies how many permutations should be made to randomly assemble communities with the same species richness as the community in question. The probability weight for each DBI is set as 2^-DBI, i.e., a species with a DBI higher by one unit has half the probability of being selected into a random community than a species with a lower DBI value.}
}
\value{
A data frame consisting of the columns of score types  (sum of DBI, mean of DBI, DBI potential, real DBI potential, and permutational DBI potential) with samples in rows.
}
\description{
Calculates sum of DBI, mean of DBI, DBI potential, real DBI potential, and permutational DBI potential for odonate community samples.
}
\examples{
# For this function, you must have a data frame created by "UniteData".

# Unification of Highway stormwater and control ponds dataset and Central European checklist with DBI values. Saved as "StormwatersDBI".

StormwatersDBI<-UniteData(Stormwaters, DBI_val = "CE")

# "StormwatersDBI" is then the input of the “CalculateDBI” function.

# Calculate set of DBI score types for the Highway stormwater and control ponds dataset. Saved as "StormwatersCAL".

StromwatersCAL<-CalculateDBI(StormwatersDBI, DBI_val = "CE")

# Then you can filter the specific calculation (column).

StormwatersCAL$PermDBI

# Calculate set of DBI score types for species from South Africa. Saved as "AfricaCAL".

AfricaCAL<-CalculateDBI(AfricaDBI, DBI_val = "SA")

# Calculate set of DBI score types for species from user data. In this case the input of "CalculateDBI" function is also users DBI checklist ("DBI_UD") Saved as "UserDataCAL".

UserDataCAL<-CalculateDBI(UserDataDBI, DBI_val = "UD", DBI_UD)

}
