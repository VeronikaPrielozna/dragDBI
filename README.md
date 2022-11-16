# dragDBI
Calculation of Dragonfly Biotic Index for Odonata communities 

### Introduction
The ‘dragDBI’ package provides the main calculation function, wrapper functions for easy calculation of individual DBI score types, reference checklists with DBI values for Central European and South African odonate species, and three data-uploading functions that can be used for i) loading data, ii) quality-checking, annotation of DBI values to particular species, and preparation of data for analysis, or iii) uploading a set of user-defined DBI values.

### Installation 
The current release version (1.0.) of the package can be installed from GitHub. The homepage of the package is https://github.com/VeronikaPrielozna/dragDBI. To install the package, you need to install the ‘devtools’ package first.


```{r, eval = FALSE}
install.packages("devtools")
```

Then you need to load the ‘devtools’ package and use the ‘install_github’ function. For building vignettes, the argument build_vignettes must be equal to ‘T’. 

```{r, eval = FALSE}
library("devtools")

install_github("VeronikaPrielozna/dragDBI", build_vignettes = T)
```

### Input format
Dataset containing a list of taxa in the first column, and abundance or presence/absence data in the following columns with sample names in the column header.

```{r, eval = FALSE}
# Load library
library(dragDBI)

# Show the format of the built-in dataset.
head(Stormwaters)

               Species Sto1 Sto2 Sto3 Sto4 Sto5 Sto6 Sto7 Sto8 Sto9 Sto10 
1       Aeshna affinis    0    0    0    0    0    0    0    0    1     0   
2        Aeshna cyanea    0    0    1    0    1    1    2    0    1     0   
3         Aeshna mixta    5    1    2    0    0    4    3    2    3     3   
4       Anax imperator    3    0    2    1    2    2    2    2    2     2   
5      Anax parthenope    2    0    0    0    2    0    0    0    0     2   
6 Calopteryx splendens    0    0    0    0    1    0    0    0    0     1   


```

### Functions
The first function you are supposed to use is ‘LoadData’. This function checks and converts the input data into the format required by the package. For uploading user checklist with DBI values, ‘LoadDBI’ function is used. To match the input taxa names based on the species-level identification with their DBI values from the checklists, you have to use ‘UniteData’ function. The main function of ‘dragDBI’ package is ‘CalculateDBI’, which calculates the sum of DBI, mean of DBI, DBI potential, real DBI potential, and permutational DBI potential for odonate community samples.  

#### LoadData
```{r, eval = FALSE}
#  Uploading data containing species of Central Europe. Saved as "Europe". 
 
Europe<-LoadData()

#  Uploading data containing species of South Africa. Saved as "Africa". 
 
Africa<-LoadData()
```

#### LoadDBI
```{r, eval = FALSE}
#  Uploading user-defined DBI checklist. Saved as "DBI_UD". 

DBI_UD<-LoadDBI()
```

#### UniteData
```{r, eval = FALSE}
#  Unification of Highway stormwater and control ponds dataset and Central European checklist 
#  with DBI values. Saved as "StormwatersDBI".

StormwatersDBI<-UniteData(Stormwaters, DBI_val = "CE")

#  Unification of species of South Africa with South African checklist with DBI values. 
#  Saved as "AfricaDBI".

AfricaDBI<-UniteData(Africa, DBI_val = "SA")

#  Unification of users data with users checklist with DBI values, uploaded by "LoadDBI" function. 

UserDataDBI<-UniteData(UserData, DBI_val = "UD", DBI_UD)
```

#### CalculateDBI
```{r, eval = FALSE}
#  Calculate set of DBI values for the Highway stormwater and control ponds dataset. 
#  Saved as "StormwatersCAL". 

StromwatersCAL<-CalculateDBI(StormwatersDBI, DBI_val = "CE")

#  Then you can filter the specific calculation (column). 

StormwatersCAL$PermDBI

#  Calculate set of DBI values for species from South Africa. Saved as "AfricaCAL".

AfricaCAL<-CalculateDBI(AfricaDBI, DBI_val = "SA")

#  Calculate set of DBI values for species from user data. In this case the input of "CalculateDBI" 
#  function is also users DBI checklist ("DBI_UD") Saved as "UserDataCAL".
 
UserDataCAL<-CalculateDBI(UserDataDBI, DBI_val = "UD", DBI_UD)
```


### Individual index functions
#### StandardDBI
```{r, eval = FALSE}
#  Calculate the sum DBI score type for the Highway stormwater and control ponds dataset.

StandardDBI(StormwatersDBI, type = "sum") 

#  Calculate the sum DBI score type for the user data with own DBI checklist and plotted results. 

StandardDBI(UserDataDBI, UD = T, type = "sum", plot = T)
```

#### PermDBI
```{r, eval = FALSE}
#  Calculate the permutational DBI potential for the Highway stormwater and control ponds dataset,
#  comparing the given community to 10,000 randomly assembled communities. 

PermDBI(StormwaterDBI, DBI_val = "CE")

#  Calculate the permutational DBI potential for the user data
with own DBI checklist and plotted results. 

PermDBI(UserDataDBI, DBI_val = "UD", DBI_UD, plot = T)
```

#### PotDBI
```{r, eval = FALSE}
#  Calculate the real DBI potential for the Highway stormwater and control ponds dataset. 

PotDBI(StormwatersDBI, DBI_val = "CE", type = "rpDBI")

#  Calculate the real DBI potential for the user data
with own DBI checklist and plotted results. 

PotDBI(UserDataDBI, DBI_val = "UD", DBI_UD, type = "rpDBI", plot = T)

```

### Reporting problems
The package has been extensively tested using different test datasets, but if you come across any error or bug, please email me (verca.prielozna@gmail.com).


### Citation
To cite package ‘dragDBI’ in publications use:

  Prieložná V., Pyszko P., Šigutová H., Bílková E. & Dolný A., 2022: dragDBI:
  Calculation of Dragonfly Biotic Index for Odonata communities. R package version
  1.0. Available from: https://github.com/VeronikaPrielozna/dragDBI.


