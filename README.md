---
output:
  github_document
---

# dragDBI
Calculation of Dragonfly Biotic Index for Odonata communities

### Introduction
The “dragDBI” package provides the main calculation function, wrapper functions for easy calculation of DBI indices, reference checklists with DBI values for Central European and South African dragonflies, and a data uploading function that can be used to load, quality-check, and prepare data for analysis.

### Installation 
The current release version (1.0) of the package can be installed from GitHub. The homepage of the package is https://github.com/VeronikaPrielozna/dragDBI. To install the package, you need to first install the “devtools” package.

```{r, eval=FALSE}
install.packages("devtools")
```

After that, you must load the “devtools” package and use the “install_github” function. For building vignettes, the argument build_vignettes must be equal to “T”. 

```{r, eval=FALSE}
library("devtools")

install_github("VeronikaPrielozna/dragDBI", build_vignettes = T)
```

### Input format
A dataset containing a list of taxa in the first column, and abundance or presence/absence data in following columns with sample names in the column’s header. 

```{r, eval=FALSE}
# Load library
library(dragDBI)


# Show the format of the built-in dataset
head(Stormwaters)
```
### Functions
The first function you are supposed to use is “LoadData”. This function checks and converts the input data into the format needed by the package. To match the input taxa names based on species-level identification with their values for DBI from the checklists, you have to use “UniteData” function. The main function of “dragDBI” package is “CalculateDBI”, which calculates the sum of DBI, mean of DBI, DBI potential, true DBI potential, and permutational DBI potential for dragonfly community samples.  

LoadDBI, LoadData, UniteData, CalculateDBI

### Individual index functions
StandardDBI, PermDBI, PotDBI

### Reporting problems
The package has been extensively tested using different test datasets, but if you come across an error or bug, then please [email me] (verca.prielozna@gmail.com).

### Paper and citation



