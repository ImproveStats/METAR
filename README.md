# META-R
## Multi Environment Trial Analysis

META-R is a set of R programs that performs statistical analyses to calculate BLUEs, BLUPs, genetic correlations among locations and genetic correlations between variables, broad-sense heritability, and other statistics for breeding trials are given too, in order to make boxplots and histograms. Analyses may be performed by location, across management conditions or across all locations. META-R contains a graphical JAVA interface that helps the user to easily choose input files, which analysis to implement, and which variables to analyze. 

META-R is easy-to-use, the next steps must be typed in the R console:
# Installation in R from github
```R
   install.packages(pkg='devtools',repos='https://cran.r-project.org/')  # install devtools
   library(devtools)                                                     # load the library
   install_git('https://github.com/MarcooLopez/METAR/')  
```

# Getting started
To start to use META-R, the following code is neccesary
```R
   library(METAR)                                                    # load the library
   METAR()                                                           # open the interface
```
