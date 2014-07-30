Greenhouse
==========

Chart and slice greenhouse gas emissions from 1990-2010 by economic sector and category.


**To run from R:**

    require("dplyr")  
    require("devtools")  
    devtools::install_github("shiny", "rstudio")  
    devtools::install_github('rCharts', 'ramnathv')  

    library(shiny)  
    runGitHub("Greenhouse", "dKvale")
