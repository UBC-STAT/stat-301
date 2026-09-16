#!/usr/bin/env Rscript

packages = commandArgs(trailingOnly=TRUE)

for (l in packages) {

    withCallingHandlers(install.packages(l, dependencies=TRUE, repos='https://cran.rstudio.com/'), warning = function(w) stop(w));

}