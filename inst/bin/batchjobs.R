#! /usr/bin/env Rscript

#' Submit All Jobs
#' 
#' @param dir path to the directory where to look for a registry.  
#'

library(CLIR)

dir <- cli_arg('--dir')
max_jobs <- cli_arg('--max-jobs', Inf)
test_only <- cli_arg('--test', FALSE)
    
# lookup for registry file
regpath <- list.files(dir, pattern = '^registry.RData$', recursive = TRUE, full.names = TRUE)
sapply(regpath, function(p){

      # load packages
      library(BBmisc)
      library(BatchExperiments)
      
      # change to parent directory 
      owd <- setwd(dirnames(p))
      on.exit( setwd(owd) )
      # load registry
      reg <- loadRegistry(basename(p))
      
      if( test_only ) return( testJob(reg) )
      
      # submit all pending jobs (possibly chunked)
      ids <- dbFindSubmitted(reg, negate = TRUE)
      if( is.finite(max_jobs) && max_jobs < length(ids) ){
        chunked <- chunk(, n.chunks = max_jobs)
        #submitJobs(reg, chunked)
        str(chunked)
      }else submitJobs(reg, ids)
      
      ids
    })


cat(paste(sprintf("%s: %s", names(basename(regpath)), regpath), collapse = ", "), "\n", sep = "")
