#### Function to create input file for STRUCTURE software from adegenet genind object ####
## author: Lindsay V. Clark, 26 July 2015, R_genetics_conv repository 
## adapted by Florian Kunz 10.04.2025

## Function needed update, as original output file was not readable for STRUCTURE
## STRUCTURE expects the first line to only contain loci names
## Minor update in code necessary (line 54 to 59), so full credits to original author Lindsay Clark

## obj: genind object
## file: file name to write
## pops: whether to include population info in the file
## Function is flexible with regards to ploidy, although genotypes are
## considered to be unambiguous.
## Missing data must be recorded as NA in obj@tab.

genind2structure <- function(obj, file="", pops=FALSE){
  if(!"genind" %in% class(obj)){
    warning("Function was designed for genind objects.")
  }
  
  # get the max ploidy of the dataset
  pl <- max(obj@ploidy)
  # get the number of individuals
  S <- adegenet::nInd(obj)
  # column of individual names to write; set up data.frame
  tab <- data.frame(ind=rep(adegenet::indNames(obj), each=pl))
  # column of pop ids to write
  if(pops){
    popnums <- 1:adegenet::nPop(obj)
    names(popnums) <- as.character(unique(adegenet::pop(obj)))
    popcol <- rep(popnums[as.character(adegenet::pop(obj))], each=pl)
    tab <- cbind(tab, data.frame(pop=popcol))
  }
  loci <- adegenet::locNames(obj) 
  # add columns for genotypes
  tab <- cbind(tab, matrix(-9, nrow=dim(tab)[1], ncol=adegenet::nLoc(obj),
                           dimnames=list(NULL,loci)))
  
  # begin going through loci
  for(L in loci){
    thesegen <- obj@tab[,grep(paste("^", L, "\\.", sep=""), 
                              dimnames(obj@tab)[[2]]), 
                        drop = FALSE] # genotypes by locus
    al <- 1:dim(thesegen)[2] # numbered alleles
    for(s in 1:S){
      if(all(!is.na(thesegen[s,]))){
        tabrows <- (1:dim(tab)[1])[tab[[1]] == adegenet::indNames(obj)[s]] # index of rows in output to write to
        tabrows <- tabrows[1:sum(thesegen[s,])] # subset if this is lower ploidy than max ploidy
        tab[tabrows,L] <- rep(al, times = thesegen[s,])
      }
    }
  }
  
  # UPDATE: exclude "ind" and "pop" and write line 1 first
  first_row <- as.data.frame(t(colnames(tab)[3:length(colnames(tab))]))
  write.table(first_row, file=file, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  # append rest of table
  write.table(tab, file=file, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE, append = T)
}