suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(xlsx))

projects <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)

num.smells <- matrix(0, nrow=3, ncol=6)

tot.corr <- c()
tot.corr.num <- c()
tot.corr.num.p <- c()
tot.corr.num.n <- c()
tot.corr.num2 <- c()
tot.corr.num.p2 <- c()
tot.corr.num.n2 <- c()
tot.corr.num3 <- c()
tot.corr.num.p3 <- c()
tot.corr.num.n3 <- c()
tot.corr.2 <- c()
tot.corr.3 <- c()
tot.corr.num.all <- c()



first <- TRUE

for (prj in projects) {
  
  ## devs.
  repo <- read.csv(paste(prj, "report.csv", sep="/"))
  mean.devs <- ceiling(mean(repo$devs))
  if (mean.devs <= 50)
    class.prj <- 1
  else if (mean.devs <= 150)
    class.prj <- 2
  else
    class.prj <- 3
  num.smells[class.prj, 6] <- num.smells[class.prj, 6]+1
  rm(repo)

  p.e <- read.csv(paste(prj, "pearson-estimate.csv", sep="/"))[-1]
  p.p <- read.csv(paste(prj, "pearson-pvalue.csv", sep="/"))[-1]
  s.e <- read.csv(paste(prj, "spearman-estimate.csv", sep="/"))[-1]
  s.p <- read.csv(paste(prj, "spearman-pvalue.csv", sep="/"))[-1]

  
  if (first){
    colnames(num.smells) <- c("org.silo","prima.donnas", "radio.silence", "black.cloud", "missing.links", "Tot projects")
    rownames(num.smells) <- c("<50", "50-150", ">150")
    tot.corr <- t(p.p)
    tot.corr.num <- t(p.p)
    for (a in 1:nrow(p.p)){
      for (b in 1:ncol(p.p)){
        tot.corr[b, a] <- ""
	tot.corr.num[b, a] <- 0
      }
    }
    colnames(tot.corr) <- c("org.silo","prima.donnas", "radio.silence", "black.cloud", "missing.links")
    colnames(tot.corr.num) <- c("org.silo","prima.donnas", "radio.silence", "black.cloud", "missing.links")
    tot.corr.num.n <- tot.corr.num
    tot.corr.num.p <- tot.corr.num
    tot.corr.num2 <- tot.corr.num
    tot.corr.num.n2 <- tot.corr.num
    tot.corr.num.p2 <- tot.corr.num
    tot.corr.num3 <- tot.corr.num
    tot.corr.num.n3 <- tot.corr.num
    tot.corr.num.p3 <- tot.corr.num
    tot.corr.num.all <- tot.corr.num
    tot.corr.2 <- tot.corr
    tot.corr.3 <- tot.corr
    
    first <- FALSE
  }
  
  
  for (smell in 1:nrow(p.p)){
    nope <- 0
    for (col in 1:ncol(p.p)){
      if (is.na(p.p[smell, col])){
        nope <- nope+1
        next
      }
      
      p.spear <-  s.p[smell, col]
      p.pear <-  p.p[smell, col]
      est <- NA
      pva <- NA
      
      if (p.spear < p.pear) {
        if ( round(p.spear, digits = 2) < 0.05){
          est <- s.e[smell, col]
          pva <- round(p.spear, digits = 2)
        }
      } else
        if ( round(p.pear, digits = 2) < 0.05){
          est <- p.e[smell, col]
          pva <- round(p.pear, digits = 2)
        }

      if(!is.na(est)){
        entry <- paste(prj, " (", round(est, digits = 2), ", ", pva, ")", sep="")

        if (class.prj==1){
          if(nchar(tot.corr[col, smell])==0){
            tot.corr[col, smell] <- entry
          } else
            tot.corr[col, smell] <- paste(entry, tot.corr[col, smell], sep = "\n")
          if(est>=0)
              tot.corr.num.p[col, smell] <- tot.corr.num.p[col, smell]+1
          else
	      tot.corr.num.n[col, smell] <- tot.corr.num.n[col, smell]+1
        }
        else if (class.prj==2){
          if(nchar(tot.corr.2[col, smell])==0){
            tot.corr.2[col, smell] <- entry
          } else
            tot.corr.2[col, smell] <- paste(entry, tot.corr.2[col, smell], sep = "\n")
          if(est>=0)
              tot.corr.num.p2[col, smell] <- tot.corr.num.p2[col, smell]+1
          else
	      tot.corr.num.n2[col, smell] <- tot.corr.num.n2[col, smell]+1
        }
        else{
          if(nchar(tot.corr.3[col, smell])==0){
            tot.corr.3[col, smell] <- entry
          } else
            tot.corr.3[col, smell] <- paste(entry, tot.corr.3[col, smell], sep = "\n")
          if(est>=0)
              tot.corr.num.p3[col, smell] <- tot.corr.num.p3[col, smell]+1
          else
	      tot.corr.num.n3[col, smell] <- tot.corr.num.n3[col, smell]+1
        }
      }
    }
    if(nope < ncol(p.p))
      num.smells[class.prj, smell] <- num.smells[class.prj, smell]+1
  }
  
}
  

tot.corr.all <- tot.corr

for (a in 1:nrow(tot.corr.all)){
  for (b in 1:ncol(tot.corr.all)){
    if(nchar(tot.corr.2[a, b])!=0)
      if(nchar(tot.corr.all[a, b])==0) {
        tot.corr.all[a, b] <- tot.corr.2[a, b]
      } else
        tot.corr.all[a, b] <- paste(tot.corr.all[a, b], tot.corr.2[a, b], sep = "\n")
    if(nchar(tot.corr.3[a, b])!=0)
      if(nchar(tot.corr.all[a, b])==0) {
        tot.corr.all[a, b] <- tot.corr.3[a, b]
      } else
        tot.corr.all[a, b] <- paste(tot.corr.all[a, b], tot.corr.3[a, b], sep = "\n")

    tot.corr.num[a, b] <- paste(tot.corr.num.p[a, b],"(+); ", tot.corr.num.n[a, b], "(-);", sep = "")
    tot.corr.num2[a, b] <- paste(tot.corr.num.p2[a, b],"(+); ", tot.corr.num.n2[a, b], "(-);", sep = "")
    tot.corr.num3[a, b] <- paste(tot.corr.num.p3[a, b],"(+); ", tot.corr.num.n3[a, b], "(-);", sep = "")
    tot.corr.num.all[a, b] <- paste(tot.corr.num.p[a, b]+tot.corr.num.p2[a, b]+tot.corr.num.p3[a, b], "(+); ", 
					tot.corr.num.n[a, b]+tot.corr.num.n2[a, b]+tot.corr.num.n3[a, b], "(-);", sep = "")

  }
}




write.xlsx(tot.corr, "correlations.xlsx", sheetName="<50", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
write.xlsx(tot.corr.num, "correlations.xlsx", sheetName="<50 (Num)", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.2, "correlations.xlsx", sheetName="50-150", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.num2, "correlations.xlsx", sheetName="50-150 (Num)", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.3, "correlations.xlsx", sheetName=">150", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.num3, "correlations.xlsx", sheetName=">150 (Num)", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.all, "correlations.xlsx", sheetName="ALL", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(tot.corr.num.all, "correlations.xlsx", sheetName="ALL (Num)", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(num.smells, "correlations.xlsx", sheetName="Summary", 
           col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
