library("downloader")

getGDSCU129 <- function(tmpdir = tempdir()) {
  dir.create(tmpdir, showWarnings=FALSE, recursive=TRUE)

  ftpdir <- "ftp://ftp.ebi.ac.uk/pub/databases/microarray/data/experiment/MTAB/E-MTAB-3610/"
  
  ## phenodata
  dwl.status <- download(url=sprintf("%s/E-MTAB-3610.sdrf.txt", ftpdir), destfile=file.path(tmpdir, "E-MTAB-3610.sdrf.txt"), quiet=TRUE)
  sampleinfo <- read.csv(file.path(tmpdir, "E-MTAB-3610.sdrf.txt"), sep="\t", stringsAsFactors=FALSE)
  rownames(sampleinfo) <- sampleinfo[ , "Assay.Name"]
  sampleinfo[ , "Array.Data.File"] <- gsub("[.]cel$", ".CEL.gz", sampleinfo[ , "Array.Data.File"])
  uarchive <- sort(unique(sampleinfo[ , "Comment..ArrayExpress.FTP.file."]))
  

  message("Download genomic data")

  require(R.utils) || stop("Library R.utils is not available!")

    ## download and compress CEL files
    celfile.timestamp <- celfn <- NULL
    i <- 1
    while(i <= length(uarchive)) {
      ## assuming there are only 9 zip archives (need to check if the update version has more)
      dwl.status <- download(url=uarchive[i], destfile=file.path(tmpdir, basename(uarchive)[i]), quiet=TRUE)
      if(dwl.status != 0) {
       message("\t-> download failed, let's try again ...")
       file.remove(file.path(tmpdir, basename(uarchive)[i]))
       # i <- i - 1
       } else {
         ## unzip archive
         fff <- unzip(zipfile=file.path(tmpdir, basename(uarchive)[i]), list=TRUE)
         fff[,"Name"] <- file.path("/pfs/out", as.character(fff[,"Name"]))
         celfile.timestamp <- c(celfile.timestamp, as.character(fff[ ,"Date"]))
         res <- unzip(zipfile=file.path(tmpdir, basename(uarchive)[i]), exdir=tmpdir)
         message(res)
         ## rename CEL files 
         sapply(as.character(fff[ ,"Name"]), function (x) {
           system(sprintf("mv %s %s", x, gsub("[.]cel$", ".CEL", x)))
           })
         ## compress each CEL file individually using gzip
         sapply(fff[ ,"Name"], R.utils::gzip, overwrite=TRUE)
         fff[ ,"Name"] <- gsub("[.]cel$", ".CEL.gz", as.character(fff[ ,"Name"]))
         celfn <- c(celfn, fff[ ,"Name"])
         sapply(as.character(fff[ ,"Name"]), function (x) {
           system(sprintf("mv %s %s", x, gsub(tmpdir, "/pfs/out", x, fixed=TRUE)))
           })
         i <- i + 1
       }
     }
     celfile.timestamp <- t(sapply(strsplit(celfile.timestamp, split=" "), function(x) { return(x) }))
     dimnames(celfile.timestamp) <- list(gsub("[.]CEL$", "", celfn), c("file.day", "file.hour"))
     write.csv(celfile.timestamp, file=file.path("/pfs/out/celfile_timestamp.csv"))
     save(list=c("celfile.timestamp"), compress=TRUE, file="/pfs/out/celfile_timestamp.RData")

   }
getGDSCU129()
