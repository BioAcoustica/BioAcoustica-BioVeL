library(tuneR);
library(seewave);

#<<<@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@>>>
#<<<@@@@@@@@@@@@@@@@@@@@@@@@   Auxiliary functions     @@@@@@@@@@@@@@@@@@@@@@@>>>
#<<<@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@>>>

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

download <- function(url, ...) {
  options(timeout=300)
  url <- trim(url)
  if (grepl('^https?://', url)) {
    if (.Platform$OS.type == "windows") {
      on.exit(suppressWarnings(setInternet2(use=FALSE)))    
	  if (grepl('^https://', url)) suppressWarnings(setInternet2(use=TRUE))	  
      download.file(url, ...)      
    } else {
      if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))
        options(download.file.extra = paste("-L", orig_extra_options))
      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }
      download.file(url, method = method, ...)
    }
  } else {  
    download.file(url, ...)
  }
}  

getExt <- function (path){
  parts <- strsplit(path, "\\.")[[1]]
  last <- parts[length(parts)]
  last
} 

#<<<@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@>>>
#<<<@@@@@@@@@@@@@@@@@@@@@@@@@@@   Main body    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@>>>
#<<<@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@>>>


print(paste("Number of lines:",length(readLines(infile)),sep=" "))

#Data input
## Detect if the in sound file is a remote file and if so download it
firstLineInFileInput=readLines(infile, n = 1, warn = FALSE)
if (length(readLines(infile))==1 && (toupper(substr(firstLineInFileInput, 1, 4))=="HTTP" | toupper(substr(firstLineInFileInput, 1, 3))=="FTP")){
    tempSoundFile <- paste(tempfile(),getExt(firstLineInFileInput),sep=".")
    download(firstLineInFileInput,tempSoundFile,mode="wb")
    infile <- tempSoundFile	
} 

#ACI Analysis
long <- readWave(infile);
f = long@samp.rate
long <- cutw(long, f=f, from=sstart, to=sstop, method="Wave");
aci <- ACI(long, f=f);

#2D-spectrogram of the time wave
png(spectro_plot)
spectro(long, f=f)
dev.off()

#frequency spectrum of the time wave
png(spec_plot)
spec(long, f=f)
dev.off()


#Dominant frequency of the time wave
png(dfreq_plot)
dfreq(long, f=f)
dev.off()

#Dominant frequency matrix
dfreqMatrix <- dfreq(long, f=f, plot=FALSE)
options(digits=16, width=160,max.print=length(dfreqMatrix))
sink(dfreq_matrix)
print(dfreqMatrix)
sink() 



#Oscilogram generation of PNGs
png(oscilo_plot)
oscillo(long,f=f)
dev.off()
