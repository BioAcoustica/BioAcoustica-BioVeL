#! /usr/bin/Rscript
library(getopt);
library(tuneR);
library(seewave);

spec = matrix(c(
  'infile', 'i', 1, "character",
  'sstart', 's', 2, "double",
  'sstop',  't', 2, "double"
), byrow=TRUE, ncol=4);
opt = getopt(spec);

download.file(opt$infile, destfile="temp.wav");
long <- readWave("temp.wav");
f = long@samp.rate
if (!(is.null(opt$sstart) & is.null(opt$sstop))) {
  long <- cutw(long, f=f, from=opt$sstart, to=opt$sstop, method="Wave");
}
aci <- ACI(long, f=f);
aci;
