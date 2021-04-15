#!/bin/bash
# embedded options to bsub - start with #BSUB # -- our name ---

#BSUB -J loglike_process_outputs

#BSUB -q hpc
#BSUB -W 16:00
#BSUB -R "rusage[mem=20GB]"
# -- Allocate 1 node, 1 core
#BSUB -n 1

#BSUB -u fba@aqua.dtu.dk
#BSUB -B
#BSUB -N

#BSUB -o Output_benthiswkf_%J.out 
#BSUB -e Error_benthiswkf_%J.err 



#R_exe=/appl/R/bin/R-3.4.1
R_exe=/appl/R/bin/R-3.6.3-mkl
$R_exe R CMD BATCH '--args a_year=2019' BenthisWorkflow2020_pel.r wk2019_pel.R.out &

# need to install first the R libraries for vmstools on the 'linuxsh' node
# e.g. first look after the R packages online on cran
# then  cd to /appl/R/bin 
# and then doing R-3.4.1 CMD INSTALL ~/benthis/doBy.tar.gz  (install.packages() in R console does not work with hpc)
# problem is we have sometimes to install by hand all the dependent packages......

# Alternatively, maybe it works to do
# install vmstools locally with e.g. 1- be in the right folder, 2- launch R and 3- type: 
# install.packages("vmstools_70.tar.gz", repos = NULL, type="source")

#Take care of changing the CRLF (i.e. \r\n) if Windows to LF (\n) only to Unix

# then run this shell with bsub < BenthisWorkflow2020.sh 


