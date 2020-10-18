#!/bin/sh
#PBS -V # export all environment variables to the batch job.
#PBS -d /gpfs/ts0/projects/Research_Project-186477/Probit2_submat/Replication/GSL_multivariate5
#PBS -q pq # submit to the serial queue
#PBS -l walltime=168:00:00 # Maximum wall time for the job.
#PBS -l nodes=1:ppn=1 # specify number of processors.
#PBS -m e -M g.katz@exeter.ac.uk # email me at job completion
export OMP_NUM_THREADS=1
module load R/3.6.0-foss-2019a

R CMD BATCH --vanilla ContinuousModel.R  
