# setup the password as an environmental variable
source ~/.bash-profile

# just to check the progress of simulations on spartan
sshpass -e ssh twozniak@spartan.hpc.unimelb.edu.au
squeue -u twozniak
exit


# Upload files
sshpass -e scp oz_empirical/bsvars_data.rda twozniak@spartan.hpc.unimelb.edu.au:/data/projects/punim0093/bsvars_mcxs/
sshpass -e scp oz_empirical/*.R twozniak@spartan.hpc.unimelb.edu.au:/data/projects/punim0093/bsvars_mcxs/
sshpass -e scp oz_empirical/*.slurm twozniak@spartan.hpc.unimelb.edu.au:/data/projects/punim0093/bsvars_mcxs/

# Download and upload RData files
scp twozniak@spartan.hpc.unimelb.edu.au:/data/projects/punim0093/bsvars_mcxs/bsvars_lr_ex_p4.rda oz_empirical/

# working with svar_betel on spartan
#################################################
sshpass -e ssh twozniak@spartan.hpc.unimelb.edu.au
cd /data/projects/punim0093/bsvars_mcxs/

sbatch bsvars_ur_p12.slurm 
sbatch tax23.slurm
squeue -u twozniak

rm *.out
cat *65.out
cat *MR.slurm
tail -200 source_code/bsvar_sv.cpp
ls
ls results/
ls source_code/
exit
#

# install bsvars package on spartan
#################################################
sshpass -e ssh twozniak@spartan.hpc.unimelb.edu.au
sinteractive
module load R/4.2.1
R
devtools::install_git("https://github.com/bsvars/bsvars.git")
bsvars::bsvar
q("no")
exit

