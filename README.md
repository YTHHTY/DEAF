# Electricity DEAF
code for paper titled “Evaluating electricity transmission and distribution efficiency using Data Envelopment Analysis Forest with feature importance”

# Paper Information
Tianhao Yi, Lisha Li, Zhiyong Li, Jiaxuan Zhang, Evaluating electricity transmission and distribution efficiency using Data Envelopment Analysis Forest with feature importance, Energy, Volume 330, 2025, 136580, ISSN 0360-5442, https://doi.org/10.1016/j.energy.2025.136580.

# Code Introduction
Refer to "Read me Programs.pdf" and "Read me Data.pdf" for more detailed information.

Main file for Monte Carlo simulation
1. Simulation_DEA.R
2. Simulation_SEDEA.R
3. Simulation_NRDEA.R
4. Simulation_BDEA.R
5. Simulation_DEAF.R

Production Function for DGP in Monte Carlo Simulation
1.Fun_CobbDouglas.R
2.Fun_ReducedForm.R
3.Fun_Translog.R

Main file for empirical purposes
1. One_DEA.R
2. One_SEDEA.R
3. One_NRDEA.R
4. One_BDEA.R
5. One_DEAF.R

Calculation file of feature importance in empirical analysis
1. Importance_DEAF_new.R
2. Importance_DEAF_old.R

Explanation of other documents
1. BCC.R & BCC_XX.R 
Used to calculate BCC models in BDEA and DEAF.

2. CCR.R & CCR_XX.R
Used to calculate CCR models in BDEA and DEAF.

3. GenerateData.R
DGP file for Monte Carlo simulation

4. Disturb.R
Used to artificially add perturbations to generate observation data based on raw data

5.LCV.R
The Likehood cross validation function is used to automatically calculate the most suitable bandwidth h in Bootstrap DEA & DEA Forest