* install traj
net from https://www.andrew.cmu.edu/user/bjones/traj
net install traj
* load data
import delimited "stata_gbtm.csv", clear
* determine number of latent groups
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3)
* 1 group: BIC: -7798.52
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3)
* 2 groups: BIC: -7434.08
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3)
* 3 groups: BIC: -7258.89
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3 3)
* 4 groups: BIC: -7189.84
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3 3 3)
* 5 groups: BIC: -7153.39 
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 2)
* initial test for 4/5 groups fits better, but for comparison with other data, 2 very similar trajectories and a group with <5%
* -> models with 3 groups refined

* determine the shape of the trajectories
* 3 groups:
* start with (3 3 3) and decrease order of the polynomial for a group, when the part of the hightest order for the group is not significant ((Prob > |T|) > 0.05)
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3)
* (3,3,3): BIC: -7258.89
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 3)
* (3 2 3): BIC: -7256.09
matrix mv1 = 43.07968,-7.91319,0.37427,-0.00595,91.51776,-7.60849,0.16785,81.08851,4.04018,-0.38913,22.41971,64.15250,30.21042,5.63708
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 2) start(mv1)
* to get the same groups as previous model: use previous model to get starting values
* (3,2,2): BIC: -7253.69
matrix mv2 = 43.14332,-7.92050,0.37452,-0.00596,91.54410,-7.60139, 0.16772,92.22149, 0.22711,22.43681,64.22298,30.20672,5.57031
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 1) start(mv2)
* (3,2,1): BIC: -7251.42
traj, var(prep_adh_3-prep_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(2 2 1)
* (2,2,1): BIC: -7250.38
* all parameter have Prob > |T| < 0.05 -> stop, (2 2 1) final model
export delimited using F:\FU\FU\Bachelorarbeit\Code\groups_prep.csv
trajplot, ci
