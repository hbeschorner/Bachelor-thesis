* install traj
net from https://www.andrew.cmu.edu/user/bjones/traj
net install traj
* load data
import delimited "stata_gbtm.csv", clear
* determine number of latent groups
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3)
* 1 group: BIC: -2481.83 
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3)
* 2 groups: BIC: -1998.65
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3)
* 3 groups: BIC: -1962.60
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3 3)
* 4 groups: BIC: -1966.08 -> model worse
* -> model with 3 groups refined

* determine the shape of the trajectories
* 3 groups:
* start with (3 3 3) and decrease order of the polynomial for a group, when the part of the hightest order for the group is not significant ((Prob > |T|) > 0.05)
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 3 3)
* (3 3 3): BIC: -1962.60
matrix mv1 = -17.92389,121.94969,-20.83765,0.63807,292.28117,20.66305,-3.36818,597.64994,-52.19121,3.84483,-0.08589,205.27836,26.27927,19.49711,54.22361
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 3) start(mv1)
* to get the same groups as previous model: use previous model to get starting values
* (3 2 3): BIC: -1960.45
matrix mv2 = 188.82348,30.75311,-10.34624,0.34825,288.35723,19.71891,-1.88035,561.10610,-42.77349,2.74594,204.21849,28.13996,13.20545,58.65458
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 2) start(mv2)
* (3 2 2): BIC: -1958.29
matrix mv3 = 189.90053,30.35477,-10.28581,0.34825,283.83319,20.85775,-1.92723,481.19134,-16.52126,204.06027,28.21628,13.11764,58.66608
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 1) start(mv3)
* (3 2 1): BIC: -1957.28
matrix mv4 = 200.44418,26.53484,-9.95467,0.33859,310.66631,15.33812,410.99876,-2.81164,204.98173,28.11357,13.12035,58.76608
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 1 1) start(mv4)
* (3 1 1): does not converge -> use (3 2 1) even tough one parameter has ((Prob > |T|) > 0.05)
traj, var(pharm_adh_3-pharm_adh_24) indep(month1-month8) model(cnorm) min(0) max(100) order(3 2 1) start(mv3)
export delimited using F:\FU\FU\Bachelorarbeit\Code\groups_pharm.csv
trajplot, ci
