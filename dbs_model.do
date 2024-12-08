* install traj
net from https://www.andrew.cmu.edu/user/bjones/traj
net install traj
* load data
import delimited "stata_gbtm.csv", clear
* determine number of latent groups
traj, var(dbs_3 dbs_9 dbs_18 dbs_24) indep(month1 month3 month6 month8) model(cnorm) min(100) max(2874) order(3)
matrix mv1 = 386.39445,-280.44999,18.59851,-0.40746,1001.52376
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(0) max(2874) order(3) start(mv1)
* Likelihood could not be computed at start values with all data points -> simpler model with 4 dates used to get starting values for model with all dates
* 1 group: BIC: -13672.34
traj, var(dbs_3 dbs_12 dbs_18 dbs_24) indep(month1 month4 month6 month8) model(cnorm) min(100) max(2874) order(3 3)
matrix mv2 = 95.40101,-156.09505,7.79494,-0.15049,1409.02198,-145.35538,8.32940,-0.18729,734.10941,89.41539,10.58461
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(3 3) start(mv2)
* 2 groups: BIC: -3074.07 
traj, var(dbs_3 dbs_12 dbs_18 dbs_24) indep(month1 month4 month6 month8) model(cnorm) min(100) max(2874) order(3 3 3)
matrix mv3 = -267.09270,-62.32769,2.52723,-0.05145,1069.25982,443.72346,-40.15005,0.90688,1600.31654,-316.13563,19.96844,-0.41911,550.28329,76.17448,1.35264,22.47288
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(3 3 3) start(mv3)
* 3 groups: BIC: -3050.59, (but one group has  4.2135%)
* 4 groups did not converge, always "warning: variance matrix is nonsymmetric or highly singular."

* altough 4.21% are not optimal, to compare with prep 3 groups are further analysed

* determine the shape of the trajectories
* 3 groups:
* start with (3 3 3) and decrease order of the polynomial for a group, when the part of the hightest order for the group is not significant ((Prob > |T|) > 0.05)
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(3 3 3) start(mv3)
* (3 3 3): BIC: -3050.59
matrix mv4 = -187.16919,-88.52189,2.00703,1268.57019,-21.76928,0.81206,-0.04574,1219.23394,-189.82597,7.77359,-0.12259,570.46882,74.81185,4.21350,20.97465
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(2 3 3) start(mv4)
* (2 3 3): BIC: -3047.70
matrix mv5 = -187.07258,-90.42225,2.26273,1268.55867,-21.80015,0.81193,1219.12953,-190.19853,7.78875,-0.12261,570.47614 ,74.69879,4.21708,21.08413
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(2 2 3) start(mv5)
* (2 2 3): BIC: -3044.83
matrix mv6 = -168.85414,-93.22942,2.35743,1215.75778,-1.60431, 1217.68507,-188.69508,7.68750,-0.12071,570.50367,74.95412,4.20725,20.83862
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(2 1 3) start(mv6)
* (2 1 3): BIC: -3042.17
matrix mv7 = -180.52320,-91.61970,2.30175,1346.50610,-28.28224,1217.38419,-189.67186,7.76731,571.19421,74.78866,4.21855,20.99279
traj, var(dbs_3-dbs_24) indep(month1-month8) model(cnorm) min(100) max(2874) order(2 1 2) start(mv7)
* (2 1 2): BIC: -3039.58
* all parameter have Prob > |T| < 0.05 -> stop, (2 1 2) final model
export delimited using F:\FU\FU\Bachelorarbeit\Code\groups_dbs.csv
trajplot, ci
