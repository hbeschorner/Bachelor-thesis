# Bachelor thesis: Statistical modelling of HIV PrEP adherence based on multiple data sources

Supplementary code to recreate the plots in the bachelor thesis "Statistical modelling of HIV PrEP adherence based on multiple data sources".

## Installation

The environment.yml file can be used to load all necessary packages.

### Install Conda/Mamba
To create the environment from the environment.yml, follow the instructions to install [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html) or [mamba](https://mamba.readthedocs.io/en/latest/installation/mamba-installation.html).

### Downlad code
Dowload code by cloning git repository.
```
git clone https://github.com/hbeschorner/Bachelor-thesis
```
### Create the Conda Environment
To install all necessary packages create the environment from the environment.yml with
```
conda env create -f environment.yml
```
if the installation with the environment.yml does not work use:
```
conda create -n prep_adh_modelling -c conda-forge r-base=4.4.2 r-dplyr r-ggpubr r-readxl r-tidyr r-ggplot2 r-purr r-caret r-forcats r-magick r-shades r-pdftools
```
to create the conda environment

Activate the Conda Environment with:
```
activate prep_adh_modelling
```
### Set up stata
To use the plugin traj, downloaded it in stata with:
```
net from https://www.andrew.cmu.edu/user/bjones/traj
net install traj
```

## Input data
For creating the plots, the data from the MPYA study found on [harvard dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PPQKSW) is needed.

Download questionaire.xlsx, adherence.xlsx and dbs.xlsx into the working directory

## Generating plots
First run the R-script preprocessing_adhernce_data.R with:

```
Rscript preprocessing_adhernce_data.R
```

Afterwards create the models by running dsb_model.do, eam_model.do, pharm_model.do in stata

(trajplots from stata have to be formatted and exported to dbs_adherence_ci.pdf, eam_adherence_ci.pdf, pharm_adherence_ci.pdf for data_analysis.R, examples for the formatted stata graphs can be found in the stata_plots folder)

To generate the plots, run the R-scrips data_exploration.R and data_analysis.R with:
```
Rscript data_exploration.R
Rscript data_analysis.R
```
