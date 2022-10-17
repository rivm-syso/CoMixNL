# CoMixNL

This repository contains the code developed and used for the analysis of the CoMix data for the Netherlands, consisting of:

a. the data used in the analysis, comprising both the CoMix data files as published on [Zenodo](https://dx.doi.org/10.5281/zenodo.4790347) and additional data used for reference

b. scripts and functions to reproduce the analysis results and figures

c. all figures and intermediate results on which they are based

d. article files in LaTeX

All code has been written in the programming language [R](https://www.r-project.org/about.html); see [Requirements](#requirements) for detailed specification.

## Usage

`scripts` contains numbered R files that reproduce the results when executed in order:

`00_Masterscript.r` runs all subsequent scripts

`01_Load_packages.r` loads the required R packages and functions

`02_Load_surveydata.r` loads the survey data from Zenodo using socialmixr or (if that fails) from the csv files in `data`

`03_Load_additionaldata.r` loads additional data needed for the analysis either from `data` (e.g. vaccination coverage) or by defining them in situ (e.g. holiday periods)

`04_Prepare_data.r` prepares data for analysis, such as defining identical age groups for all data sources

`05_Analyze_characteristics_studypopulation.r` analyzes participant data in terms of participation numbers and representativeness

`06_Analyze_contacts_studypopulation.r` analyzes number of non-household contacts per participant (fits are saved but not included in repository) and shows resulting fixed effects (stationary and non-stationary)

`07_Predict_contacts_studypopulation.r` calculates fitted results for study population and compares to observations (intermediate results saved and included in repository)

`08_Predict_contacts_generalpopulation.r` extrapolates fitted results to general population and compares to observations in pico survey (intermediate results saved and included in repository)


## <a name = "requirements"></a> Requirements

The code has been developed and runs under the RIVM R-Studio servers.

```
R version 4.1.3 (2022-03-10) One Push-Up 
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: Red Hat Cloud Infrastructure
```

Next to the R base packages, the following packages need to be installed

```
tidyverse
lubridate
mgcv
cowplot
xtable
```

The `socialmixr` package is optional and only needed when directly downloading data from Zenodo. On this system it takes about 6 hours to reproduce all results, and saving the fits requires 130 Mb of free space.


## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


## Feedback

If you encounter a clear bug, please file an issue with a minimal reproducible example on GitHub.