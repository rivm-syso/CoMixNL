################################################################################
#
# Copyright 2022 Rijksinstituut voor Volksgezondheid en Milieu (RIVM).
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU Affero General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Masterscript for CoMixNL analysis
# 
# Steps 01 - 04 need to be run always
# Figures in steps 07 - 08 can be run independently using intermediate results 
# that are stored in ./results directory
#
################################################################################

source("./scripts/01_Load_packages.r")
source("./scripts/02_Load_surveydata.r")
source("./scripts/03_Load_additionaldata.r")
source("./scripts/04_Prepare_data.r")
source("./scripts/05_Analyze_characteristics_studypopulation.r")
source("./scripts/06_Analyze_contacts_studypopulation.r")
source("./scripts/07_Predict_contacts_studypopulation.r")
source("./scripts/08_Predict_contacts_generalpopulation.r")

