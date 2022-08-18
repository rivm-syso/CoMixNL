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
# Load additional data
#
# - Vaccination coverage over time by age group
#   (source: https://www.rivm.nl/covid-19-vaccinatie/cijfers-vaccinatieprogramma,
#    accessed April 2022)
# - Start of vaccination campaign in general population by age group
#   (source: Twitter @hugodejonge, Minister of Health 2017-2022)
# - Fraction of high risk population by age group
#   (source: https://www.nivel.nl/sites/default/files/bestanden/1003980.pdf)
# - Population distribution by sex and age for 2020 and 2021
#   (source: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/7461BEV/)
# - General holidays and schoolholidays in 2020 and 2021
#   (source: https://opendata.rijksoverheid.nl/v1/sources/rijksoverheid/infotypes/schoolholidays/schoolyear/2020-2021?output=html)
#
################################################################################


vaccination_coverage <- readRDS("./data/vaccination_coverage.rds")

vaccination_start_dates <- read_tsv("./data/startvaccination_date_birthyear.txt") 

high_risk <- tibble(age = 0:99, 
                    high_risk = c(rep(0.031, 16), rep(0.046, 2), rep(0.073, 32), rep(0.2, 10), rep(0.309, 5), rep(0.509, 35)))

population_data_2020 <- readRDS("./data/populationNL_2020.rds")

population_data_2021 <- readRDS("./data/populationNL_2021.rds")

schoolholidays <- c(seq.Date(as.Date("2020-04-25"), as.Date("2020-05-03"), by = "day"),
                    seq.Date(as.Date("2020-07-04"), as.Date("2020-08-30"), by = "day"),
                    seq.Date(as.Date("2020-10-17"), as.Date("2020-10-25"), by = "day"),
                    seq.Date(as.Date("2020-12-19"), as.Date("2021-01-03"), by = "day"),
                    seq.Date(as.Date("2021-02-20"), as.Date("2021-02-28"), by = "day"),
                    seq.Date(as.Date("2021-05-01"), as.Date("2021-05-09"), by = "day"),
                    seq.Date(as.Date("2021-07-10"), as.Date("2021-09-05"), by = "day"))

holidays <- c(as.Date("2020-04-12"), as.Date("2020-04-13"), as.Date("2020-04-27"), as.Date("2020-05-05"), as.Date("2020-05-21"), 
              as.Date("2020-05-31"), as.Date("2020-06-01"), as.Date("2020-12-25"), as.Date("2020-12-26"),
              as.Date("2021-01-01"), as.Date("2021-04-04"), as.Date("2021-04-05"), as.Date("2021-04-27"), as.Date("2021-05-05"), 
              as.Date("2021-05-13"), as.Date("2021-05-23"), as.Date("2021-05-24"))


