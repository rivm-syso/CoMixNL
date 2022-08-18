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
# Load survey data
#
# from Zenodo using socialmixr or (if that fails)
# from csv files in format of socialcontactdata.org
#
################################################################################

# First try Zenodo, will result in a "try-error" when Zenodo record does not exist
# or when socialmixr is not installed
# TO DO: this is a non-existing Zenodo record, to be replaced by CoMixNL record
survey <- try(get_survey("https://zenodo.org/record/478559"), silent = TRUE)

if (class(survey) == "survey") {

  # note that household and survey day data files are not downloaded by socialmixr
  participant_data <- survey$participants
  contact_data <- survey$contacts
  
} else {
  
  participant_common <- read_csv(
    file = "./data/CoMix_nl_participant_common.csv",
    col_types = cols(
      .default = col_character(),
      part_id = col_integer(),
      part_age = col_integer()
    )
  )

  participant_extra <- read_csv(
    file = "./data/CoMix_nl_participant_extra.csv",
    col_types = cols(
      .default = col_character(),
      survey_round = col_integer(),
      part_id = col_integer(),
      date = col_date("%Y-%m-%d"),
      survey_date = col_date("%Y-%m-%d"),
      part_vacc = col_integer(),
      hh_size = col_integer()
    )
  )

  contact_common <- read_csv(
    file = "./data/CoMix_nl_contact_common.csv",
    col_types = cols(
      .default = col_character(),
      part_id = col_integer(),
      cnt_age_est_min = col_integer(),
      cnt_age_est_max = col_integer(),
      cnt_home = col_logical(),
      cnt_work = col_logical(),
      cnt_school = col_logical(),
      cnt_transport = col_logical(),
      cnt_leisure = col_logical(),
      cnt_otherplace = col_logical()
    )
  )

  contact_extra <- read_csv(
    file = "./data/CoMix_nl_contact_extra.csv",
    col_types = cols(
      .default = col_character(),
      survey_round = col_integer(),
      date = col_date("%Y-%m-%d"),
      cnt_bar_rest = col_logical(),
      cnt_health_facility = col_logical(),
      cnt_household = col_logical(),
      cnt_inside = col_logical(),
      cnt_other_house = col_logical(),
      cnt_other_place = col_logical(),
      cnt_outside = col_logical(),
      cnt_outside_other = col_logical(),
      cnt_public_market = col_logical(),
      cnt_public_transport = col_logical(),
      cnt_salon = col_logical(),
      cnt_shop = col_logical(),
      cnt_sport = col_logical(),
      cnt_supermarket = col_logical(),
      cnt_worship = col_logical(),
      cnt_phys = col_logical(),
      cnt_prec_1_and_half_m_plus = col_logical(),
      cnt_prec_mask = col_logical(),
      cnt_prec_prefer_not_to_say = col_integer()
    )
  )

  household_common <- read_csv(
    file = "./data/CoMix_nl_hh_common.csv",
    col_types = cols(
      .default = col_character(),
      hh_size = col_integer()
    )
  )

  household_extra <- read_csv(
    file = "./data/CoMix_nl_hh_extra.csv",
    col_types = cols(
      .default = col_character(),
      survey_round = col_integer(),
      date = col_date("%Y-%m-%d")
    )
  )

  survey_day <- read_csv(
    file = "./data/CoMix_nl_sday.csv",
    col_types = cols(
      .default = col_character(),
      part_id = col_integer(),
      month = col_integer(),
      year = col_integer(),
      dayofweek = col_integer(),
      day = col_integer(),
      wave = col_integer()
    )
  )

  # Join common and extra files
  participant_data <- participant_common %>% left_join(participant_extra)
  contact_data <- contact_common %>% left_join(contact_extra)
  household_data <- household_common %>% left_join(household_extra)

  # Remove common and extra files (not needed anymore)
  rm(participant_common)
  rm(participant_extra)
  rm(contact_common)
  rm(contact_extra)
  rm(household_common)
  rm(household_extra)
}
