Online Study Data Cleaning
================
March 25, 2024

- [Set up](#set-up)
  - [Rename variables](#rename-variables)
  - [Assign subject IDs](#assign-subject-ids)
  - [Exclusions](#exclusions)
    - [Incomplete surveys](#incomplete-surveys)
    - [Duplicate emails](#duplicate-emails)
    - [Blocks of suspicious responses](#blocks-of-suspicious-responses)
    - [Catch questions](#catch-questions)
    - [Education outliers](#education-outliers)
  - [Export excluded subjects to separate file for
    cleaning.](#export-excluded-subjects-to-separate-file-for-cleaning)
  - [Number of subjects for analysis](#number-of-subjects-for-analysis)
- [Demographics](#demographics)
- [OSIQ](#osiq)
- [SAM](#sam)
- [BFI](#bfi)
- [BFAS](#bfas)
- [PHQ](#phq)
- [Merge clean dataframes](#merge-clean-dataframes)

This script takes the raw csv file downloaded from Qualtrics, organizes
the raw data and calculates total scores and reverse codes items when
needed, and then outputs the cleaned data.

To download the raw data from Qualtrics, select the survey “SAM Demo”,
and export the data using “Download Data Table” (not “Legacy Exporter”).
Download the data as a csv, and make sure “Download all fields” and “Use
choice text” are checked. Don’t check any of the options under “More
options”. Note that this file will not be correctly opened in Excel, so
don’t open it in Excel — read it straight into R.

Places that should be hard-coded in this script when running on a new
raw csv file:

- “date” variable at the top of the script, which is then used to
  read/create data files
- directory of SAM processing script

The files created by this script are:

- “yyyy-mm-dd-demo-varnames.csv”, which contains all of the raw labels
  in the csv file output by Qualtrics and the corresponding label in the
  cleaned datasheet

- “yyyy-mm-dd-demo-dict.csv”, which contains information about all the
  variables included in the cleaned datasheet

- “yyyy-mm-dd-demo-subjectID.xlsx”, which contains the assigned subject
  IDs and corresponding email addresses

- “yyyy-mm-dd-demo-clean.csv”, which contains all the cleaned data,
  ready for analysis

- “yyyy-mm-dd-demo-clean-caught.csv”, which contains cleaned data for
  subjects who failed the catch trials and were thus excluded from the
  cleaned data for analysis

<!-- ============================================================================= -->

# Set up

``` r
date = "2023-07-26"
```

``` r
# load packages
library(BBmisc)
library(lubridate)
library(writexl)
library(magrittr)
library(tidyverse)
```

``` r
# read in raw data downloaded from Qualtrics
df.raw = paste0(date, "-demo-raw.csv") %>% read.csv()
```

## Rename variables

``` r
# retrieve column names from raw file exported from Qualtrics
raw_column_name = names(df.raw)

# clean up column names 
clean_column_name = 
    c("start_date", "end_date", "response_type", "IPaddress", "progress",
      "duration", "finished", "recorded_date", "responseID",
      "last_name", "first_name", "recipient_email", "external_reference", 
      "location_latitude", "location_longitude", 
      "distribution_channel", "user_language", "consent",
      "gender", "age", "edu", "hand", "gta", "country", "country_text", 
      "osiq_s_1", "osiq_s_2", "osiq_s_3", "osiq_o_4", "osiq_s_5", "osiq_s_6",
      "osiq_o_7", "osiq_o_8", "osiq_s_9", "osiq_o_10", "osiq_s_11", "osiq_o_12",
      "osiq_s_13", "osiq_s_14", "osiq_o_15", "catch1", 
      "osiq_o_16", "osiq_o_17", "osiq_s_18",
      "osiq_o_19", "osiq_s_20", "osiq_o_21", "osiq_o_22", "osiq_s_23", "osiq_s_24",
      "osiq_o_25", "osiq_o_26", "osiq_s_27", "osiq_o_28", "osiq_s_29", "osiq_o_30",
      "sam_event_1", "sam_event_2", "sam_event_3", "sam_event_4", 
      "sam_event_5", "sam_event_6", "sam_event_7", "sam_event_8",
      "field_obs_1", "field_obs_2",
      "sam_semantic_1", "sam_semantic_2", "sam_semantic_3", 
      "sam_semantic_4", "sam_semantic_5", "sam_semantic_6",
      "sam_spatial_1", "sam_spatial_2", "sam_spatial_3", 
      "sam_spatial_4", "catch2", "sam_spatial_5", "sam_spatial_6",
      "sam_future_1", "sam_future_2", "sam_future_3", 
      "sam_future_4", "sam_future_5", "sam_future_6",
      "bfi_e_1", "bfi_e_6", "bfi_e_11", "bfi_e_16", 
      "bfi_e_21", "bfi_e_26", "bfi_e_31", "bfi_e_36",
      "bfas_o_10", "bfas_o_20", "bfas_o_30", "bfas_o_40", "bfas_o_50", 
      "bfas_o_60", "bfas_o_70", "bfas_o_80", "bfas_o_90", "bfas_o_100", 
      "bfi_a_2", "bfi_a_7", "bfi_a_12", "bfi_a_17", "bfi_a_22", 
      "bfi_a_27", "bfi_a_32", "bfi_a_37", "bfi_a_42",
      "bfas_i_5", "bfas_i_15", "bfas_i_35", "bfas_i_45", "bfas_i_55", 
      "bfas_i_65", "bfas_i_75", "bfas_i_85", "bfas_i_95",
      "bfi_c_3", "bfi_c_8", "bfi_c_13", "bfi_c_18", "bfi_c_23", 
      "bfi_c_28", "bfi_c_33", "bfi_c_38", "bfi_c_43",
      "bfi_n_4", "bfi_n_9", "bfi_n_14", "bfi_n_19", 
      "bfi_n_24", "bfi_n_29", "bfi_n_34", "bfi_n_39", 
      "bfi_o_5", "bfi_o_10", "bfi_o_15", "bfi_o_20", "bfi_o_25", 
      "bfi_o_30", "bfi_o_35", "bfi_o_40", "bfi_o_41", "bfi_o_44",
      "hsam", "sdam", 
      "health_tbi", "health_stroke", "health_dementia", "health_epilepsy", 
      "health_brainsurg", "health_cancer", "health_psychotic", "health_learning", 
      "health_alcdrug", "health_cog", "health_other", "health_other_text",
      "anxiety", "anxiety_type", "anxiety_tx", "anxiety_rx", "anxiety_rx_text", 
      "depression", "depression_episodes", 
      "depression_tx", "depression_rx", "depression_rx_text",
      "phq_1", "phq_2", "phq_3", "phq_4", "phq_5", 
      "phq_6", "phq_7", "phq_8", "phq_9", "phq_function",
      "covid_1", "covid_2", "covid_3",
      "text_catch", "retired", "job_ifNotRetired", "job_ifRetired", "job_mental",
      "email", "email_confirm", "do_not_contact", 
      "score", "source", "email_topics")

# create data frame that matches raw to clean column names  
varnames = cbind(raw_column_name, clean_column_name) %>% as.data.frame(stringsAsFactors = FALSE)
# write.csv(varnames, paste0(date, "-demo-varnames.csv"), row.names = FALSE)

# rename variables

df = df.raw

names(df) = mapValues(x = names(df),
                      from = raw_column_name,
                      to = clean_column_name)

# change class of all variables from factors to strings
df %<>% mutate_all(as.character)
```

## Assign subject IDs

``` r
# remove rows that don't correspond to subject data
df = df[-c(1,2),]

# assign subject IDs
df$subjectID = paste0("DEM", sprintf("%0.5d", 1:nrow(df)))

# export subject IDs and email addresses
subjectID = df %>% select(subjectID, email, gta, country)
# write_xlsx(subjectID, paste0(date, "-demo-subjectID.xlsx"))
```

## Exclusions

Before any exclusions, there are 37543 survey responses.

``` r
# remove variables that don't need to be in cleaned file
df = df %>% 
    select(-c(start_date, end_date, response_type, IPaddress, progress, 
              responseID, last_name, first_name, recipient_email, 
              external_reference, distribution_channel, user_language, 
              consent, score, source, email_topics))

# create copy of dataframe for cleaning excluded rows later
df.excluded = df
```

### Incomplete surveys

15528 subjects did not finish the entire survey and are now excluded.

``` r
df %<>% filter(finished == "True")
```

### Duplicate emails

``` r
exclude.email = df %>% filter(email != "")

exclude.email = exclude.email[duplicated(exclude.email$email), ]

df %<>% filter(! subjectID %in% exclude.email$subjectID)
```

Of the complete responses, 734 responses are duplicate emails and
excluded. For duplicates, only the first response is kept.

### Blocks of suspicious responses

Starting around November 2019, we noticed some suspicious response
patterns that are likely bots/spam.

Remove responses that were finished at the same time (rounded to the
nearest minute) (n = 48):

``` r
df$recorded_date %<>% 
  ymd_hms(tz = "EST") %>% 
  round_date(unit = "minute")

df = df[!(duplicated(df$recorded_date) | duplicated(df$recorded_date, fromLast = TRUE)), ]
```

There are also blocks of rows completed over the span of several hours
that have suspicious patterns of email addresses (e.g., gibberish, or
<a href="mailto:firstnamelastname###@gmail.com"
class="email">firstnamelastname###@gmail.com</a>). Closer visual
inspection of the data reveals that these sketchy email address
responses almost invariably leave what country they live in blank,
presumably because it’s the only demographics question that the
questionnaire doesn’t force a response to. The vast majority of
responses that look like they are valid have the country filled out. So,
by excluding rows that didn’t indicate a country of residence, hopefully
we can exclude all of the trolls. 1403 participants are excluded based
on blank country.

**Update, Sept. 19, 2022:** We started recruiting participants in the
GTA and found that when participants indicate that they’re from the GTA,
they’re skipped over the country question so it’s left blank. We want to
retain these participants, so instead of removing all participants with
blank country, I’ll remove only those who have blank country *and* did
not indicate they’re from the GTA (n = 16)

``` r
# df %<>% filter(country != "")
df %<>% 
  filter(!(gta == "No" & country == ""))
```

### Catch questions

There are 2 “catch” questions to make sure participants are paying
attention, where the question instructs them to select a specific
response. Remove participants who didn’t select the required response.

For catch 1, subjects are to select “Totally agree”. For catch 2,
subjects are to select “Neither agree nor disagree”.

In August 2018, we also added a text catch question that required
subjects to enter the answer to an arithmetic problem in a text box.

``` r
# catch 1
exclude.catch1 = df %>% 
  filter(catch1 != "Totally agree" | is.na(catch1) == TRUE) %>% 
  select(subjectID)

# catch 2
exclude.catch2 = df %>%
  filter(catch2 != "Neither Agree nor Disagree" | is.na(catch2) == TRUE) %>% 
  select(subjectID)

# text catch

df$text_catch %<>% tolower()

exclude.textcatch = df %>% 
  filter(recorded_date > "2018-08-13 12:00:00") %>% 
  filter(!grepl(pattern = "10|ten", x = text_catch)) %>%
  select(subjectID)

# remove subjects who failed at least one catch question

exclude.catch = rbind(exclude.catch1, exclude.catch2)

exclude.catch %<>% 
  rbind(exclude.textcatch) %>% 
  unique()

df %<>% filter(! subjectID %in% exclude.catch$subjectID)
```

After removing incomplete responses, duplicate emails, and responses
with blank countries, 1233 subjects failed catch question 1, 543 failed
catch question 2, and 259 subjects failed the text catch. A total of
1572 subjects are excluded for failing at least 1 of these 3 catch
questions.

### Education outliers

``` r
df$edu %<>% as.numeric()

exclude.edu = df %>% filter(edu < 9 | edu > 26)

df %<>% filter(! subjectID %in% exclude.edu$subjectID)
```

After excluding catch question fails, 778 report having either less than
9 or more than 26 years of education and are excluded.

## Export excluded subjects to separate file for cleaning.

``` r
df.excluded %<>% filter(! subjectID %in% df$subjectID)

# run separate cleaning script
# source("../cleaning/demo-cleaning-excluded.R")
```

## Number of subjects for analysis

After all exclusions, there are 17772 subjects left for analysis.

Check that the number of rows in the clean dataframe plus the number of
rows in the excluded dataframe equal the number of rows in the raw data:

``` r
nrow(df.raw) - 2 == nrow(df) + nrow(df.excluded)
```

    ## [1] TRUE

<!-- ============================================================================= -->

# Demographics

``` r
# select demographics variables
df.dem = df %>% 
  select(c(subjectID, recorded_date, do_not_contact, gender, age, edu, hand, gta,
           location_latitude, location_longitude, country, country_text, 
           field_obs_1, field_obs_2, hsam, sdam,
           retired, job_ifNotRetired, job_ifRetired, job_mental,
           starts_with("health"), starts_with("anxiety"), starts_with("depression"), starts_with(("covid"))))
```

<!-- ============================================================================= -->

# OSIQ

``` r
# create new dataframe that only contains OSIQ items
df.osiq = df %>% select(c(subjectID, starts_with("osiq")))

# replace text in responses with numbers
for (row in 1:nrow(df.osiq)) {
  for (col in 2:ncol(df.osiq)) {
    switch(df.osiq[row, col],
           "Totally disagree"           = {df.osiq[row, col] = 1},
           "Disagree somewhat"          = {df.osiq[row, col] = 2},
           "Neither agree nor disagree" = {df.osiq[row, col] = 3},
           "Agree somewhat"             = {df.osiq[row, col] = 4},
           "Totally agree"              = {df.osiq[row, col] = 5}
           )
  }
}

# change class of all variables from strings to numeric
for (i in 2:ncol(df.osiq)) {
  df.osiq[ , i] = df.osiq[ , i] %>% as.numeric()
}

# reverse coding
df.osiq$osiq_s_27r = 6 - df.osiq$osiq_s_27
df.osiq$osiq_s_27 = NULL

# domain scores
df.osiq$osiq_s = df.osiq %>% select(starts_with("osiq_s")) %>% rowMeans()
df.osiq$osiq_o = df.osiq %>% select(starts_with("osiq_o")) %>% rowMeans()
```

<!-- ============================================================================= -->

# SAM

``` r
# create new dataframe that only contains SAM items
df.sam = df %>% select(c(subjectID, starts_with("sam")))

# replace text in responses with numbers
for (row in 1:nrow(df.sam)) {
  for (col in 2:ncol(df.sam)) {
    switch(df.sam[row, col],
           "Strongly Disagree"           = {df.sam[row, col] = 1},
           "Disagree Somewhat"           = {df.sam[row, col] = 2},
           "Neither Agree nor Disagree"  = {df.sam[row, col] = 3},
           "Neither Agree  nor Disagree" = {df.sam[row, col] = 3},
           "Agree Somewhat"              = {df.sam[row, col] = 4},
           "Strongly Agree"              = {df.sam[row, col] = 5}
           )
  }
}

# change class of all variables from strings to numeric
for (i in 2:ncol(df.sam)) {
  df.sam[ , i] = df.sam[ , i] %>% as.numeric()
}

# reorder columns
df.sam = df.sam[ , order(names(df.sam))] %>% select(subjectID, everything())

# load script to calculate domain scores
source("P:/coding/r_functions/score_sam.R")
df.sam %<>% score_sam()

# reverse coding
sam.reverse = c("sam_event_1", "sam_event_2", 
                "sam_semantic_2", "sam_semantic_5", 
                "sam_spatial_3", "sam_spatial_4", "sam_future_6")

for (i in 1:length(sam.reverse)) {
  df.sam[ , (ncol(df.sam) + 1)] = NA # add an empty column for the reverse coded item
  names(df.sam)[ncol(df.sam)] = paste0(sam.reverse[i], "r") # rename empty column
  colToReverse = which(names(df.sam) == sam.reverse[i]) # retrieve index of item to be reversed
  df.sam[ , ncol(df.sam)] = 6 - df.sam[ , colToReverse] # reverse code the item
  df.sam[ , colToReverse] = NULL # remove the original item score
}

# reorder columns
df.sam = df.sam[ , order(names(df.sam))] %>% select(subjectID, everything())
df.sam %<>% select(
  subjectID,
  starts_with("sam_event"), starts_with("sam_future"), starts_with("sam_semantic"), starts_with("sam_spatial"),
  sam_epi, sam_fut, sam_sem, sam_spa
)
```

<!-- ============================================================================= -->

# BFI

``` r
# create new dataframe that only contains BFI items
df.bfi = df %>% select(c(subjectID, starts_with("bfi")))

# replace text in responses with numbers
for (row in 1:nrow(df.bfi)) {
  for (col in 2:ncol(df.bfi)) {
    switch(df.bfi[row, col],
           "Disagree strongly"           = {df.bfi[row, col] = 1},
           "Disagree a little"           = {df.bfi[row, col] = 2},
           "Neither agree nor disagree"  = {df.bfi[row, col] = 3},
           "Agree a little"              = {df.bfi[row, col] = 4},
           "Agree strongly"              = {df.bfi[row, col] = 5}
           )
  }
}

# change class of all variables from strings to numeric
for (i in 2:ncol(df.bfi)) {
  df.bfi[ , i] = df.bfi[ , i] %>% as.numeric()
}

# reverse coding
bfi.reverse = c("bfi_e_6", "bfi_e_21", "bfi_e_31",
                "bfi_a_2", "bfi_a_12", "bfi_a_27", "bfi_a_37",
                "bfi_c_8", "bfi_c_18", "bfi_c_23", "bfi_c_43",
                "bfi_n_9", "bfi_n_24", "bfi_n_34",
                "bfi_o_35", "bfi_o_41")

for (i in 1:length(bfi.reverse)) {
  df.bfi[ , (ncol(df.bfi) + 1)] = NA # add an empty column for the reverse coded item
  names(df.bfi)[ncol(df.bfi)] = paste0(bfi.reverse[i], "r") # rename empty column
  colToReverse = which(names(df.bfi) == bfi.reverse[i]) # retrieve index of item to be reversed
  df.bfi[ , ncol(df.bfi)] = 6 - df.bfi[ , colToReverse] # reverse code the item
  df.bfi[ , colToReverse] = NULL # remove the original item score
}

# reorder columns
df.bfi = df.bfi[ , order(names(df.bfi))] %>% select(subjectID, everything())

# domain scores
df.bfi$bfi_e = df.bfi %>% select(starts_with("bfi_e")) %>% rowMeans()
df.bfi$bfi_a = df.bfi %>% select(starts_with("bfi_a")) %>% rowMeans()
df.bfi$bfi_c = df.bfi %>% select(starts_with("bfi_c")) %>% rowMeans()
df.bfi$bfi_n = df.bfi %>% select(starts_with("bfi_n")) %>% rowMeans()
df.bfi$bfi_o = df.bfi %>% select(starts_with("bfi_o")) %>% rowMeans()
```

<!-- ============================================================================= -->

# BFAS

``` r
# create new dataframe that only contains BFAS items
df.bfas = df %>% select(c(subjectID, starts_with("bfas")))

# replace text in responses with numbers
for (row in 1:nrow(df.bfas)) {
  for (col in 2:ncol(df.bfas)) {
    switch(df.bfas[row, col],
           "Disagree strongly"           = {df.bfas[row, col] = 1},
           "Disagree a little"           = {df.bfas[row, col] = 2},
           "Neither agree nor disagree"  = {df.bfas[row, col] = 3},
           "Agree a little"              = {df.bfas[row, col] = 4},
           "Agree strongly"              = {df.bfas[row, col] = 5}
           )
  }
}

# change class of all variables from strings to numeric
for (i in 2:ncol(df.bfas)) {
  df.bfas[ , i] = df.bfas[ , i] %>% as.numeric()
}

# reverse coding
bfas.reverse = c("bfas_i_15", "bfas_i_45", "bfas_i_55", "bfas_i_85",
                 "bfas_o_50", "bfas_o_60", "bfas_o_80", "bfas_o_90")

for (i in 1:length(bfas.reverse)) {
  df.bfas[ , (ncol(df.bfas) + 1)] = NA # add an empty column for the reverse coded item
  names(df.bfas)[ncol(df.bfas)] = paste0(bfas.reverse[i], "r") # rename empty column
  colToReverse = which(names(df.bfas) == bfas.reverse[i]) # retrieve index of item to be reversed
  df.bfas[ , ncol(df.bfas)] = 6 - df.bfas[ , colToReverse] # reverse code the item
  df.bfas[ , colToReverse] = NULL # remove the original item score
}

# reorder columns
df.bfas = df.bfas[ , order(names(df.bfas))] %>% select(subjectID, everything())

# domain scores
df.bfas$bfas_oi = df.bfas %>% select(starts_with("bfas")) %>% rowMeans()
```

<!-- ============================================================================= -->

# PHQ

``` r
# create new dataframe that only contains PHQ items
df.phq = df %>% select(c(subjectID, starts_with("phq")))

# replace text in responses with numbers
for (row in 1:nrow(df.phq)) {
  for (col in 2:(ncol(df.phq)-1)) {
    switch(df.phq[row, col],
           "Not at all"              = {df.phq[row, col] = 0},
           "Several days"            = {df.phq[row, col] = 1},
           "More than half the days" = {df.phq[row, col] = 2},
           "Nearly every day"        = {df.phq[row, col] = 3}
           )
  }
}

# change class of all variables from strings to numeric
for (i in 2:(ncol(df.phq)-1)) {
  df.phq[ , i] = df.phq[ , i] %>% as.numeric()
}

# total score
df.phq$phq_total = df.phq %>%
  select(starts_with("phq")) %>%
  select(-phq_function) %>%
  rowSums()
```

<!-- ============================================================================= -->

# Merge clean dataframes

``` r
df.clean = Reduce(function(x, y) merge(x, y, by = "subjectID"), 
                  list(df.dem,
                       df.osiq,
                       df.sam,
                       df.bfi,
                       df.bfas,
                       df.phq))

# write.csv(df.clean, paste0(date, "-demo-clean.csv"), row.names = FALSE)
```

<!-- ============================================================================= -->
<!-- Data dictionary -->
