On the relationship between episodic autobiographical memory and spatial
navigation
================
Carina Fan, Hervé Abdi, & Brian Levine
Last updated: March 25, 2024

- [Set up](#set-up)
  - [Exclusions](#exclusions)
- [Demographics](#demographics)
- [Factor structure of the Survey of Autobiographical Memory
  (SAM)](#factor-structure-of-the-survey-of-autobiographical-memory-sam)
- [Relationship between SAM and Object-Spatial Imagery Questionnaire
  (OSIQ)](#relationship-between-sam-and-object-spatial-imagery-questionnaire-osiq)
  - [Univariate analysis (linear
    regression)](#univariate-analysis-linear-regression)
  - [Multivariate analysis (partial least squares
    correlation)](#multivariate-analysis-partial-least-squares-correlation)
- [Supplemental analyses](#supplemental-analyses)
  - [Factor structure of the SAM in two independent
    samples](#factor-structure-of-the-sam-in-two-independent-samples)
    - [2013 dataset](#2013-dataset)
    - [2019 dataset](#2019-dataset)
  - [Relationship betwen SAM and
    OSIQ](#relationship-betwen-sam-and-osiq)
    - [Replication in an independent sample (2019
      dataset)](#replication-in-an-independent-sample-2019-dataset)
  - [Gender differences](#gender-differences)
  - [Personality effects](#personality-effects)
    - [Regressions of OSIQ on SAM](#regressions-of-osiq-on-sam)
    - [BFI and SAM](#bfi-and-sam)
  - [Depression effects](#depression-effects)
- [Session info](#session-info)

This file contains the code and output from all the analyses reported in
the main text and supplemental materials of the manuscript.

# Set up

``` r
# load packages
library(corrplot)
library(ExPosition)
library(gplots)
library(gridExtra)
library(lmSupport)
library(psych)
# devtools::install_github("https://github.com/HerveAbdi/PTCA4CATA")
library(PTCA4CATA)
library(readxl)
library(reshape)
library(TInPosition)
library(wrapr)
library(magrittr)
library(tidyverse)
```

``` r
# colour codes for plots 
colourCodeicor = colorRampPalette(c("blue", "yellow", "red"))(n = 299)
colourCode.sam = c("deepskyblue", "darkolivegreen", "firebrick3", "darkorchid3")
colourCode.osiq = c("darkgoldenrod", "slateblue")
colourCode.bfi = c("orange1", "lightsalmon1", "navajowhite3", "gold1", "peachpuff3")
```

``` r
# load data
df.raw = read.csv("../../data/old/2019-10-03-demo-raw.csv")
df.clean = read.csv("../../data/old/2019-10-03-demo-clean.csv", na.strings = "")

# number of responses recorded
nrow(df.raw)
```

    ## [1] 18189

``` r
# number of responses after preliminary cleaning
nrow(df.clean)
```

    ## [1] 10168

## Exclusions

``` r
# names of health variables to exclude based on
neuroNames = df.clean %>% 
  select(starts_with("health_")) %>% 
  select(-c("health_other", "health_other_text")) %>% 
  names()

# number of participants endorsing each condition
df.neuro = df.clean %>% select(neuroNames)
df.neuro %<>% mutate_all(as.character)
sapply(df.neuro, function(x) sum(str_count(x, "Yes"), na.rm = TRUE))
```

    ##       health_tbi    health_stroke  health_dementia  health_epilepsy 
    ##              465               35               20              155 
    ## health_brainsurg    health_cancer health_psychotic  health_learning 
    ##               50              156              147              637 
    ##   health_alcdrug       health_cog 
    ##              777              502

``` r
# column indices of variables to exclude 
neuroIndices = which(names(df.clean) %in% neuroNames)

# initialize vector of subjects with neuro conditions 
neuroSubjects = vector(mode = "character")

## if subject responded Yes to any neuro condition, 
## add their row number to neuroSubject
for (row in 1:nrow(df.clean)) {
  for (col in 1:length(neuroIndices)) {
    if (is.na(df.clean[row, neuroIndices[col]])) {
      next
    }
   else if (df.clean[row, neuroIndices[col]] == "Yes") {
      neuroSubjects = append(neuroSubjects, row)
    }
  }
}

# number of subjects to exclude based on neurological history
length(unique(neuroSubjects)) 
```

    ## [1] 2234

``` r
# number of subjects to exclude based on PHQ (scores >= 20)
df.clean$phq_total %<>% as.character() %>% as.numeric()

df.clean %>%
  filter(phq_total >= 20) %>%
  nrow()
```

    ## [1] 532

``` r
# exclude subjects based on health conditions
df = df.clean %>% filter(!row_number() %in% neuroSubjects) 

# exclude subjects based on PHQ
df %<>% filter(phq_total < 20)

# number of participants after all exclusions
nrow(df)
```

    ## [1] 7487

<!-- ======================================================================= -->

# Demographics

Age and education:

``` r
df %>% 
  summarise(age_mean = mean(age),
            age_sd = sd(age),
            edu_mean = mean(edu),
            edu_sd = sd(edu))
```

    ##   age_mean   age_sd edu_mean  edu_sd
    ## 1 39.99826 14.23142 16.92106 3.14841

``` r
# age range
range(df$age)
```

    ## [1] 18 85

Gender:

``` r
df %>% 
  group_by(gender) %>% 
  summarise(n = n())
```

    ## # A tibble: 3 × 2
    ##   gender                   n
    ##   <chr>                <int>
    ## 1 Female                4954
    ## 2 Male                  2451
    ## 3 Prefer not to answer    82

<!-- ======================================================================= -->

# Factor structure of the Survey of Autobiographical Memory (SAM)

``` r
# select SAM items
df.sam = df %>%
  select(gender, starts_with("sam")) %>%
  select(-c("sam_epi", "sam_sem", "sam_spa", "sam_fut"))

# rename variables
names(df.sam) = c(
  "gender",
  paste0("E", seq(1, 8)),
  paste0("F", seq(1, 6)),
  paste0("M", seq(1, 6)),
  paste0("S", seq(1, 6))
)

# split dataframe by gender for supplemental analyses
df.sam.female = df.sam %>% filter(gender == "Female")
df.sam.male   = df.sam %>% filter(gender == "Male")

# remove gender column from dataframes
df.sam$gender        = NULL
df.sam.female$gender = NULL
df.sam.male$gender   = NULL

# assign colours to variables
colour.sam = as.vector(rep("", ncol(df.sam)))

for (i in 1:ncol(df.sam)) {
  switch(substr(names(df.sam)[i], 1, 1),
         E = {colour.sam[i] = colourCode.sam[1]},
         F = {colour.sam[i] = colourCode.sam[2]},
         M = {colour.sam[i] = colourCode.sam[3]},
         S = {colour.sam[i] = colourCode.sam[4]})
}

# convert to numeric
df.sam %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)
df.sam.female %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)
df.sam.male %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)
```

``` r
pca.sam = epPCA(
  df.sam,
  scale = FALSE,
  center = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
pca.sam$ExPosition.Data$t
```

    ##  [1] 27.1822652 12.9353156  9.4451116  6.5069597  5.1960259  3.5370823
    ##  [7]  3.2514153  2.8217797  2.6479445  2.5728325  2.2208786  2.1807472
    ## [13]  2.0180007  1.9317582  1.8498197  1.6965680  1.6226651  1.5750722
    ## [19]  1.4483770  1.2756815  1.1509422  1.1053306  1.0632070  1.0333022
    ## [25]  0.9641271  0.7667903

<!-- ======================================================================= -->

# Relationship between SAM and Object-Spatial Imagery Questionnaire (OSIQ)

## Univariate analysis (linear regression)

``` r
# select SAM and OSIQ items
df.sam.osiq = df %>%
  select(starts_with("sam"),
         osiq_s_1, osiq_s_9, osiq_s_13, osiq_s_14, osiq_s_18, osiq_s_27r, osiq_s_29,
         osiq_o_4, osiq_o_12, osiq_o_17, osiq_o_22, osiq_o_25, osiq_o_28, osiq_o_30)

## convert to numeric
df.sam.osiq %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)

# calculate OSIQ domain scores based on mini OSIQ items

## object
df.sam.osiq$osiq_mini_o =
  df.sam.osiq %>%
  select(starts_with("osiq_o")) %>%
  rowMeans()

## spatial
df.sam.osiq$osiq_mini_s =
  df.sam.osiq %>%
  select(starts_with("osiq_s")) %>%
  rowMeans()

# calculate SAM domain scores based on raw item scores

## episodic
df.sam.osiq$sam_epi_raw = df.sam.osiq %>%
  select(starts_with("sam_event")) %>%
  rowMeans()

## spatial
df.sam.osiq$sam_spa_raw = df.sam.osiq %>%
  select(starts_with("sam_spatial")) %>%
  rowMeans()

## semantic
df.sam.osiq$sam_sem_raw = df.sam.osiq %>%
  select(starts_with("sam_semantic")) %>%
  rowMeans()

## future
df.sam.osiq$sam_fut_raw = df.sam.osiq %>%
  select(starts_with("sam_future")) %>%
  rowMeans()
```

``` r
# SAM-episodic
model_samE_osiq = lm(sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
model_samE_osiq %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2873 -0.4851 -0.1213  0.3652  3.6353 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.621282   0.029615  20.978  < 2e-16 ***
    ## osiq_mini_o 0.519846   0.009080  57.255  < 2e-16 ***
    ## osiq_mini_s 0.057974   0.009282   6.246 4.44e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7307 on 7484 degrees of freedom
    ## Multiple R-squared:  0.3331, Adjusted R-squared:  0.3329 
    ## F-statistic:  1869 on 2 and 7484 DF,  p-value: < 2.2e-16

``` r
model_samE_osiq %>% modelEffectSizes()
```

    ## lm(formula = sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept)  234.9864  1   0.0555     NA
    ## osiq_mini_o 1750.3216  1   0.3046 0.2921
    ## osiq_mini_s   20.8300  1   0.0052 0.0035
    ## 
    ## Sum of squared errors (SSE): 3996.1
    ## Sum of squared total  (SST): 5991.8

``` r
# SAM-spatial
model_samS_osiq = lm(sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
model_samS_osiq %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2805 -0.5937  0.0880  0.6208  2.3522 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.04161    0.03408   59.91   <2e-16 ***
    ## osiq_mini_o  0.08933    0.01045    8.55   <2e-16 ***
    ## osiq_mini_s  0.40760    0.01068   38.16   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8408 on 7484 degrees of freedom
    ## Multiple R-squared:  0.1948, Adjusted R-squared:  0.1946 
    ## F-statistic: 905.5 on 2 and 7484 DF,  p-value: < 2.2e-16

``` r
model_samS_osiq %>% modelEffectSizes()
```

    ## lm(formula = sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept) 2537.5268  1   0.3242     NA
    ## osiq_mini_o   51.6792  1   0.0097 0.0079
    ## osiq_mini_s 1029.6417  1   0.1629 0.1567
    ## 
    ## Sum of squared errors (SSE): 5290.4
    ## Sum of squared total  (SST): 6570.6

``` r
# SAM-future
model_samF_osiq = lm(sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
model_samF_osiq %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3709 -0.5485 -0.0209  0.5153  3.4845 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.476181   0.032511   14.65   <2e-16 ***
    ## osiq_mini_o 0.838945   0.009967   84.17   <2e-16 ***
    ## osiq_mini_s 0.127483   0.010190   12.51   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8022 on 7484 degrees of freedom
    ## Multiple R-squared:  0.526,  Adjusted R-squared:  0.5259 
    ## F-statistic:  4153 on 2 and 7484 DF,  p-value: < 2.2e-16

``` r
model_samF_osiq %>% modelEffectSizes()
```

    ## lm(formula = sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept)  138.0414  1   0.0279     NA
    ## osiq_mini_o 4558.6421  1   0.4863 0.4486
    ## osiq_mini_s  100.7235  1   0.0205 0.0099
    ## 
    ## Sum of squared errors (SSE): 4815.8
    ## Sum of squared total  (SST): 10160.9

``` r
# SAM-semantic
model_samM_osiq = lm(sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
model_samM_osiq %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.23700 -0.55986 -0.01788  0.54048  2.58801 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.160709   0.032117  67.275   <2e-16 ***
    ## osiq_mini_o 0.157589   0.009847  16.004   <2e-16 ***
    ## osiq_mini_s 0.093690   0.010066   9.308   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7925 on 7484 degrees of freedom
    ## Multiple R-squared:  0.05634,    Adjusted R-squared:  0.05609 
    ## F-statistic: 223.4 on 2 and 7484 DF,  p-value: < 2.2e-16

``` r
model_samM_osiq %>% modelEffectSizes()
```

    ## lm(formula = sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept) 2842.2158  1   0.3769     NA
    ## osiq_mini_o  160.8504  1   0.0331 0.0323
    ## osiq_mini_s   54.4018  1   0.0114 0.0109
    ## 
    ## Sum of squared errors (SSE): 4699.8
    ## Sum of squared total  (SST): 4980.4

## Multivariate analysis (partial least squares correlation)

``` r
# select mini OSIQ items
df.osiq = df %>%
  select(osiq_s_1, osiq_s_9, osiq_s_13, osiq_s_14, osiq_s_18, osiq_s_27r, osiq_s_29,
         osiq_o_4, osiq_o_12, osiq_o_17, osiq_o_22, osiq_o_25, osiq_o_28, osiq_o_30)

## convert to numeric
df.osiq %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)

# rename variables
names(df.osiq) = c(
  "s1", "s9", "s13", "s14", "s18", "s27", "s29",
  "o4", "o12", "o17", "o22", "o25", "o28", "o30"
)

# assign colours to variables
colour.osiq = as.vector(rep("", ncol(df.osiq)))

for(i in 1:ncol(df.osiq)) {
  switch(substr(names(df.osiq)[i], 1, 1),
         "o" = {colour.osiq[i] = colourCode.osiq[1]},
         "s" = {colour.osiq[i] = colourCode.osiq[2]})
}
```

``` r
plsc.sam.osiq = tepPLS(
  DATA1 = df.osiq,
  DATA2 = df.sam,
  center1 = TRUE, scale1 = FALSE,
  center2 = TRUE, scale2 = FALSE,
  make_design_nominal = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
plsc.sam.osiq$TExPosition.Data$t
```

    ##  [1] 8.927432e+01 9.851861e+00 5.024150e-01 1.711771e-01 6.967871e-02
    ##  [6] 6.187196e-02 3.915539e-02 8.997989e-03 7.230080e-03 5.132297e-03
    ## [11] 3.441175e-03 2.248993e-03 1.664889e-03 8.039029e-04

<!-- ======================================================================= -->

# Supplemental analyses

## Factor structure of the SAM in two independent samples

### 2013 dataset

``` r
# load SAM data from independent sample
df.og = read_excel("../../../../damp/SAMOriginalData_FinalSampleNof598.xls")

# select SAM items
df.og.sam = df.og[ , which(colnames(df.og) == 'E1') : which(colnames(df.og) == 'Sp20')]

# IDs and n for subtables
domainNames = wrapr::qc(Episodic, Future, Semantic, Spatial) 
domainNamesVar = wrapr::qc(Episodic, Future, Semantic, sPatial) 
posUp = sapply(strsplit(domainNamesVar, ''), function(a) which(a %in% LETTERS)[1])
domainKeys = substr(domainNamesVar, posUp, posUp)
domainNs = c(42, 16, 24, 20)
domains = data.frame(domainNames, domainNamesVar, domainKeys, domainNs, colourCode.sam,
                     row.names = domainNames,
                     stringsAsFactors = FALSE)

# rename variables
colnames(df.og.sam) = paste0(rep(domains$domainKeys, domains$domainNs),
                             unlist(sapply(domains$domainNs,function(x){1:x})))

# keep only items from final published questionnaire
keepSEpisodic = c(9, 10, 13, 21, 28, 35, 37, 39)
keepSFuture = c(1, 3, 5, 7, 2, 15)
keepSSemantic = c(4, 5, 11, 12, 13, 22)
keepSSpatial = c(7, 3, 5, 1, 12, 17)
keepMini = c(keepSEpisodic, 
             42 + keepSFuture, 
             42 + 16 + keepSSemantic, 
             42 + 16 + 24 + keepSSpatial)
df.og.sam = df.og.sam[ , keepMini]

# rename variables
names(df.og.sam) = c(
  paste0("E", seq(1, 8)),
  paste0("F", seq(1, 6)),
  paste0("M", seq(1, 6)),
  paste0("S", seq(1, 6))
)
```

``` r
pca.sam.og = epPCA(
  df.og.sam,
  scale = FALSE,
  center = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
pca.sam.og$ExPosition.Data$t
```

    ##  [1] 26.1882410 10.0290590  8.2628908  5.6861360  5.0980504  4.2964106
    ##  [7]  3.5786442  3.3531867  3.0366926  2.7313925  2.6491799  2.4379023
    ## [13]  2.2963312  2.2757362  2.1655517  2.0237197  1.8568783  1.7813555
    ## [19]  1.6456983  1.5889981  1.4942255  1.2890033  1.2336632  1.0790063
    ## [25]  0.9807764  0.9412703

### 2019 dataset

``` r
# load SAM data from independent sample
df.et = read.csv("../../../baytourET/data/baytourET_samscreen/2020-06-23-baytourET-samscreen-clean.csv")

# number of participants
nrow(df.et)
```

    ## [1] 535

``` r
# age 
mean(df.et$age)
```

    ## [1] 26.56636

``` r
sd(df.et$age)
```

    ## [1] 8.906652

``` r
range(df.et$age)
```

    ## [1] 18 72

``` r
# education
mean(df.et$edu)
```

    ## [1] 16.95421

``` r
sd(df.et$edu)
```

    ## [1] 2.673608

``` r
range(df.et$edu)
```

    ## [1]  9 26

``` r
# gender
df.et %>% 
  group_by(gender) %>% 
  summarise(n  = n())
```

    ## # A tibble: 3 × 2
    ##   gender                   n
    ##   <chr>                <int>
    ## 1 Female                 383
    ## 2 Male                   146
    ## 3 Prefer not to answer     6

``` r
# select SAM items
df.sam.et = df.et %>% 
  select(starts_with("sam"), -c(sam_epi, sam_sem, sam_spa, sam_fut))

# rename variables
names(df.sam.et) = c(
  paste0("E", seq(1, 8)),
  paste0("F", seq(1, 6)),
  paste0("M", seq(1, 6)),
  paste0("S", seq(1, 6))
)
```

``` r
pca.sam.et = epPCA(
  df.sam.et,
  scale = FALSE,
  center = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
pca.sam.et$ExPosition.Data$t
```

    ##  [1] 34.8666019 10.0052046  7.1307614  5.1469282  4.7371166  3.8241982
    ##  [7]  3.2175367  2.7782534  2.6903859  2.4115426  2.1974640  2.0699396
    ## [13]  2.0098707  1.9827260  1.8859235  1.6472253  1.5477675  1.4012551
    ## [19]  1.3596381  1.2987923  1.2275738  1.1075358  0.9881647  0.9781950
    ## [25]  0.8083431  0.6810561

## Relationship betwen SAM and OSIQ

### Replication in an independent sample (2019 dataset)

#### Univariate analysis (linear regression)

``` r
# select SAM and OSIQ items
df.sam.osiq.et = df.et %>%
  select(starts_with("sam"),
         osiq_s_1, osiq_s_9, osiq_s_13, osiq_s_14, osiq_s_18, osiq_s_27r, osiq_s_29,
         osiq_o_4, osiq_o_12, osiq_o_17, osiq_o_22, osiq_o_25, osiq_o_28, osiq_o_30)

## convert to numeric
df.sam.osiq.et %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)

# calculate OSIQ domain scores based on mini OSIQ items

## object
df.sam.osiq.et$osiq_mini_o =
  df.sam.osiq.et %>%
  select(starts_with("osiq_o")) %>%
  rowMeans()

## spatial
df.sam.osiq.et$osiq_mini_s =
  df.sam.osiq.et %>%
  select(starts_with("osiq_s")) %>%
  rowMeans()

# calculate SAM domain scores based on raw item scores

## episodic
df.sam.osiq.et$sam_epi_raw = df.sam.osiq.et %>%
  select(starts_with("sam_event")) %>%
  rowMeans()

## spatial
df.sam.osiq.et$sam_spa_raw = df.sam.osiq.et %>%
  select(starts_with("sam_spatial")) %>%
  rowMeans()

## semantic
df.sam.osiq.et$sam_sem_raw = df.sam.osiq.et %>%
  select(starts_with("sam_semantic")) %>%
  rowMeans()

## future
df.sam.osiq.et$sam_fut_raw = df.sam.osiq.et %>%
  select(starts_with("sam_future")) %>%
  rowMeans()
```

``` r
# SAM-episodic
model_samE_osiq.et = lm(sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
model_samE_osiq.et %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.33249 -0.46559  0.07985  0.46831  3.04751 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.77036    0.14744   5.225 2.51e-07 ***
    ## osiq_mini_o  0.66127    0.03597  18.383  < 2e-16 ***
    ## osiq_mini_s  0.09053    0.04077   2.220   0.0268 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7175 on 532 degrees of freedom
    ## Multiple R-squared:  0.4333, Adjusted R-squared:  0.4311 
    ## F-statistic: 203.4 on 2 and 532 DF,  p-value: < 2.2e-16

``` r
model_samE_osiq.et %>% modelEffectSizes()
```

    ## lm(formula = sam_epi_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Coefficients
    ##                  SSR df pEta-sqr dR-sqr
    ## (Intercept)  14.0542  1   0.0488     NA
    ## osiq_mini_o 173.9656  1   0.3884 0.3600
    ## osiq_mini_s   2.5376  1   0.0092 0.0053
    ## 
    ## Sum of squared errors (SSE): 273.9
    ## Sum of squared total  (SST): 483.3

``` r
# SAM-spatial
model_samS_osiq.et = lm(sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
model_samS_osiq.et %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.24605 -0.47544  0.05358  0.48407  1.84993 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.72543    0.15118   11.41  < 2e-16 ***
    ## osiq_mini_o  0.16564    0.03689    4.49 8.72e-06 ***
    ## osiq_mini_s  0.45455    0.04181   10.87  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7357 on 532 degrees of freedom
    ## Multiple R-squared:  0.2592, Adjusted R-squared:  0.2565 
    ## F-statistic: 93.09 on 2 and 532 DF,  p-value: < 2.2e-16

``` r
model_samS_osiq.et %>% modelEffectSizes()
```

    ## lm(formula = sam_spa_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Coefficients
    ##                 SSR df pEta-sqr dR-sqr
    ## (Intercept) 70.5026  1   0.1967     NA
    ## osiq_mini_o 10.9148  1   0.0365 0.0281
    ## osiq_mini_s 63.9781  1   0.1818 0.1646
    ## 
    ## Sum of squared errors (SSE): 288.0
    ## Sum of squared total  (SST): 388.7

``` r
# SAM-future
model_samF_osiq.et = lm(sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
model_samF_osiq.et %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7601 -0.4122  0.0185  0.4624  2.6749 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.00115    0.13959   7.172 2.49e-12 ***
    ## osiq_mini_o  0.71379    0.03406  20.959  < 2e-16 ***
    ## osiq_mini_s  0.10482    0.03860   2.715  0.00683 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6793 on 532 degrees of freedom
    ## Multiple R-squared:  0.5002, Adjusted R-squared:  0.4983 
    ## F-statistic: 266.2 on 2 and 532 DF,  p-value: < 2.2e-16

``` r
model_samF_osiq.et %>% modelEffectSizes()
```

    ## lm(formula = sam_fut_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Coefficients
    ##                  SSR df pEta-sqr dR-sqr
    ## (Intercept)  23.7362  1   0.0882     NA
    ## osiq_mini_o 202.6989  1   0.4523 0.4127
    ## osiq_mini_s   3.4023  1   0.0137 0.0069
    ## 
    ## Sum of squared errors (SSE): 245.5
    ## Sum of squared total  (SST): 491.1

``` r
# SAM-semantic
model_samM_osiq.et = lm(sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
model_samM_osiq.et %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.12924 -0.43293  0.04937  0.50937  1.98135 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.98185    0.14021  14.135   <2e-16 ***
    ## osiq_mini_o  0.35085    0.03421  10.256   <2e-16 ***
    ## osiq_mini_s  0.07099    0.03877   1.831   0.0677 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6823 on 532 degrees of freedom
    ## Multiple R-squared:  0.1997, Adjusted R-squared:  0.1967 
    ## F-statistic: 66.38 on 2 and 532 DF,  p-value: < 2.2e-16

``` r
model_samM_osiq.et %>% modelEffectSizes()
```

    ## lm(formula = sam_sem_raw ~ osiq_mini_o + osiq_mini_s, data = df.sam.osiq.et)
    ## 
    ## Coefficients
    ##                 SSR df pEta-sqr dR-sqr
    ## (Intercept) 93.0157  1   0.2730     NA
    ## osiq_mini_o 48.9716  1   0.1651 0.1582
    ## osiq_mini_s  1.5607  1   0.0063 0.0050
    ## 
    ## Sum of squared errors (SSE): 247.7
    ## Sum of squared total  (SST): 309.5

#### Multivariate analysis (partial least squares correlation)

``` r
# select mini OSIQ items
df.osiq.et = df.et %>%
  select(osiq_s_1, osiq_s_9, osiq_s_13, osiq_s_14, osiq_s_18, osiq_s_27r, osiq_s_29,
         osiq_o_4, osiq_o_12, osiq_o_17, osiq_o_22, osiq_o_25, osiq_o_28, osiq_o_30)

## convert to numeric
df.osiq.et %<>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)

# rename variables
names(df.osiq.et) = c(
  "s1", "s9", "s13", "s14", "s18", "s27", "s29",
  "o4", "o12", "o17", "o22", "o25", "o28", "o30"
)
```

``` r
plsc.sam.osiq.et = tepPLS(
  DATA1 = df.osiq.et,
  DATA2 = df.sam.et,
  center1 = TRUE, scale1 = FALSE,
  center2 = TRUE, scale2 = FALSE,
  make_design_nominal = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
plsc.sam.osiq.et$TExPosition.Data$t
```

    ##  [1] 93.354801499  5.575564266  0.402764511  0.226564907  0.112494363
    ##  [6]  0.094854617  0.078026533  0.047697943  0.044757950  0.020079852
    ## [11]  0.017546431  0.013163260  0.007327442  0.004356426

## Gender differences

``` r
pca.sam.female = epPCA(
  df.sam.female,
  scale = FALSE,
  center = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
pca.sam.female$ExPosition.Data$t
```

    ##  [1] 28.1050133 11.9493762  9.4471132  6.5896683  5.2609163  3.6642251
    ##  [7]  3.2871748  2.6734243  2.5650885  2.4499041  2.2380529  2.1591837
    ## [13]  2.0892158  1.9001568  1.8590618  1.7183029  1.6701243  1.5926905
    ## [19]  1.4895272  1.2976190  1.1173751  1.0981997  1.0635192  1.0150340
    ## [25]  0.9763826  0.7236502

``` r
pca.sam.male = epPCA(
  df.sam.male,
  scale = FALSE,
  center = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
pca.sam.male$ExPosition.Data$t
```

    ##  [1] 26.3532597 12.2460083 10.1058844  6.4493061  5.0806003  3.6068266
    ##  [7]  3.3807890  3.1840066  2.8646063  2.6415565  2.3110775  2.2640892
    ## [13]  2.1109701  1.8897209  1.8026925  1.6686403  1.5883757  1.5270966
    ## [19]  1.3708358  1.3187571  1.2378049  1.1114194  1.0622450  1.0120615
    ## [25]  0.9497339  0.8616356

``` r
# set up data
df.sam.osiq.gender = cbind(df$gender, df.sam.osiq)
names(df.sam.osiq.gender)[1] = "gender"
df.sam.osiq.gender %<>% filter(gender != "Prefer not to answer")

# SAM-episodic
model_samE_osiq_gender = 
  lm(sam_epi_raw ~ gender * (osiq_mini_o + osiq_mini_s), data = df.sam.osiq.gender)
summary(model_samE_osiq_gender)$adj.r.squared
```

    ## [1] 0.3350123

``` r
model_samE_osiq_gender %>% car::Anova(type = 3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sam_epi_raw
    ##                    Sum Sq   Df   F value    Pr(>F)    
    ## (Intercept)         129.2    1  242.3557 < 2.2e-16 ***
    ## gender                3.1    1    5.8629   0.01549 *  
    ## osiq_mini_o        1214.5    1 2277.6915 < 2.2e-16 ***
    ## osiq_mini_s          25.8    1   48.3841 3.803e-12 ***
    ## gender:osiq_mini_o    2.7    1    5.0918   0.02407 *  
    ## gender:osiq_mini_s    2.3    1    4.2967   0.03822 *  
    ## Residuals          3945.3 7399                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_samE_osiq_gender %>% modelEffectSizes()
```

    ## lm(formula = sam_epi_raw ~ gender * (osiq_mini_o + osiq_mini_s), 
    ##     data = df.sam.osiq.gender)
    ## 
    ## Coefficients
    ##                          SSR df pEta-sqr dR-sqr
    ## (Intercept)         129.2291  1   0.0317     NA
    ## gender                3.1262  1   0.0008 0.0005
    ## osiq_mini_o        1214.5127  1   0.2354 0.2046
    ## osiq_mini_s          25.7994  1   0.0065 0.0043
    ## gender:osiq_mini_o    2.7151  1   0.0007 0.0005
    ## gender:osiq_mini_s    2.2911  1   0.0006 0.0004
    ## 
    ## Sum of squared errors (SSE): 3945.3
    ## Sum of squared total  (SST): 5936.9

``` r
# SAM-spatial
model_samS_osiq_gender = 
  lm(sam_spa_raw ~ gender * (osiq_mini_o + osiq_mini_s), data = df.sam.osiq.gender)
summary(model_samS_osiq_gender)$adj.r.squared
```

    ## [1] 0.2085629

``` r
model_samS_osiq_gender %>% car::Anova(type = 3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sam_spa_raw
    ##                    Sum Sq   Df   F value    Pr(>F)    
    ## (Intercept)        1507.9    1 2165.0312 < 2.2e-16 ***
    ## gender               33.9    1   48.7070 3.229e-12 ***
    ## osiq_mini_o          89.7    1  128.7947 < 2.2e-16 ***
    ## osiq_mini_s         533.5    1  765.9319 < 2.2e-16 ***
    ## gender:osiq_mini_o    9.8    1   14.0148 0.0001828 ***
    ## gender:osiq_mini_s    1.2    1    1.7166 0.1901663    
    ## Residuals          5153.3 7399                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_samS_osiq_gender %>% modelEffectSizes()
```

    ## lm(formula = sam_spa_raw ~ gender * (osiq_mini_o + osiq_mini_s), 
    ##     data = df.sam.osiq.gender)
    ## 
    ## Coefficients
    ##                          SSR df pEta-sqr dR-sqr
    ## (Intercept)        1507.9071  1   0.2264     NA
    ## gender               33.9236  1   0.0065 0.0052
    ## osiq_mini_o          89.7033  1   0.0171 0.0138
    ## osiq_mini_s         533.4584  1   0.0938 0.0819
    ## gender:osiq_mini_o    9.7611  1   0.0019 0.0015
    ## gender:osiq_mini_s    1.1956  1   0.0002 0.0002
    ## 
    ## Sum of squared errors (SSE): 5153.3
    ## Sum of squared total  (SST): 6515.7

``` r
# SAM-future
model_samF_osiq_gender = 
  lm(sam_fut_raw ~ gender * (osiq_mini_o + osiq_mini_s), data = df.sam.osiq.gender)
summary(model_samF_osiq_gender)$adj.r.squared
```

    ## [1] 0.5260197

``` r
model_samF_osiq_gender %>% car::Anova(type = 3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sam_fut_raw
    ##                    Sum Sq   Df   F value    Pr(>F)    
    ## (Intercept)         119.0    1  184.8924 < 2.2e-16 ***
    ## gender                6.6    1   10.2846  0.001347 ** 
    ## osiq_mini_o        2982.1    1 4634.7113 < 2.2e-16 ***
    ## osiq_mini_s          60.7    1   94.2877 < 2.2e-16 ***
    ## gender:osiq_mini_o    0.0    1    0.0765  0.782142    
    ## gender:osiq_mini_s    1.9    1    2.9040  0.088401 .  
    ## Residuals          4760.7 7399                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_samF_osiq_gender %>% modelEffectSizes()
```

    ## lm(formula = sam_fut_raw ~ gender * (osiq_mini_o + osiq_mini_s), 
    ##     data = df.sam.osiq.gender)
    ## 
    ## Coefficients
    ##                          SSR df pEta-sqr dR-sqr
    ## (Intercept)         118.9635  1   0.0244     NA
    ## gender                6.6173  1   0.0014 0.0007
    ## osiq_mini_o        2982.0684  1   0.3851 0.2967
    ## osiq_mini_s          60.6667  1   0.0126 0.0060
    ## gender:osiq_mini_o    0.0492  1   0.0000 0.0000
    ## gender:osiq_mini_s    1.8685  1   0.0004 0.0002
    ## 
    ## Sum of squared errors (SSE): 4760.7
    ## Sum of squared total  (SST): 10050.8

``` r
# SAM-semantic
model_samM_osiq_gender = 
  lm(sam_sem_raw ~ gender * (osiq_mini_o + osiq_mini_s), data = df.sam.osiq.gender)
summary(model_samM_osiq_gender)$adj.r.squared
```

    ## [1] 0.05769657

``` r
model_samM_osiq_gender %>% car::Anova(type = 3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sam_sem_raw
    ##                    Sum Sq   Df   F value    Pr(>F)    
    ## (Intercept)        1878.4    1 2993.3977 < 2.2e-16 ***
    ## gender                2.1    1    3.3294   0.06809 .  
    ## osiq_mini_o         137.5    1  219.0410 < 2.2e-16 ***
    ## osiq_mini_s          22.1    1   35.1843 3.134e-09 ***
    ## gender:osiq_mini_o    1.5    1    2.3348   0.12655    
    ## gender:osiq_mini_s    0.1    1    0.1242   0.72455    
    ## Residuals          4643.1 7399                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_samM_osiq_gender %>% modelEffectSizes()
```

    ## lm(formula = sam_sem_raw ~ gender * (osiq_mini_o + osiq_mini_s), 
    ##     data = df.sam.osiq.gender)
    ## 
    ## Coefficients
    ##                          SSR df pEta-sqr dR-sqr
    ## (Intercept)        1878.4413  1   0.2880     NA
    ## gender                2.0893  1   0.0004 0.0004
    ## osiq_mini_o         137.4544  1   0.0288 0.0279
    ## osiq_mini_s          22.0791  1   0.0047 0.0045
    ## gender:osiq_mini_o    1.4652  1   0.0003 0.0003
    ## gender:osiq_mini_s    0.0779  1   0.0000 0.0000
    ## 
    ## Sum of squared errors (SSE): 4643.1
    ## Sum of squared total  (SST): 4930.7

## Personality effects

### Regressions of OSIQ on SAM

``` r
# select items to analyze with halo
df.halo = df %>% 
  select(starts_with("phq"),
         starts_with("bfi"))

# change items from factor to numeric
df.halo %<>%
  mutate_all(as.character) %>% 
  mutate_all(as.numeric)

# add OSIQ and SAM scores to halo dataframe
df.halo = cbind(df.halo, df.sam.osiq)

# standardize BFI domain scores
df.halo %<>% 
  mutate_at(.vars = c("bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o"),
            scale)

# calculate halo scores
df.halo$halo = 
  ((df.halo$bfi_a + 
    df.halo$bfi_c + 
    df.halo$bfi_o + 
    df.halo$bfi_e) / 4) -
  df.halo$bfi_n
```

``` r
# episodic
model_samE_osiq_halo =
  lm(sam_epi_raw ~ halo + osiq_mini_o + osiq_mini_s, data = df.halo)
summary(model_samE_osiq_halo)
```

    ## 
    ## Call:
    ## lm(formula = sam_epi_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2523 -0.4829 -0.1116  0.3706  3.5419 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.661142   0.030055  21.997  < 2e-16 ***
    ## halo        0.045259   0.006417   7.053 1.90e-12 ***
    ## osiq_mini_o 0.516484   0.009063  56.990  < 2e-16 ***
    ## osiq_mini_s 0.046179   0.009402   4.912 9.22e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7283 on 7483 degrees of freedom
    ## Multiple R-squared:  0.3375, Adjusted R-squared:  0.3372 
    ## F-statistic:  1271 on 3 and 7483 DF,  p-value: < 2.2e-16

``` r
modelEffectSizes(model_samE_osiq_halo)
```

    ## lm(formula = sam_epi_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept)  256.6977  1   0.0607     NA
    ## halo          26.3915  1   0.0066 0.0044
    ## osiq_mini_o 1722.9820  1   0.3027 0.2876
    ## osiq_mini_s   12.7984  1   0.0032 0.0021
    ## 
    ## Sum of squared errors (SSE): 3969.7
    ## Sum of squared total  (SST): 5991.8

``` r
# spatial
model_samS_osiq_halo =
  lm(sam_spa_raw ~ halo + osiq_mini_o + osiq_mini_s, data = df.halo)
summary(model_samS_osiq_halo)
```

    ## 
    ## Call:
    ## lm(formula = sam_spa_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2829 -0.5725  0.0815  0.6092  2.4244 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.131352   0.034255  62.219  < 2e-16 ***
    ## halo        0.101899   0.007313  13.933  < 2e-16 ***
    ## osiq_mini_o 0.081757   0.010329   7.915 2.82e-15 ***
    ## osiq_mini_s 0.381042   0.010716  35.560  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8301 on 7483 degrees of freedom
    ## Multiple R-squared:  0.2152, Adjusted R-squared:  0.2149 
    ## F-statistic:   684 on 3 and 7483 DF,  p-value: < 2.2e-16

``` r
modelEffectSizes(model_samS_osiq_halo)
```

    ## lm(formula = sam_spa_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept) 2667.7392  1   0.3410     NA
    ## halo         133.7788  1   0.0253 0.0204
    ## osiq_mini_o   43.1736  1   0.0083 0.0066
    ## osiq_mini_s  871.3832  1   0.1446 0.1326
    ## 
    ## Sum of squared errors (SSE): 5156.6
    ## Sum of squared total  (SST): 6570.6

``` r
# semantic
model_samM_osiq_halo =
  lm(sam_sem_raw ~ halo + osiq_mini_o + osiq_mini_s, data = df.halo)
summary(model_samM_osiq_halo)
```

    ## 
    ## Call:
    ## lm(formula = sam_sem_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.18170 -0.55338 -0.01105  0.53893  2.62756 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.220496   0.032496  68.332  < 2e-16 ***
    ## halo        0.067888   0.006938   9.785  < 2e-16 ***
    ## osiq_mini_o 0.152547   0.009798  15.569  < 2e-16 ***
    ## osiq_mini_s 0.075999   0.010165   7.476  8.5e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7875 on 7483 degrees of freedom
    ## Multiple R-squared:  0.06826,    Adjusted R-squared:  0.06789 
    ## F-statistic: 182.7 on 3 and 7483 DF,  p-value: < 2.2e-16

``` r
modelEffectSizes(model_samM_osiq_halo)
```

    ## lm(formula = sam_sem_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept) 2895.5634  1   0.3842     NA
    ## halo          59.3782  1   0.0126 0.0119
    ## osiq_mini_o  150.3058  1   0.0314 0.0302
    ## osiq_mini_s   34.6638  1   0.0074 0.0070
    ## 
    ## Sum of squared errors (SSE): 4640.4
    ## Sum of squared total  (SST): 4980.4

``` r
# future
model_samF_osiq_halo =
  lm(sam_fut_raw ~ halo + osiq_mini_o + osiq_mini_s, data = df.halo)
summary(model_samF_osiq_halo)
```

    ## 
    ## Call:
    ## lm(formula = sam_fut_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3585 -0.5467 -0.0213  0.5111  3.5523 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.495470   0.033083  14.977  < 2e-16 ***
    ## halo        0.021902   0.007063   3.101  0.00194 ** 
    ## osiq_mini_o 0.837318   0.009976  83.937  < 2e-16 ***
    ## osiq_mini_s 0.121776   0.010349  11.767  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8017 on 7483 degrees of freedom
    ## Multiple R-squared:  0.5267, Adjusted R-squared:  0.5265 
    ## F-statistic:  2775 on 3 and 7483 DF,  p-value: < 2.2e-16

``` r
modelEffectSizes(model_samF_osiq_halo)
```

    ## lm(formula = sam_fut_raw ~ halo + osiq_mini_o + osiq_mini_s, 
    ##     data = df.halo)
    ## 
    ## Coefficients
    ##                   SSR df pEta-sqr dR-sqr
    ## (Intercept)  144.1676  1   0.0291     NA
    ## halo           6.1803  1   0.0013 0.0006
    ## osiq_mini_o 4528.4253  1   0.4849 0.4457
    ## osiq_mini_s   88.9989  1   0.0182 0.0088
    ## 
    ## Sum of squared errors (SSE): 4809.7
    ## Sum of squared total  (SST): 10160.9

### BFI and SAM

``` r
# select BFI items
df.bfi = df %>% 
  select(starts_with("bfi")) %>% 
  select(-c("bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o"))

# convert to numeric
df.bfi %<>% 
  mutate_all(as.character) %>% 
  mutate_all(as.numeric)

# rename variables
for (i in 1:length(names(df.bfi))) {
  
  # remove "bfi_" from beginning of column names
  names(df.bfi)[i] %<>% substr(5, nchar(names(df.bfi)[i]))
  
  # remove "r" from end of reverse coded items 
  if (substr(names(df.bfi)[i], 
             nchar(names(df.bfi)[i]), 
             nchar(names(df.bfi)[i])) == "r") {
    names(df.bfi)[i] %<>% substr(1, nchar(names(df.bfi)[i])-1)
  }
  
  # remove underscore
  names(df.bfi)[i] = gsub(pattern = "_", replacement = "", x = names(df.bfi)[i])
  
}

# assign colours to variables
colour.bfi = as.vector(rep("", ncol(df.bfi)))

for (i in 1:ncol(df.bfi)) {
  switch(substr(names(df.bfi)[i], 1, 1),
         a = {colour.bfi[i] = colourCode.bfi[1]},
         c = {colour.bfi[i] = colourCode.bfi[2]},
         e = {colour.bfi[i] = colourCode.bfi[3]},
         n = {colour.bfi[i] = colourCode.bfi[4]},
         o = {colour.bfi[i] = colourCode.bfi[5]})
}
```

``` r
plsc.sam.bfi = tepPLS(
  DATA1 = df.sam, 
  DATA2 = df.bfi,
  center1 = TRUE, scale1 = FALSE,
  center2 = TRUE, scale2 = FALSE,
  make_design_nominal = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
plsc.sam.bfi$TExPosition.Data$t
```

    ##  [1] 77.692197364 15.973188949  2.506618445  1.620475416  0.568396032
    ##  [6]  0.492880239  0.335426665  0.218461653  0.156100027  0.106277369
    ## [11]  0.068255121  0.053626192  0.044183995  0.030416568  0.025544497
    ## [16]  0.023448192  0.020629930  0.016307496  0.012317457  0.009558946
    ## [21]  0.007940383  0.005534128  0.005248898  0.003295406  0.002531825
    ## [26]  0.001138805

## Depression effects

``` r
# select PHQ items
df.phq = df %>% 
  select(starts_with("phq")) %>% 
  select(-c("phq_function", "phq_total"))

# convert to numeric
df.phq %<>% 
  mutate_all(as.character) %>% 
  mutate_all(as.numeric)

# rename variables
for (i in 1:length(names(df.phq))) {
  
  # remove underscore
  names(df.phq)[i] = gsub(pattern = "_", replacement = "", x = names(df.phq)[i])
  
}
```

``` r
plsc.sam.phq = tepPLS(
  DATA1 = df.sam, 
  DATA2 = df.phq,
  center1 = TRUE, scale1 = FALSE,
  center2 = TRUE, scale2 = FALSE,
  make_design_nominal = TRUE,
  graphs = FALSE)

# percent variance explained by each dimension
plsc.sam.phq$TExPosition.Data$t
```

    ## [1] 91.84866059  4.52250137  1.72270775  0.87150829  0.48471848  0.26009076
    ## [7]  0.16818128  0.07326065  0.04837083

<!-- ======================================================================= -->

# Session info

``` r
sessionInfo()
```

    ## R version 4.3.3 (2024-02-29 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_Canada.utf8  LC_CTYPE=English_Canada.utf8   
    ## [3] LC_MONETARY=English_Canada.utf8 LC_NUMERIC=C                   
    ## [5] LC_TIME=English_Canada.utf8    
    ## 
    ## time zone: America/Toronto
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] lubridate_1.9.3      forcats_1.0.0        stringr_1.5.1       
    ##  [4] dplyr_1.1.4          purrr_1.0.2          readr_2.1.4         
    ##  [7] tidyr_1.3.1          tibble_3.2.1         ggplot2_3.5.0       
    ## [10] tidyverse_2.0.0      magrittr_2.0.3       wrapr_2.1.0         
    ## [13] TInPosition_0.10.4   InPosition_0.12.7.1  TExPosition_2.6.10.1
    ## [16] reshape_0.8.9        readxl_1.4.3         PTCA4CATA_0.1.0     
    ## [19] psych_2.4.3          lmSupport_2.9.13     gridExtra_2.3       
    ## [22] gplots_3.1.3.1       ExPosition_2.8.23    prettyGraphs_2.1.6  
    ## [25] corrplot_0.92       
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] mnormt_2.1.1            bitops_1.0-7            sandwich_3.1-0         
    ##   [4] rlang_1.1.2             multcomp_1.4-25         matrixStats_1.2.0      
    ##   [7] compiler_4.3.3          systemfonts_1.0.6       vctrs_0.6.5            
    ##  [10] httpcode_0.3.0          pkgconfig_2.0.3         crayon_1.5.2           
    ##  [13] fastmap_1.1.1           backports_1.4.1         ellipsis_0.3.2         
    ##  [16] caTools_1.18.2          utf8_1.2.4              promises_1.2.1         
    ##  [19] rmarkdown_2.25          tzdb_0.4.0              nloptr_2.0.3           
    ##  [22] ragg_1.3.0              xfun_0.41               modeltools_0.2-23      
    ##  [25] jsonlite_1.8.8          later_1.3.2             uuid_1.2-0             
    ##  [28] VGAM_1.1-10             broom_1.0.5             parallel_4.3.3         
    ##  [31] R6_2.5.1                stringi_1.8.3           coin_1.4-3             
    ##  [34] car_3.1-2               boot_1.3-29             cellranger_1.1.0       
    ##  [37] Rcpp_1.0.11             knitr_1.45              zoo_1.8-12             
    ##  [40] timechange_0.2.0        httpuv_1.6.13           Matrix_1.6-5           
    ##  [43] splines_4.3.3           tidyselect_1.2.1        rstudioapi_0.15.0      
    ##  [46] abind_1.4-5             yaml_2.3.8              codetools_0.2-19       
    ##  [49] curl_5.2.0              plyr_1.8.9              lattice_0.22-5         
    ##  [52] withr_3.0.0             shiny_1.8.0             askpass_1.2.0          
    ##  [55] evaluate_0.23           pwr_1.3-0               survival_3.5-8         
    ##  [58] zip_2.3.1               xml2_1.3.6              pillar_1.9.0           
    ##  [61] carData_3.0-5           KernSmooth_2.23-22      stats4_4.3.3           
    ##  [64] generics_0.1.3          hms_1.1.3               munsell_0.5.0          
    ##  [67] scales_1.3.0            minqa_1.2.6             gtools_3.9.5           
    ##  [70] xtable_1.8-4            glue_1.6.2              unmarked_1.4.1         
    ##  [73] gvlma_1.0.0.3           gdtools_0.3.7           AICcmodavg_2.3-3       
    ##  [76] tools_4.3.3             gfonts_0.2.0            lme4_1.1-35.1          
    ##  [79] mvtnorm_1.2-4           grid_4.3.3              libcoin_1.0-10         
    ##  [82] colorspace_2.1-0        nlme_3.1-164            cli_3.6.2              
    ##  [85] rvg_0.3.3               textshaping_0.3.7       officer_0.6.5          
    ##  [88] fontBitstreamVera_0.1.1 fansi_1.0.6             gtable_0.3.4           
    ##  [91] digest_0.6.33           fontquiver_0.2.1        ggrepel_0.9.5          
    ##  [94] pbkrtest_0.5.2          crul_1.4.0              TH.data_1.1-2          
    ##  [97] htmltools_0.5.7         lifecycle_1.0.4         mime_0.12              
    ## [100] fontLiberation_0.1.0    openssl_2.1.1           MASS_7.3-60.0.1
