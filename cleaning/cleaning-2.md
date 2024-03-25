Online Study Data Cleaning
================
Carina Fan
Last updated: March 25, 2024

- [Set up](#set-up)
- [Exclude based on health history and
  PHQ](#exclude-based-on-health-history-and-phq)
- [Demographics of participants in clean
  data](#demographics-of-participants-in-clean-data)
- [Demographics of previously excluded
  participants](#demographics-of-previously-excluded-participants)
- [Export data](#export-data)
- [Session info](#session-info)

This file runs further exclusions (e.g., for health conditions) after an
initial clean for data validity.

# Set up

``` r
# load packages
library(psych)
library(magrittr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::%+%()     masks psych::%+%()
    ## ✖ ggplot2::alpha()   masks psych::alpha()
    ## ✖ tidyr::extract()   masks magrittr::extract()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ purrr::set_names() masks magrittr::set_names()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
today = "2023-07-26"
```

``` r
df = paste0(today, "-demo-clean.csv") %>% 
  read.csv(stringsAsFactors = FALSE)
```

<!-- ======================================================================= -->

# Exclude based on health history and PHQ

``` r
# names of health variables to exclude based on
healthNames = df %>% 
  select(starts_with("health_")) %>% 
  select(-c("health_other", "health_other_text", "health_alcdrug")) %>% 
  names()

# number of participants endorsing each condition
df.health = df %>% 
  select(all_of(healthNames)) %>% 
  mutate_all(as.character)
sapply(df.health, function(x) sum(str_count(x, "Yes"), na.rm = TRUE))
```

    ##       health_tbi    health_stroke  health_dementia  health_epilepsy 
    ##              738               79               56              291 
    ## health_brainsurg    health_cancer health_psychotic  health_learning 
    ##              113              276              323             1248 
    ##       health_cog 
    ##              880

``` r
# column indices of variables to exclude 
healthCols = which(names(df) %in% healthNames)

# initialize vector of subjects with neuro conditions 
healthSubjects = vector(mode = "character")

## if subject responded Yes to any neuro condition, 
## add their row number to neuroSubject
for (row in 1:nrow(df)) {
  for (col in 1:length(healthCols)) {
    if (df[row, healthCols[col]] == "Yes") {
      healthSubjects = append(healthSubjects, row)
    }
  }
}

# number of subjects to exclude based on health history
length(unique(healthSubjects)) 
```

    ## [1] 3095

``` r
# number of subjects to exclude based on PHQ (scores >= 20)
# df$phq_total %<>% as.character() %>% as.numeric()
# df %>%
#   filter(phq_total >= 20) %>%
#   nrow()

# exclude subjects 
df.clean = df %>% 
  filter(!row_number() %in% healthSubjects)
         # phq_total < 20) 
df.excluded = df |> 
  filter(row_number() %in% healthSubjects)

# number of participants after all exclusions
nrow(df.clean)
```

    ## [1] 14677

<!-- ======================================================================= -->

# Demographics of participants in clean data

``` r
# age & edu
df.clean %>% 
  select(age, edu) %>% 
  describe()
```

    ##     vars     n  mean    sd median trimmed   mad min max range skew kurtosis
    ## age    1 14677 38.25 13.63     36   37.32 14.83  18  88    70 0.54    -0.41
    ## edu    2 14677 16.70  3.07     16   16.59  2.97   9  26    17 0.33     0.09
    ##       se
    ## age 0.11
    ## edu 0.03

``` r
# gender
df.clean %>% 
  group_by(gender) %>% 
  summarise(n = n())
```

    ## # A tibble: 3 × 2
    ##   gender                   n
    ##   <chr>                <int>
    ## 1 Female                9471
    ## 2 Male                  4974
    ## 3 Prefer not to answer   232

<!-- ======================================================================= -->

# Demographics of previously excluded participants

The following are numbers after excluding for health/neurological
history, but retaining participants with high PHQ scores or substance
abuse.

``` r
df.clean |> 
  filter(phq_total >= 20) |>
  group_by(sdam) |> 
  summarise(n = n())
```

    ## # A tibble: 2 × 2
    ##   sdam      n
    ##   <chr> <int>
    ## 1 No      139
    ## 2 Yes     580

Of participants with PHQ scores above threshold, 80.7% had SDAM. Note
that these participants are not excluded from the extraclean data.

``` r
df.clean |> 
  filter(health_alcdrug == "Yes") |> 
  group_by(sdam) |> 
  summarise(n = n())
```

    ## # A tibble: 2 × 2
    ##   sdam      n
    ##   <chr> <int>
    ## 1 No      306
    ## 2 Yes     609

Of participants with alcohol/drug abuse, 66.6% had SDAM.

``` r
df.clean |> 
  filter(phq_total >= 20 | health_alcdrug == "Yes") |> 
  group_by(sdam) |> 
  summarise(n = n())
```

    ## # A tibble: 2 × 2
    ##   sdam      n
    ##   <chr> <int>
    ## 1 No      420
    ## 2 Yes    1129

Of those with PHQ or substance abuse, 72.9% had SDAM.

<!-- ======================================================================= -->

# Export data

``` r
# df.clean %>% 
#   write.csv(paste0(today, "-demo-extraclean.csv"), row.names = FALSE)
```

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
    ##  [1] lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
    ##  [5] purrr_1.0.2     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1   
    ##  [9] ggplot2_3.4.4   tidyverse_2.0.0 magrittr_2.0.3  psych_2.4.3    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.4      compiler_4.3.3    tidyselect_1.2.0  parallel_4.3.3   
    ##  [5] scales_1.3.0      yaml_2.3.8        fastmap_1.1.1     lattice_0.22-5   
    ##  [9] R6_2.5.1          generics_0.1.3    knitr_1.45        munsell_0.5.0    
    ## [13] pillar_1.9.0      tzdb_0.4.0        rlang_1.1.2       utf8_1.2.4       
    ## [17] stringi_1.8.3     xfun_0.41         timechange_0.2.0  cli_3.6.2        
    ## [21] withr_2.5.2       digest_0.6.33     grid_4.3.3        rstudioapi_0.15.0
    ## [25] hms_1.1.3         lifecycle_1.0.4   nlme_3.1-164      vctrs_0.6.5      
    ## [29] mnormt_2.1.1      evaluate_0.23     glue_1.6.2        fansi_1.0.6      
    ## [33] colorspace_2.1-0  rmarkdown_2.25    tools_4.3.3       pkgconfig_2.0.3  
    ## [37] htmltools_0.5.7
