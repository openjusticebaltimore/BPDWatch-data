Clean up BPD job codes + titles
================

``` r
library(tidyverse)
library(rvest)
```

I’m trying to reconcile job titles [scraped from the HR
site](./rselenium_job_codes.R) with the titles in the public employees
rosters. Main issue was just abbreviations that didn’t match,
e.g. “supv” or “supervi” instead of “supervisor.” HR site had full
words, not abbreviations, so I cleaned roster titles to match HR.

``` r
replacements <- c(
  `\\bsupv\\b` = "supervisor",
  `\\basst\\b` = "assistant",
  `\\bprgm\\b` = "program",
  `\\bprog\\b` = "program",
  `\\bmech\\b` = "mechanic",
  `a & p` = "air & powerplant",
  `a&$` = "air & powerplant",
  `-air&power` = " air & powerplant",
  `\\bserv\\b` = "services",
  `\\bsrvc\\b` = "services",
  `\\bspec\\.?\\b` = "specialist",
  `\\bphoto\\b` = "photographer",
  `\\btech\\b` = "technician",
  `\\bsupervi\\b` = "supervisor",
  `\\boff\\b` = "officer",
  `\\bmaint\\b` = "maintenance",
  `\\basso\\b` = "associate",
  `\\bsr\\b` = "senior",
  `\\bassoc\\b` = "associate",
  `\\bair &` = "airframe &"
  )
```

``` r
hr_codes <- read_csv("hr_scraped_job_codes.csv") %>%
  mutate(title_clean = title %>%
           str_to_lower() %>%
           str_replace_all("-", " ") %>%
           str_replace("filght", "flight") %>%
           str_replace_all("\\s{2,}", " ")) %>%
  rename(hr_code = code) %>%
  select(hr_code, title_clean)

nrow(hr_codes)
```

    ## [1] 890

Cleaning up titles from rosters and taking distinct combinations of
messy title, clean title, and code–gets down to 165 unique combos.

``` r
rosters <- file.path("..", "rosters") %>%
  list.files(path = ., pattern = "*.xlsx", full.names = T) %>%
  set_names() %>%
  set_names(str_extract, "[\\w\\d]+(?=\\.xlsx$)") %>%
  map(readxl::read_excel) %>%
  map(janitor::clean_names) %>%
  map(select, emplid, job_code, job_title, starts_with("seq")) %>%
  map(rename_all, str_remove, "(?<=seq).+$")


rost_codes <- rosters %>%
  bind_rows(.id = "file") %>%
  mutate(title_clean = job_title %>%
           str_to_lower() %>%
           str_replace_all(replacements) %>%
           str_remove_all("\\.") %>%
           str_replace_all("-", " ")) %>%
  arrange(title_clean) %>%
  distinct(job_code, title_clean, job_title)

head(rost_codes)
```

    ## # A tibble: 6 x 3
    ##   job_code title_clean            job_title             
    ##   <chr>    <chr>                  <chr>                 
    ## 1 34142    accountant ii          ACCOUNTANT II         
    ## 2 34142    accountant ii          Accountant II         
    ## 3 34145    accountant supervisor  ACCOUNTANT SUPV       
    ## 4 34145    accountant supervisor  Accountant Supervisor 
    ## 5 34131    accounting assistant i ACCOUNTING ASST I     
    ## 6 34131    accounting assistant i Accounting Assistant I

``` r
nrow(rost_codes)
```

    ## [1] 165

Now there’s only 3 job codes where clean versions of titles from roster
& HR don’t match, and they seem to be from compound jobs.

``` r
rost_codes %>%
  left_join(hr_codes, by = c("job_code" = "hr_code"), suffix = c("_from_roster", "_from_hr")) %>%
  select(job_code, title_clean_from_roster, title_clean_from_hr) %>%
  filter(title_clean_from_roster != title_clean_from_hr) 
```

    ## # A tibble: 3 x 3
    ##   job_code title_clean_from_roster              title_clean_from_hr       
    ##   <chr>    <chr>                                <chr>                     
    ## 1 33150    agency it supervisor/project manager agency it supervisor      
    ## 2 83343    media producer director ii           media producer/director ii
    ## 3 83343    media producer director ii           media producer/director ii

Figure out which job titles are sworn officers, based on
[Wikipedia](https://en.wikipedia.org/wiki/Baltimore_Police_Department#Rank_structure_and_insignia).
The only one that seems off is that Wikipedia doesn’t have Chief, but
roster does. Looking at this [outdated (May 2018) org
chart](https://www.baltimorepolice.org/sites/default/files/General%20Website%20PDFs/BPDOrgChart.pdf),
people have “Chief” as a title with their name, but not as an actual job
title…I’m collapsing that into the regex as well.

``` r
# the greatest xpath of my life!
sworn <- read_html("https://en.wikipedia.org/wiki/Baltimore_Police_Department") %>%
  html_node(xpath = "//span[@id='Rank_structure_and_insignia']/parent::h2/following-sibling::table[@class='wikitable']") %>%
  html_table() %>% 
  janitor::clean_names() %>%
  separate_rows(title, sep = "/") %>%
  mutate(title = title %>% str_to_lower() %>% str_trim())

sworn_regex <- c(sworn$title, "chief") %>%
  sort() %>%
  paste(collapse = "|") %>%
  sprintf("(%s)", .)
```

Adding in `is_sworn_officer` code (0 or 1 so it translates easily into
Python). 15 jobs are sworn officers.

``` r
job_code_lookup <- rost_codes %>%
  mutate(is_sworn_officer = str_detect(title_clean, sworn_regex) %>% as.numeric()) %>%
  distinct(job_code, title_clean, is_sworn_officer)

job_code_lookup %>%
  filter(is_sworn_officer == 1)
```

    ## # A tibble: 15 x 3
    ##    job_code title_clean                is_sworn_officer
    ##    <chr>    <chr>                                 <dbl>
    ##  1 34426    chief of fiscal services i                1
    ##  2 10281    deputy police commissioner                1
    ##  3 10277    police captain                            1
    ##  4 10276    police chief                              1
    ##  5 10280    police colonel                            1
    ##  6 10282    police commissioner                       1
    ##  7 41113    police lieutenant                         1
    ##  8 10279    police lieutenant colonel                 1
    ##  9 41133    police lieutenant eid                     1
    ## 10 10278    police major                              1
    ## 11 41111    police officer                            1
    ## 12 41121    police officer eid                        1
    ## 13 41110    police officer trainee                    1
    ## 14 41112    police sergeant                           1
    ## 15 41132    police sergeant eid                       1

CSV file is in `job_codes.csv`.

``` r
write_csv(job_code_lookup, "job_codes.csv")
```
