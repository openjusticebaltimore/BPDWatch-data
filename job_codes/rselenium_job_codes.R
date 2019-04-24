library(tidyverse)
library(rvest)
library(httr)
library(RSelenium)

# system("docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug")

hr_url <- "https://www.governmentjobs.com/careers/baltimorecity/classspecs"
# url <- "https://www.governmentjobs.com/careers/classspecifications/index?agency=baltimorecity&sort=ClassTitle&isDescendingSort=false&_=1554870658883"

ff_driver <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  javascript = T,
  browserName = "firefox"
)
ff_driver$open(silent = T)
Sys.sleep(10)
# getting the search box errors the first time...not sure why. Try it a couple times.
ff_driver$navigate(hr_url)
Sys.sleep(15)

# found <- FALSE
# attempt <- 0
# while (!found && attempt < 3) {
#   attempt <- attempt + 1
#   search <- ff_driver$findElement(using = "css selector", "[name='keyword']")
#   if (length(search) > 0) {
#     search$sendKeysToElement(list("police", key = "enter"))
#     found <- TRUE
#   }
# }

# close cookies popup that gets in the way of page nav
ff_driver$findElement(using = "css selector", "#cookie-consent button")$clickElement()

page_count <- ff_driver$findElement(using = "css selector", "#number-found-items")$getElementText() %>%
  str_remove_all("\\D") %>%
  as.numeric() %>%
  magrittr::divide_by(10) %>%
  ceiling()

cop_lis <- map(1:page_count, possibly(function(i) {
  # sleep 15 seconds since it loads really slowly
  Sys.sleep(15)
  container <- ff_driver$getPageSource()[[1]] %>%
    read_html() %>%
    html_node("ul.search-results-listing-container")
  lis <- container %>%
    html_nodes("li.list-item")
  
  next_button <- ff_driver$findElement(using = "css selector", "li.PagedList-skipToNext>a")
  next_button$clickElement()
  lis
}, NULL))


# make a dataframe of job info
cop_df <- cop_lis %>%
  map_dfr(function(li_node) {
    # href
    href <- li_node %>%
      html_nodes("h3 a") %>%
      html_attr("href")
    # title
    title <- li_node %>%
      html_nodes("h3 a") %>%
      html_text()
    # code
    code <- li_node %>%
      html_nodes("li") %>%
      html_text()
    # blurb
    blurb <- li_node %>%
      html_nodes("div.list-entry") %>%
      html_text(trim = T)
    tibble(code, title, blurb, href)
  }, .id = "page")

write_csv(cop_df, "job_codes/hr_scraped_job_codes.csv")

ff_driver$close()

# system("docker stop $(docker ps -q | head -n 1)")
