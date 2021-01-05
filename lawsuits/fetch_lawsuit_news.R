#    (  (    )          ) .  (               (   (( (   (  (   ((  )   
#   ()) )\  (()        ()) . )\  )\ )\       )\ (\())\  )\ )\ (\()(()  
#  (_)()(_)()(_)      (())) ((_)((_)(_)     ((_))(_)(_)((_)(_))(_))(_) 
#  | _ ) _ \   \     (/ __|/ _ \|_   _|    / __| __| _ \ \ / / __|   \ 
#  | _ \  _/ |) |    | (_ | (_) | | |      \__ \ _||   /\   /| _|| |) |
#  |___/_| |___/      \___|\___/  |_|      |___/___|_|_\ \_/ |___|___/ 
#  lawsuits

library(tidyverse)
library(httr)

# was going to use dates as search param, but if they were scumbags in e.g. 2015 
# they were probably also scumbags in 2012 ¯\_(ツ)_/¯
# fill in missing date with earliest
base_url <- "https://serpapi.com/search"

# there's a few cops in the lawsuit database that only have a last name. 
# checking against the cleaned cop names--any time a cop with a lawsuit matches
# *only* one last name from the cleaned cop names, I'll fill in the first name
# from there. only adds a few, but lawsuits are a relatively small subset of cops
# anyway, so it's still helpful
name_lookup <- read_csv("https://raw.githubusercontent.com/openjusticebaltimore/gttf/master/data/cleaned_cop_names.csv") %>%
	mutate(name2 = name_clean %>%
				 	str_to_title() %>%
				 	str_replace("^([\\w \\-]+)(?:,)?( )?([\\w \\-]+$)?", "\\3\\2\\1")) %>%
	select(clean_name = name2) %>%
	distinct() %>%
	extract(clean_name, into = c("first", "last"), regex = "(^.+) (.+$)", remove = FALSE)

cases_read <- read_csv("lawsuits/bpd_lawsuits_scraped.csv") %>%
	select(everything(), -defendant, defendant)

cases_fill_in <- cases_read %>%
	filter(!str_detect(cop_name, "\\s")) %>%
	left_join(name_lookup, by = c("cop_name" = "last")) %>%
	group_by(cop_name) %>%
	filter(n() == 1) %>%
	ungroup() %>%
	mutate(cop_name = coalesce(clean_name, cop_name)) %>%
	select(-clean_name, -first)

cases_df <- bind_rows(
		cases_read %>%
			anti_join(cases_fill_in, by = c("row", "defendant")),
		cases_fill_in
	) %>%
	arrange(row, cop_name)

rm(cases_read, cases_fill_in)

search_cop <- function(cop) {
	Sys.sleep(1)
		query <-  list(
			q = sprintf('"baltimore police" "%s"', cop),
			enfine = "google",
			api_key = Sys.getenv("SERP_API"),
			google_domain = "google.com",
			gl = "us",
			hl = "en",
			num = 15
		)
		GET(base_url, query = query)
}

reqs <- cases_df %>%
	filter(!is.na(cop_name)) %>%
	distinct(cop_name) %>%
	pull() %>%
	set_names() %>%
	# map(possibly(search_cop, NULL)) %>%
	# compact() %>%
	map(search_cop) %>%
	map(content) %>%
	map(pluck, "organic_results") %>%
	map_depth(2, as_tibble) %>%
	map_dfr(~bind_rows(.), .id = "cop_name") %>%
	select(cop_name, title, link, link_date = date, snippet)


joined <- cases_df %>%
	left_join(reqs, by = "cop_name")

joined %>%
	nest(cop_data = c(-row:-plaintiff)) %>%
	mutate(cop_data = cop_data %>%
				 	map(nest, news = c(-defendant, -cop_name))) %>%
	jsonlite::write_json("lawsuits/bpd_lawsuits_newslinks_nested.json")

joined %>%
	select(row, case_no, cop_name:snippet) %>%
	write_csv("lawsuits/bpd_lawsuits_newslinks.csv", na = "")