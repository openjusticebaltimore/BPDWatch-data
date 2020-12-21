# bpd database of lawsuits against cops that no one knew about
library(tidyverse)
library(rvest)

base_url <- "https://ca.baltimorecity.gov/law/index.php" #?g=1
base_src <- httr::GET(base_url, query = list(g = 1)) %>%
	httr::content()

# easiest is to just read how many pages there are & map over
n_pages <- base_src %>%
	html_nodes(xpath = "//div[contains(text(),'Page')]") %>%
	html_text() %>%
	str_extract("(?<= of )(\\d+$)") %>%
	as.numeric()

# 0-indexed
# keep table rows that aren't class=hdr and don't have style set
tbl_links <- map(1:n_pages, function(i) {
	cp <- i - 1
	httr::GET(base_url, query = list(g = 1, cp = cp)) %>%
		httr::content() %>%
		html_node("table#searchtbl") %>%
		html_nodes(xpath = "//tr[not(@class='hdr') and not(@style)]") %>%
		map(html_nodes, "td") %>%
		map(html_nodes, "a") %>%
		map_chr(html_attr, "href")
}) %>%
	flatten()

# would use html_table, but that strips away <br>s that help delineate parties
# https://stackoverflow.com/questions/30129407/r-rvest-extracting-innerhtml
case_deets <- tbl_links %>%
	map(read_html) %>%
	map(html_node, "table#casetbl") %>%
	map(html_nodes, "tr") %>%
	map_depth(2, html_nodes, "td") %>%
	map_depth(3, html_nodes, xpath = ".//text()") %>%
	map_depth(3, as.character) %>%
	map_depth(2, function(x) set_names(list(x[2]), x[1])) %>%
	map_depth(2, as_tibble) %>%
	map_dfr(bind_cols) %>%
	janitor::clean_names()

# split disposition into type & details, since right now it's all one vector
# collapse damages
# sep parties by plaintiff vs defendant; keep cops on multiple rows
str_collapse <- function(x) {
	map_chr(x, paste, collapse = "; ") %>%
		str_squish() %>%
		na_if("")
}

# F U C K this is messy
cases_df <- case_deets %>%
	mutate(row = row_number()) %>%
	pivot_longer(-row, values_drop_na = TRUE) %>%
	unnest(value) %>%
	filter(!str_detect(value, "^\\:$")) %>%
	mutate(value = str_remove_all(value, "\\n") %>% str_squish()) %>%
	pivot_wider(id_cols = row, values_fn = list) %>%
	group_by(row) %>%
	mutate(disposition_type = map_chr(disposition, 1),
				 disposition_det = map(disposition, ~.[-1]) %>% map_chr(paste, collapse = "; ")) %>%
	mutate(across(c(case_no, venue:incident_date), str_collapse),
				 settlement_num = parse_number(verdict_settlement_amount),
				 incident_date = lubridate::mdy(incident_date)) %>%
	select(-disposition, -verdict_settlement_amount) %>% 
	unnest(parties) %>%
	extract(parties, into = c("party_type", "party_name"), regex = "(^\\w+)\\: (.+$)") %>%
	mutate(party_type = tolower(party_type)) %>%
	pivot_wider(names_from = party_type, values_from = party_name, values_fn = list) %>%
	unnest(c(defendant, plaintiff)) %>%
	ungroup() %>%
	mutate(cop_name = defendant %>%
				 	str_remove(", et al.*$") %>%
				 	str_remove_all("(Officer?|Sgt|Det(ective)?.?|Major)") %>%
				 	str_remove("(^\\.|\\.$)") %>%
				 	str_squish() %>%
				 	if_else(str_detect(., "(^Balt|City of Baltimore|City Council|Management|^BPD$)"), NA_character_, .))

# siiiiiick, write this out
write_csv(cases_df, "lawsuits/bpd_lawsuits_scraped.csv")
