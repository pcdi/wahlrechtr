library(tidyverse)
wahlrecht_base <- "https://www.wahlrecht.de/umfragen/"
institutes <- c("allensbach", "emnid", "forsa", "politbarometer", "gms", "dimap", "insa", "yougov")
table_names <- c("date", "cdu_csu", "spd", "gruene", "fdp", "linke", "afd", "other", "nonvoter", "respondents", "timeframe")

html <- rvest::read_html("https://www.wahlrecht.de/umfragen/forsa.htm")
table <- html %>% rvest::html_element(".wilko")
df_raw <- table %>% rvest::html_table(header = F, na.strings = "")
df <- df_raw %>%
  dplyr::select(-c(X2, X10)) %>%
  dplyr::rename_with(.fn = ~table_names) %>%
  dplyr::slice(5:n()) %>%
  dplyr::mutate(date = readr::parse_date(date, format = "%d.%m.%Y")) %>%
  dplyr::filter(!timeframe == "Bundestagswahl") %>%
  dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ tidyr::replace_na(data = .x, replace = "0"))) %>%
  dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ dplyr::if_else(.x == "–", "0", .x))) %>%
  dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ readr::parse_number(.x, locale = locale(grouping_mark = ".")))) %>%
  tidyr::separate(timeframe, into = c("timeframe_begin", "timeframe_end"), sep = "–", fill = "right") %>%
  dplyr::mutate(timeframe_end = dplyr::coalesce(timeframe_end, timeframe_begin))


# needs year
# %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("timeframe_"), .fns = ~ readr::parse_date(.x, format = "%d.%m.")))
