library(tidyverse)
wahlrecht_base <- "https://www.wahlrecht.de/umfragen/"
non_numeric_cols <- c("date", "survey_period", "institute", "type")

get_institute_names <- function() {
  institute_links <- rvest::read_html(stringr::str_c(wahlrecht_base, "index.htm")) %>%
    rvest::html_elements(".wilko thead .in a") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("[^.]+")
}

get_all_institutes <- function() {
  purrr::map(.x = get_institute_names(), .f = get_institute) %>%
    dplyr::bind_rows()
}

get_institute <- function(institute_name) {
  html <- rvest::read_html(stringr::str_c(wahlrecht_base, institute_name, ".htm"))
  table <- html %>% rvest::html_element(".wilko")
  df_raw <- table %>% rvest::html_table(header = F, na.strings = "")
  df_raw[1, 1] <- "date"
  df_raw <- df_raw %>% dplyr::select(-where(~ is.na(.x[1])))
  df <- df_raw %>%
    dplyr::rename_with(.fn = ~ as_vector(df_raw[1, ])) %>%
    dplyr::mutate(institute = institute_name)
}

clean_data <- function(df) {
  df %>%
    dplyr::rename(
      cdu_csu = `CDU/CSU`,
      spd = SPD,
      gruene = GRÜNE,
      fdp = FDP,
      linke = LINKE,
      afd = AfD,
      others = Sonstige,
      respondents = Befragte,
      survey_period = Zeitraum,
      nonvoters = `Nichtwähler/Unentschl.`,
      piraten = PIRATEN,
      freie_waehler = FW
    ) %>%
    dplyr::filter(stringr::str_detect(date, "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}")) %>%
    dplyr::filter(!survey_period == "Bundestagswahl") %>%
    dplyr::mutate(date = readr::parse_date(date, format = "%d.%m.%Y")) %>%
    dplyr::filter(str_detect(cdu_csu, "[.+\\%]")) %>%
    dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(non_numeric_cols), .fns = ~ dplyr::na_if(.x, "–"))) %>%
    dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(non_numeric_cols), .fns = ~ dplyr::na_if(.x, "?"))) %>%
    tidyr::separate(col = respondents, into = c("type", "respondents"), sep = " • ", fill = "left") %>%
    dplyr::mutate(type = forcats::as_factor(type)) %>%
    dplyr::mutate(institute = forcats::as_factor(institute)) %>%
    dplyr::mutate(
      freie_waehler = dplyr::if_else(
        condition = !stringr::str_detect(string = others, pattern = "^[:digit:]"),
        true = stringr::str_match(string = others, pattern = "FW ([:digit:] %)")[, 2],
        false = freie_waehler
      )
    ) %>%
    dplyr::mutate(
      piraten = dplyr::if_else(
        condition = !stringr::str_detect(string = others, pattern = "^[:digit:]"),
        true = stringr::str_match(string = others, pattern = "PIR ([:digit:] %)")[, 2],
        false = piraten
      )
    ) %>%
    dplyr::mutate(
      others = dplyr::if_else(
        condition = !stringr::str_detect(string = others, pattern = "^[:digit:]"),
        true = stringr::str_match(string = others, pattern = "Sonst\\. ([:digit:] %)")[, 2],
        false = others
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = -dplyr::any_of(non_numeric_cols),
        .fns = ~ readr::parse_number(.x, locale = locale(grouping_mark = ".", decimal_mark = ","))
      )
    )
}

df <- get_all_institutes()
df_clean <- df %>%
  clean_data() %>%
  pivot_longer(cols = all_of(c("cdu_csu", "spd", "gruene", "fdp", "linke", "afd", "others", "nonvoters", "piraten", "freie_waehler")), names_to = "party")

df_clean %>% ggplot2::ggplot(mapping = aes(x = date, y = value, color = party)) +
  ggplot2::scale_x_date(breaks = scales::breaks_pretty()) +
  ggplot2::geom_line() +
  scale_color_brewer(type = "qual")




#   dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ tidyr::replace_na(data = .x, replace = "0"))) %>%
#   dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ dplyr::if_else(.x == "–", "0", .x))) %>%
#   dplyr::mutate(dplyr::across(.cols = cdu_csu:respondents, .fns = ~ readr::parse_number(.x, locale = locale(grouping_mark = ".")))) %>%
#   tidyr::separate(timeframe, into = c("timeframe_begin", "timeframe_end"), sep = "–", fill = "right") %>%
#   dplyr::mutate(timeframe_end = dplyr::coalesce(timeframe_end, timeframe_begin))


# needs year
# %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("timeframe_"), .fns = ~ readr::parse_date(.x, format = "%d.%m.")))
