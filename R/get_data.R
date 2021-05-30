library(tidyverse)
wahlrecht_base <- "https://www.wahlrecht.de/umfragen/"
non_numeric_cols <- c("date", "survey_period", "institute", "type")


party_colors <- c(
  "#000000", # black
  "#E3000F", # red (SPD Corporate Design Manual 4/2015)
  "#46962b", # green (Das Grüne Corporate Design 1/2017)
  "#ffed00", # yellow (FDP Gestaltungsrichtlinien 10/2016)
  "#FF0000", # red (Zum Umgang mit der Marke DIE LINKE 5/2007)
  "#009ee0", # light blue (AfD Corporate Design 3/2017)
  "#808080", # grey
  "#404040", # dark grey
  "#FF8800", # orange (Styleguide Piratenpartei Deutschland 2015)
  "#353A90" # blue (Corporate Identity Freie Wähler Hessen 2016)
)
names(party_colors) <- c(
  "cdu_csu",
  "spd",
  "gruene",
  "fdp",
  "linke",
  "afd",
  "others",
  "nonvoters",
  "piraten",
  "freie_waehler"
)
scale_color_party <- ggplot2::scale_color_manual(
  name = "party",
  values = party_colors
)


get_institute_names <- function() {
  rvest::read_html(stringr::str_c(wahlrecht_base, "index.htm")) %>%
    rvest::html_elements(".wilko thead .in a") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("[^.]+")
}

get_all_institutes <- function() {
  get_institute_names() %>%
    purrr::map_dfr(.f = get_institute)
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
  dplyr::select(
    -dplyr::all_of(
      c(
        "piraten",
        "freie_waehler",
        "nonvoters",
        "type"
      )
    )
  ) %>%
  tidyr::separate(
    survey_period,
    into = c("survey_period_begin", "survey_period_end"),
    sep = "–",
    fill = "right"
  ) %>%
  dplyr::mutate(survey_period_end = dplyr::coalesce(survey_period_end, survey_period_begin)) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("survey_period_"),
      .fns = ~ readr::parse_date(str_c(.x, lubridate::year(date)), format = "%d.%m.%Y")
    )
  ) %>%
  tidyr::pivot_longer(
    cols = any_of(
      c(
        "cdu_csu",
        "spd",
        "gruene",
        "fdp",
        "linke",
        "afd",
        "others",
        "nonvoters",
        "piraten",
        "freie_waehler"
      )
    ), names_to = "party"
  ) %>%
  dplyr::mutate(party = forcats::as_factor(party)) %>%
  tidyr::drop_na(value)


df_clean %>%
  ggplot2::ggplot(
    mapping = aes(
      x = date,
      y = value,
      color = party
    )
  ) +
  ggplot2::scale_x_date(
    breaks = scales::breaks_pretty(),
    limits = c(readr::parse_date("2017-09-25"), NA)
  ) +
  ggplot2::geom_smooth(
   # span = 0.025
  ) +
  ggplot2::geom_point(
    aes(shape = institute)
  ) +
  # ggplot2::facet_wrap(facets = vars(institute)) +
  scale_color_party
