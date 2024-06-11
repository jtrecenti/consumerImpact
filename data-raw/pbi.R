codigos_assuntos_consumidor <- c(
  1156, 11974, 11868, 7771, 7752, 14757, 14758, #11806,
  10945, 14926, 11807, 11808, 7772, 11861, 7619, 7620,
  7773, 7761, 7760, 6233, 12222, 12225, 12223, 12224,
  11860, 7621, 7775, 7774, 7617, 7626, 10598, 7627, 4862,
  7748, 4829, 4830, 4832, 4831, 11809, 7776, 11814, 11815,
  7618, 12930, 12931, 11810, 11864, 11866, 11812, 11811,
  6220, 7769, 7780, 7779, 12042, 6226, 7781, 7770, 11867,
  14925, 7768, 7767, 15048, 11865
)

loc <- readr::locale(encoding = "UTF-8")
aux_pbi <- fs::dir_ls("data-raw/pbi") |>
  purrr::map(\(x) readr::read_csv2(x, locale = loc)) |>
  purrr::list_rbind(names_to = "source") |>
  janitor::clean_names()

aux_pbi |>
  dplyr::glimpse()

aux_pbi_assuntos_1grau <- aux_pbi |>
  dplyr::filter(grau %in% c("JE", "G1")) |>
  dplyr::mutate(
    assuntos = stringr::str_remove_all(codigos_assuntos, "[{}]"),
    assuntos = stringr::str_split(assuntos, ",")
  ) |>
  tibble::rowid_to_column() |>
  tidyr::unnest(assuntos) |>
  dplyr::filter(assuntos %in% codigos_assuntos_consumidor) |>
  tidyr::nest(assuntos = assuntos)


aux_pbi_assuntos_1grau |>
  dplyr::count(grau)

aux_intervencoes <- tibble::tibble(
  tribunal = c("TJDFT", "TJMT", "TJMA"),
  dt_intervencao = lubridate::ymd(c("2020-11-24", "2022-11-28", "2023-05-25"))
)

aux_pbi_assuntos_1grau |>
  dplyr::mutate(ano_mes = lubridate::ymd(paste(ano, mes, "01"))) |>
  dplyr::count(tribunal, ano_mes) |>
  ggplot2::ggplot(ggplot2::aes(x = ano_mes, y = n)) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = dt_intervencao),
    colour = 2, linetype = 2,
    data = aux_intervencoes
  ) +
  ggplot2::facet_wrap(ggplot2::vars(tribunal), ncol = 1) +
  ggplot2::scale_x_date(date_labels = "%m\n%Y", date_breaks = "1 month") +
  ggplot2::theme_bw()


dplyr::glimpse(aux_pbi_assuntos_1grau)
