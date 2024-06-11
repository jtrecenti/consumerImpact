codigos_assuntos_consumidor <- c(
  # 11806, 7780, 7779, 6226, ## problematicos
  1156, 11974, 11868, 7771, 7752, 14757, 14758,
  10945, 14926, 11807, 11808, 7772, 11861, 7619, 7620,
  7773, 7761, 7760, 6233, 12222, 12225, 12223, 12224,
  11860, 7621, 7775, 7774, 7617, 7626, 10598, 7627, 4862,
  7748, 4829, 4830, 4832, 4831, 11809, 7776, 11814, 11815,
  7618, 12930, 12931, 11810, 11864, 11866, 11812, 11811,
  6220, 7769, 12042, 7781, 7770, 11867,
  14925, 7768, 7767, 15048, 11865
)

header <- c(
  "Authorization" = "ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==",
  "Content-Type" = "application/json"
)

body <- list(
  query = list(
    bool = list(
      must = list(
        match = list(
          "assuntos.codigo" = "0000"
        )
      )
    )
  ),
  size = 10000,
  sort = list(
    list(
      "@timestamp" = list(
        order = "asc"
      )
    )
  )
)

pesquisar_tribunal <- function(tribunal, body, path) {

  usethis::ui_info("Pesquisando tribunal {tribunal}...")
  fs::dir_create(path)
  assunto <- body$query$bool$must$match[["assuntos.codigo"]]
  f <- glue::glue("{path}/{assunto}.rds")
  if (!file.exists(f)) {

    endpoint <- glue::glue(
      "https://api-publica.datajud.cnj.jus.br/api_publica_{tribunal}/_search"
    )

    pag <- 1
    usethis::ui_info("Pesquisando página {pag}...")

    r <- httr::POST(
      endpoint,
      body = body,
      encode = "json",
      httr::add_headers(header)
    )
    r_list <- httr::content(r)$hits$hits

    len_list <- length(r_list)
    usethis::ui_info("Foram encontrados {len_list} registros.")

    while (len_list == 10000) {
      pag <- pag + 1
      usethis::ui_info("Pesquisando página {pag}...")
      last_sort <- r_list |>
        dplyr::last() |>
        purrr::pluck("sort") |>
        unlist()
      body$search_after <- list(last_sort)
      r <- httr::POST(
        endpoint,
        body = body,
        encode = "json",
        httr::add_headers(header)
      )
      r_pag <- httr::content(r)$hits$hits
      r_list <- c(r_list, r_pag)

      len_list <- length(r_pag)
      usethis::ui_info("Foram encontrados {len_list} registros.")
    }
    readr::write_rds(r_list, f)
  }

}

list_consumidor <- purrr::walk(codigos_assuntos_consumidor, \(x) {
  usethis::ui_info("Pesquisando assunto {x}...")
  body$query$bool$must$match[["assuntos.codigo"]] <- x
  pesquisar_tribunal("tjmt", body, "data-raw/tjmt")
  pesquisar_tribunal("tjdft", body, "data-raw/tjdft")
  pesquisar_tribunal("tjma", body, "data-raw/tjma")
})


## parse -------------- NAO FOI ADAPTADO DO TEMPLATE

parse_item <- function(x) {
  src <- x[["_source"]]
  tab_parsed <- tibble::tibble(
    id = src$id,
    n_processo = src$numeroProcesso,
    classe_id = src$classe$codigo,
    classe_nm = src$classe$nome,
    sistema_id = src$sistema$codigo,
    sistema_nm = src$sistema$nome,
    formato_id = src$formato$codigo,
    formato_nm = src$formato$nome,
    tribunal = src$tribunal,
    data_ultima_atualizacao = src$dataHoraUltimaAtualizacao,
    grau = src$grau,
    data_ajuizamento = src$dataAjuizamento,
    nivel_sigilo = src$nivelSigilo,
    orgao_julgador_ibge = src$orgaoJulgador$codigoMunicipioIBGE,
    orgao_julgador_id = src$orgaoJulgador$codigo,
    orgao_julgador_nm = src$orgaoJulgador$nome,
    movimentos = src$movimentos |>
      tibble::enframe() |>
      tidyr::unnest_wider(value) |>
      janitor::clean_names() |>
      list(),
    assuntos = src$assuntos |>
      purrr::map_if(\(x) length(x) == 1, \(x) x[[1]], .else = \(x) x) |>
      purrr::discard(\(x) length(x) != 2) |>
      tibble::enframe() |>
      tidyr::unnest_wider(value) |>
      list()
  )
  tab_parsed
}

# x <- res[[228]]

# parse_item(res[[228]])

# da_result <- purrr::map(res, parse_item, .progress = TRUE) |>
#   purrr::list_rbind(names_to = "elastic_id")



