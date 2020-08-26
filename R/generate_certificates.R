#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_site Webpage to be linked
#' @export
generate_certificates <- function(ids_table, base_site) {
  # ids_table = id_table
  ids_table <- ids_table %>%
    dplyr::mutate(id_short = stringr::str_extract(id, ".{8}")) %>%
    dplyr::arrange(name)

  fs::dir_create("temp", "qrcode")
  # Generate qrcode for the id
  # i = 1
  for (i in 1:nrow(ids_table)) {
    qr_code_string <- generate_qrcode(ids_table$id_short[i], base_site, ids_table$edition[i])
    #fs::filefs::path("temp", "qrcode", glue::glue("{ids_table$id_short[i]}.svg"))
  }
  # Function to generate cert in parallel
  # i = 1
  # for (i in 1:nrow(ids_table)) {
  gen_cert_html <- function(i) {
    template_html_path <- glue::glue("templates/html/certificate_{ids_table$type[i]}_template.html")
    template_html <- readr::read_lines(template_html_path)

    qrcode_path <- glue::glue("qrcode/{ids_table$id_short[i]}.svg")


    cert_url <- as.character(glue::glue("{base_site}{ids_table$edition[i]}/{ids_table$id_short[i]}"))

    # Replace strings in the template
    participant_html <- template_html %>%
      stringr::str_replace("##PARTICIPANT_NAME##", ids_table$name[i]) %>%
      stringr::str_replace("##COURSE_TITLE##", ids_table$course[i]) %>%
      stringr::str_replace("##EVENT_DATE##", ids_table$date[i]) %>%
      stringr::str_replace("##CERT_HOURS##", ids_table$hours[i]) %>%
      stringr::str_replace("##QRCODE_PATH##", qrcode_path) %>%
      stringr::str_replace("##CERT_URL##", cert_url) %>%
      stringr::str_replace("##HASH_CODE##", ids_table$id[i])

    # format and normalize name for folders and paths
    participant_name_path <- stringr::str_replace_all(ids_table$name[i], "[[:blank:]]", "_")
    p_name_path_wt_accent <- stringi::stri_trans_general(participant_name_path, "Latin-ASCII")
    if (!isTRUE(fs::dir_exists(glue::glue("certs/{p_name_path_wt_accent}")))) {
      fs::dir_create(glue::glue("certs/{p_name_path_wt_accent}"))
    }

    category <- ids_table$type[i]
    edition <- ids_table$edition[i]
    if (isTRUE(category == "minicurso")) {
      category_path <- ids_table$course[i] %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("[[:blank:]]", "_") %>%
        stringr::str_replace_all("/", "-") %>%
        stringi::stri_trans_general("Latin-ASCII")
    } else {
      category_path <- category
    }
    # write to temp file
    participant_html %>%
      readr::write_lines(glue::glue("temp/{p_name_path_wt_accent}-{category_path}-CV_bioinfo_{edition}-UFMG.html"))
    # generate pdf certificate
    # pagedown::find_chrome()
    if (!isTRUE(fs::dir_exists("certs"))) {
      fs::dir_create("certs")
    }
  }
  return_list <- 1:nrow(ids_table) %>%
    purrr::map(gen_cert_html)
}

