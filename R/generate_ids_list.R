#' Generate Description list
#' @export
generate_ids_list <- function(ids_table, event_data) {
  ids_list <- seq_len(nrow(ids_table)) %>%
    purrr::map(~{
      i <- .x
      temp_list <- list()
      temp_list$id_short <- ids_table$id_short[i]
      temp_list$type <- ids_table$type[i]
      temp_list$course <- ids_table$course[i]
      temp_list$date <- ids_table$date[i]
      temp_list$hours <- ids_table$hours[i]
      temp_list$edition <- ids_table$edition[i]
      temp_list$id <- ids_table$id[i]
      temp_list$name <- ids_table$name[i]
      temp_list$email <- ids_table$email[i]
      temp_list$cert_url <- as.character(glue::glue("{event_data$base_url}{ids_table$edition[i]}/{ids_table$id_short[i]}"))
      temp_list$qrcode_path <- as.character(glue::glue("qrcode/{ids_table$id_short[i]}.svg"))
      temp_list$path_name <- ids_table$name[i] %>%
        sanitize_string_path()
      temp_list$path_course <- ids_table$course[i] %>%
        sanitize_string_path()
      temp_list$html_path <- as.character(glue::glue(
        "temp/{temp_list$path_name}-{temp_list$type}-{temp_list$path_course}-CV_bioinfo_{temp_list$edition}-UFMG.html"
      ))
      temp_list$pdf_path <- as.character(glue::glue(
        "certs/{temp_list$path_name}/{temp_list$path_name}-{temp_list$type}-{temp_list$path_course}-CV_bioinfo_{temp_list$edition}-UFMG.pdf"
      ))
      return(temp_list)
    })
  names(ids_list) <- ids_table$id_short
  return(ids_list)
}

#' Sanitize Strings for Path Usage
sanitize_string_path <- function(string) {
  clean_string <- string %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("[[:blank:]]", "_") %>%
    stringr::str_replace_all("/", "-") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    fs::path_sanitize()
  return(clean_string)
}

#' Replace User Strings from template Markdown or HTML
#' @export
replace_template_strings <- function(template_file_path, temp_list) {
  replaced_strings_file <- template_file_path %>%
    readr::read_lines() %>%
    stringr::str_replace("##CERT_ID##", temp_list$id_short) %>%
    stringr::str_replace("##CERT_TYPE##", temp_list$type) %>%
    stringr::str_replace("##COURSE##", temp_list$course) %>%
    stringr::str_replace("##EVENT_DATE##", temp_list$date) %>%
    stringr::str_replace("##CERT_HOURS##", temp_list$hours) %>%
    stringr::str_replace("##EVENT_EDITION##", temp_list$edition) %>%
    stringr::str_replace("##FULL_ID##", temp_list$id) %>%
    stringr::str_replace("##CERT_URL##", temp_list$cert_url) %>%
    stringr::str_replace("##HASH_CODE##", temp_list$id) %>%
    stringr::str_replace("##QRCODE_PATH##", temp_list$qrcode_path) %>%
    stringr::str_replace("##PARTICIPANT_NAME##", temp_list$name) %>%
    stringr::str_replace("##COURSE_TITLE##", temp_list$course)
  return(replaced_strings_file)
}
