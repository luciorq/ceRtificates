#' Prepare Input data for certificates
#' @param participant_table `tibble`, `data.frame` or coercible object,
#'   containing atendees info
#' @export
prepare_data <- function(participant_table) {
  # i <- 1
  # participant_lookup_df <- tibble::tibble()
  participant_lookup_df <- 1:nrow(participant_table) %>%
    purrr::map_df(~{
    i <- .x
    # Var init
    course <- participant_table$course[i]
    category <- participant_table$category[i]
    cert_hours <- participant_table$cert_hours[i]
    edition <- participant_table$edition[i]
    event_date <- participant_table$event_date[i]
    cert_name <- participant_table$participant_name[i]

    # format and normalize name for folders and paths
    participant_name_path <- stringr::str_replace_all(participant_table$participant_name[i], "[[:blank:]]", "_")
    p_name_path_wt_accent <- stringi::stri_trans_general(participant_name_path, "Latin-ASCII")
    formated_course_name <- participant_table$course[i] %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("[[:blank:]]", "_") %>%
      stringr::str_replace_all("/", "-") %>%
      stringi::stri_trans_general("Latin-ASCII")

    path_to_cert <- fs::path("certs", fs::path_sanitize(p_name_path_wt_accent))
    if (!isTRUE(fs::dir_exists(path_to_cert))) {
      fs::dir_create(path_to_cert)
    }

    # unique certificate identifier
    id_string <- glue::glue("{p_name_path_wt_accent}-{formated_course_name}-{cert_hours}-{edition}")
    unique_id <- generate_hash(id_string, type = "long")
    id_string <- as.character(unique_id)

    lookup_df <- tibble::tibble(
      id = id_string,
      name = cert_name,
      course = course,
      type = category,
      hours = cert_hours,
      edition = edition,
      date = event_date
    )
    return(lookup_df)
  })
  return(participant_lookup_df)
}
