#' Update GH Pages for hosting conference certificate checking
#' @param ids_table `tibble` containing all certificate ids to be uploaded
#' @param repo GitHub repo to be used for the API checks,
#'   you need to have write access to this repo to upload
#' @param edition event edition
#' @inheritParams event_data
#' @export
update_api <- function(ids_table, event_data) {
  # ids_table <- id_table
  # repo <- "cvbioinfoufmg/certificates"
  # edition <- "2020"

  if (isTRUE(event_data$use_github_repo)) {
    repo <- event_data$cert_repo
    edition <- event_data$edition
  } else {
    stop("field event_data$use_github_repo from config file is not set")
  }

  previous_wd <- fs::path_wd()
  fs::dir_create("temp_repo")
  setwd(fs::path("temp_repo"))
  # isolate repo creation function
  # TODO use packaget gert? gh?
  # TODO check for https or ssh preference; currently ssh
  sys::exec_wait("git", args = c("clone", glue::glue("git@github.com:{repo}.git")))
  setwd(fs::path(basename(repo)))
  fs::dir_create(edition)

  # TODO move to prepare_infrastructure
  path_404 <- fs::path(previous_wd, "templates", "webpage", "404.md")
  if (!fs::file_exists(path_404)) {
    fs::file_copy(path_404, edition)
  }

  # create shorter id
  # TODO add a loop that increase number if length is not equal
  ids_table <- ids_table %>%
    dplyr::mutate(id_short = stringr::str_extract(id, ".{8}"))
  # save ids
  long_num <- length(unique(dplyr::pull(ids_table, id)))
  short_num <- length(unique(dplyr::pull(ids_table, id_short)))
  if (!identical(long_num, short_num)) {
    setwd(previous_wd)
    stop("Short Ids are not unique.")
  }
  # check if there is already certs uploaded
  json_to_save <- ids_table %>%
    dplyr::select(-name)

  json_file_path <- fs::path(edition, "cert.json")
  if (fs::file_exists(json_file_path)) {
    saved_json <- jsonlite::fromJSON(json_file_path)
    saved_long_ids <- dplyr::pull(tibble::as_tibble(saved_json), id)
    json_to_save <- json_to_save %>%
      dplyr::filter(!(id %in% saved_long_ids))
  }

  # save new jsons, appending file if necessary
  json_to_save %>%
    jsonlite::toJSON(pretty = TRUE) %>%
    readr::write_lines(json_file_path, append = TRUE)

  # Generate folder for each certificate
  # event_data$paths$templates$webpage
  template_check_html <- fs::path(previous_wd, "templates", "webpage", "certificate_check_template.md")
  if(!isTRUE(fs::file_exists(template_check_html))) {
    setwd(previous_wd)
    stop("Template for certificate check not found.")
  }
  check_strings_html <- readr::read_lines(template_check_html)
  written_mds <- seq_along(ids_table$id_short) %>%
    purrr::map(~{
      # i <- 1
      i <- .x
      fs::dir_create(fs::path(edition, ids_table$id_short[i]))
      correct_strings_html <- check_strings_html %>%
        stringr::str_replace("##CERT_ID##", ids_table$id_short[i]) %>%
        stringr::str_replace("##CERT_TYPE##", ids_table$type[i]) %>%
        stringr::str_replace("##COURSE##", ids_table$course[i]) %>%
        stringr::str_replace("##EVENT_DATE##", ids_table$date[i]) %>%
        stringr::str_replace("##CERT_HOURS##", ids_table$hours[i]) %>%
        stringr::str_replace("##EVENT_EDITION##", ids_table$edition[i]) %>%
        stringr::str_replace("##FULL_ID##", ids_table$id[i])

      correct_strings_html %>%
        readr::write_lines(fs::path(edition, ids_table$id_short[i], "index.md"))
  })

  commit_date <- Sys.time() %>%
    as.character() %>%
    stringr::str_remove(".{2}") %>%
    stringr::str_remove_all("-") %>%
    stringr::str_remove_all(":") %>%
    stringr::str_replace("[[:blank:]]", "-") %>%
    stringr::str_remove(".{2}$")

  commit_message <- glue::glue("update cert list {commit_date}")

  sys::exec_wait("git", args = c("add", edition))
  sys::exec_wait("git", args = c("add", glue::glue("{edition}/cert.json")))
  sys::exec_wait("git", args = c("commit", "-m", commit_message))
  sys::exec_wait("git", args = c("push"))
  # return to executing folder
  setwd(previous_wd)
}
