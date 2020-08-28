#' Update GH Pages for hosting conference certificate checking
#' @param ids_table `tibble` containing all certificate ids to be uploaded
#' @param repo GitHub repo to be used for the API checks,
#'   you need to have write access to this repo to upload
#' @param edition event edition
#' @inheritParams event_data
#' @export
update_api <- function(ids_table, event_data) {
  if (isTRUE(event_data$use_github_repo)) {
    repo <- event_data$cert_repo
    edition <- event_data$edition
  } else {
    stop("field event_data$use_github_repo from config file is not set")
  }
  edition_path <- create_edition_repo(event_data)

  # TODO move to prepare_infrastructure
  base_wd <- fs::path_wd()
  path_404 <- fs::path(base_wd, "templates", "webpage", "404.md")
  if (!fs::file_exists(path_404)) {
    fs::file_copy(path_404, edition)
  }

  # create shorter id
  # TODO check if 8 is enough
  # TODO add a loop that increase number if length is not equal
  ids_table <- ids_table %>%
    dplyr::mutate(id_short = stringr::str_extract(id, ".{8}"))
  # save ids
  long_num <- length(unique(dplyr::pull(ids_table, id)))
  short_num <- length(unique(dplyr::pull(ids_table, id_short)))
  if (!identical(long_num, short_num)) {
    stop("Short Ids are not unique.")
  }
  # check if there is already certs uploaded
  json_to_save <- ids_table %>%
    dplyr::select(-name)

  json_file_path <- fs::path(edition_path, "cert.json")
  if (fs::file_exists(json_file_path)) {
    saved_json <- jsonlite::fromJSON(json_file_path)
    saved_long_ids <- dplyr::pull(tibble::as_tibble(saved_json), id)
    json_to_save <- json_to_save %>%
      dplyr::filter(!(id %in% saved_long_ids))
    json_to_save <- saved_json %>%
      tibble::as_tibble() %>%
      dplyr::bind_rows(json_to_save)
  }
  # save new jsons, appending file if necessary
  json_to_save %>%
    jsonlite::toJSON(pretty = TRUE) %>%
    readr::write_lines(json_file_path)
  # Check if json can be loaded
  test_json <- jsonlite::fromJSON(json_file_path)
  rm(test_json)

  ids_list <- generate_ids_list(ids_table, event_data)
  # Generate folder for each certificate
  # event_data$paths$templates$webpage
  template_check_html <- fs::path(base_wd, "templates", "webpage", "certificate_check_template.md")
  if (!isTRUE(fs::file_exists(template_check_html))) {
    stop("Template for certificate check not found.")
  }
  #check_strings_html <- readr::read_lines(template_check_html)
  written_mds <- names(ids_list) %>%
    purrr::map(~{
      # temp_list <- ids_list[[names(ids_list)[1]]]
      temp_list <- ids_list[[.x]]
      # Generate folder for each certificate
      fs::dir_create(fs::path(edition_path, temp_list$id_short))
      correct_strings_html <- template_check_html %>%
        replace_template_strings(temp_list)
      correct_strings_html %>%
        readr::write_lines(fs::path(edition_path, temp_list$id_short, "index.md"))
      temp_list
  })
  rm(written_mds)

  # update and push remote repo
  update_edition_repo(repo, edition)

  return(ids_list)
}


####################aux functions #######################
#' create repo
create_edition_repo <- function(event_data) {
  repo <- event_data$cert_repo
  edition <- event_data$edition
  base_wd <- fs::path_wd()
  fs::dir_create(fs::path(base_wd, "temp_repo"))
  repo_name <- basename(repo)
  path_to_clone <- fs::path(base_wd, "temp_repo", repo_name)
  # TODO use packaget gert? gh?
  # TODO check for https or ssh preference; currently ssh; https in container
  sys::exec_wait(
    cmd = "git",
    args = c("clone", glue::glue("git@github.com:{repo}.git"), path_to_clone)
  )
  edition_path <- fs::path(path_to_clone, edition)
  fs::dir_create(edition_path)
  return(edition_path)
}

#' update repo
update_edition_repo <- function(repo, edition) {
  repo_path <- fs::path("temp_repo", basename(repo))
  sys::exec_wait(cmd = "git", args = c("-C", repo_path, "status"))
  sys::exec_wait(cmd = "git", args = c("-C", repo_path, "add", edition))
  sys::exec_wait(
    cmd = "git",
    args = c("-C", repo_path, "add", glue::glue("{edition}/cert.json"))
  )

  commit_date <- Sys.time() %>%
    as.character() %>%
    stringr::str_remove(".{2}") %>%
    stringr::str_remove_all("-") %>%
    stringr::str_remove_all(":") %>%
    stringr::str_replace("[[:blank:]]", "-") %>%
    stringr::str_remove(".{2}$")

  commit_message <- glue::glue("update cert list {commit_date}")

  sys::exec_wait(
    cmd = "git",
    args = c("-C", repo_path, "commit", "-m", commit_message)
  )
  sys::exec_wait(cmd = "git", args = c("-C", repo_path, "push"))
  message("Repo updated")
}
