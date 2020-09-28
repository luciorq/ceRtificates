#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_url Webpage to be linked
#' @export
check_certificates <- function(ids_list, event_data, min_size_pdf = NULL) {
  # Check number of possible certificates
  vector_pdf_path <- purrr::map_chr(ids_list, ~{unlist(.x)["pdf_path"]})
  certs_to_be_created <- vector_pdf_path[fs::file_exists(vector_pdf_path)]
  certs_in_folder <- fs::dir_ls(fs::dir_ls("certs"))
  already_created_pdf_path <- certs_to_be_created[certs_to_be_created %in% certs_in_folder]
  # Just stop when every file is ready
  while (length(ids_list) != length(already_created_pdf_path)) {
    return_list <- names(ids_list) %>%
      # TODO implement
      # future::plan(future::multisession, workers = 16)
      # furrr::future_map(~{})
      purrr::map(~{
        temp_list <- ids_list[[.x]]
        gen_cert_pdf(temp_list, min_size_pdf)
      })
    # future::plan(future::sequential)
    if (is.null(min_size_pdf)) {
      min_size_pdf <- check_files_size_distribution(already_created_pdf_path)
    }
    # update value of already created certificates
    already_created_pdf_path <- certs_to_be_created[certs_to_be_created %in% already_created_pdf_path]
    wrong_size_files <- already_created_pdf_path[as.numeric(fs::file_size(already_created_pdf_path))/1024 <= min_size_pdf]
    # remove wrong size files from already created vector
    wrong_size_files <- already_created_pdf_path[!(already_created_pdf_path %in% wrong_size_files)]
    if (length(wrong_size_files) > 0) {
      message(
        "Certs with wrong size:", "\n", paste0("    ", wrong_size_files, collapse = "\n")
      )
    }
  }
  message("Generated all PDF certificates.")
  message(glue::glue("{length(already_created_pdf_path)} certificates ready."))
}

#' Check pdf certs size distribution
#' Check mean file size to garantee that they were generated correctly
check_files_size_distribution <- function(already_created_pdf_path) {
  cert_path_vector <- already_created_pdf_path
  cert_size_vector <- as.numeric(fs::file_size(cert_path_vector))/1024
  cert_size_vector <- sort(cert_size_vector, decreasing = TRUE)
  min_size_pdf <- mean(cert_size_vector) * 0.95
  # mad(cert_size_vector)
  # sd(cert_size_vector, na.rm = TRUE)
  return(min_size_pdf)
}
