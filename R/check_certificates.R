#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_url Webpage to be linked
#' @export
check_certificates <- function(ids_list, event_data, min_size_pdf = NULL) {
  # Check number of possible certificates
  vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs"))
  cert_number <- length(vector_pdf_path)
  # Just stop when every file is ready
  while (length(ids_list) != cert_number) {
    return_list <- names(ids_list) %>%
      # future::plan(future::multisession, workers = 16)
      # furrr::future_map(~{})
      purrr::map(~{
        temp_list <- ids_list[[.x]]
        gen_cert_pdf(temp_list, min_size_pdf)
      })
    # future::plan(future::sequential)
    if (is.null(min_size_pdf)) {
      min_size_pdf <- check_files_size_distribution("certs")
    }

    # update value for while condition
    vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs"))
    cert_number <- length(vector_pdf_path)
    cert_missing_number <- length(ids_list) - cert_number
    print(cert_missing_number)
    wrong_size_files <- sum(as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf)
    cert_number <- cert_number - wrong_size_files
    message("Certs with wrong size:", "\n", paste0("    ",vector_pdf_path[as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf], sep = "\n"))
  }
  message("Generated all PDF certificates.")
  message(glue::glue("{cert_number} certificates ready."))
}

#' Check pdf certs size distribution
#' Check mean file size to garantee that they were generated correctly
check_files_size_distribution <- function(cert_path = "certs") {
  cert_path_vector <- fs::dir_ls(fs::dir_ls(cert_path))
  cert_size_vector <- as.numeric(fs::file_size(cert_path_vector))/1024
  cert_size_vector <- sort(cert_size_vector, decreasing = TRUE)
  # uses mean size of the gets
  min_size_pdf <- mean(cert_size_vector[seq_len(floor(length(cert_size_vector)/10))]) * 0.95
  # sd(cert_size_vector[seq_len(floor(length(cert_size_vector)/10))])
  # cert_path_vector <- fs::dir_ls(fs::dir_ls(cert_path))
  # cert_size_vector <- as.numeric(fs::file_size(cert_path_vector))/1024
  # cert_size_vector <- sort(cert_size_vector, decreasing = TRUE)
  # median(cert_size_vector)
  # sd(cert_size_vector)

  return(min_size_pdf)
}
