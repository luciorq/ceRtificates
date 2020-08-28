#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_url Webpage to be linked
#' @export
check_certificates <- function(ids_list, event_data, min_size_pdf = 870) {
  # Check number of possible certificates
  vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs/"))
  cert_number <- length(vector_pdf_path)
  # Just stop when every file is ready
  while (length(ids_list) != cert_number) {
    return_list <- names(ids_list) %>%
      purrr::map(~{
        temp_list <- ids_list[[.x]]
        gen_cert_pdf(temp_list)
      })
    # update value for while condition
    vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs/"))
    cert_number <- length(vector_pdf_path)
    cert_missing_number <- length(ids_list) - cert_number
    print(cert_missing_number)
    wrong_size_files <- sum(as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf)
    cert_number <- cert_number - wrong_size_files
    message("Certs with wrong size:", "\n", paste0("    ",vector_pdf_path[as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf], sep = "\n"))
  }
}



