#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_url Webpage to be linked
#' @export
generate_certificates <- function(ids_list, event_data) {
  fs::dir_create("temp", "qrcode")
  # Generate qrcode for the id
  return_list <- names(ids_list) %>%
    purrr::map(~{
      temp_list <- ids_list[[.x]]
      qr_code_string <- generate_qrcode(temp_list$id_short, event_data$base_url, temp_list$edition)
    })
  if (!isTRUE(fs::dir_exists("certs"))) {
    fs::dir_create("certs")
  }
  return_list <- names(ids_list) %>%
    purrr::map(~{
      temp_list <- ids_list[[.x]]
      gen_cert_html(temp_list)
    })
}

#' Function to generate HTML certificates
#' @param temp_list info for one certificate
gen_cert_html <- function(temp_list) {
  template_html_path <- fs::path(glue::glue(
    "templates/html/certificate_{temp_list$type}_template.html"
  ))
  # Replace strings in the template
  participant_html <- template_html_path %>%
    replace_template_strings(temp_list)
  # create participant folder
  if (!isTRUE(fs::dir_exists(glue::glue("certs/{temp_list$path_name}")))) {
    fs::dir_create(glue::glue("certs/{temp_list$path_name}"))
  }
  html_certificate_output <- fs::path(temp_list$html_path)
  participant_html %>%
    readr::write_lines(html_certificate_output)

  ## Check if generated certs number are right
  if (length(fs::dir_ls("temp/", glob = "*.html")) != length(unique(names(ids_list)))) {
    stop("HTML templates are in different numbers from input table.")
  }
}

########### PDF cert
# temp_list <- ids_list[[names(ids_list)[1]]]
#' function to generate PDF certificates
gen_cert_pdf <- function(temp_list, min_size_pdf = 870) {
  # pdf file path
  pdf_file_name_path <- temp_list$pdf_path
  # html file path
  html_file_path <- temp_list$html_path

  if (!fs::file_exists(html_file_path)) {
    message(glue::glue("{temp_list$html_path} don't exist."))
    # ("temp/{ temp_list$path_name}-{category_path}-{temp_list$path_course}-CV_bioinfo_{edition}-UFMG.html")
    template_html_path <- glue::glue("templates/html/certificate_{temp_list$type}_template.html")
    # Replace strings in the template
    participant_html <- template_html_path %>%
      replace_template_strings(temp_list)
    # write to temp file
    #"temp/{ temp_list$path_name}-{category_path}-{temp_list$path_course}-CV_bioinfo_{edition}-UFMG.html"
    participant_html %>%
      readr::write_lines(temp_list$html_path)
  } else {
    if (isTRUE(fs::file_exists(pdf_file_name_path))) {
      pdf_file_size <- as.numeric(fs::file_size(pdf_file_name_path))/1024
      if (isTRUE(min_size_pdf > pdf_file_size)) {
        warning(glue::glue("Apparently {pdf_file_name_path} has the wrong size. Check it."))
        message("Generating again:")
        message(glue::glue("Saving certificate: {temp_list$course} - {temp_list$name}"))
        pagedown::chrome_print(
          input = html_file_path,
          output = pdf_file_name_path,
          wait = 5,
          format = "pdf",
          timeout = 120,
          verbose = 1
        )
      }
      return(0)
    }
  }
  if (!isTRUE(fs::file_exists(pdf_file_name_path))) {
    # Print to PDF
    message(glue::glue("Saving certificate: {temp_list$course} - {temp_list$name}"))
    #fs::dir_create(pdf_file_name_path)
    pagedown::chrome_print(
      input = html_file_path,
      output = pdf_file_name_path,
      wait = 5,
      format = "pdf",
      timeout = 120,
      verbose = 1
    )
  } else {
    message(glue::glue("{pdf_file_name_path} already exists."))
  }
}
