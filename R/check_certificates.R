#' Generate PDF certificate
#' @inheritParams prepare_data
#' @param base_site Webpage to be linked
#' @export
check_certificates <- function(ids_table, base_site, min_size_pdf = 870) {

  # Function to generate cert in parallel
  # i = 1
  gen_cert_pdf <- function(i) {
    message(ids_table$name[i])
    participant_name_path <- stringr::str_replace_all(ids_table$name[i], "[[:blank:]]", "_")
    p_name_path_wt_accent <- stringi::stri_trans_general(participant_name_path, "Latin-ASCII")
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

    # check if template exists

    # pdf file path
    file_name_path <- glue::glue("certs/{p_name_path_wt_accent}/{p_name_path_wt_accent}-{category_path}-CV_bioinfo_{edition}-UFMG.pdf")
    # html file path
    html_file_path <- glue::glue("temp/{p_name_path_wt_accent}-{category_path}-CV_bioinfo_{edition}-UFMG.html")

    if (!fs::file_exists(html_file_path)) {
      message(glue::glue("html_file_path don't exist."))
      template_html_path <- glue::glue("templates/html/certificate_{ids_table$type[i]}_template.html")
      template_html <- readr::read_lines(template_html_path)

      qrcode_path <- glue::glue("qrcode/{ids_table$id_short[i]}.svg")


      cert_url <- as.character(glue::glue("{base_site}{ids_table$edition[i]}/{ids_table$id_short[i]}"))

      #file_name_path <- glue::glue("certs/{p_name_path_wt_accent}/{p_name_path_wt_accent}-{category_path}-CV_bioinfo_{edition}-UFMG.pdf")

      # Replace strings in the template
      participant_html <- template_html %>%
        stringr::str_replace("##PARTICIPANT_NAME##", ids_table$name[i]) %>%
        stringr::str_replace("##COURSE_TITLE##", ids_table$course[i]) %>%
        stringr::str_replace("##EVENT_DATE##", ids_table$date[i]) %>%
        stringr::str_replace("##CERT_HOURS##", ids_table$hours[i]) %>%
        stringr::str_replace("##QRCODE_PATH##", qrcode_path) %>%
        stringr::str_replace("##CERT_URL##", cert_url) %>%
        stringr::str_replace("##HASH_CODE##", ids_table$id[i])
      # write to temp file
      participant_html %>%
        readr::write_lines(glue::glue("temp/{p_name_path_wt_accent}-{category_path}-CV_bioinfo_{edition}-UFMG.html"))
    } else {
      if (isTRUE(fs::file_exists(file_name_path))) {
        pdf_file_size <- as.numeric(fs::file_size(file_name_path))/1024
        if (isTRUE(min_size_pdf > pdf_file_size)) {
          warning(glue::glue("Apparently {file_name_path} has the wrong size. Check it."))
          pagedown::chrome_print(
            input = html_file_path,
            output = glue::glue("{file_name_path}"),
            wait = 5,
            format = "pdf",
            timeout = 120,
            verbose = 1
          )
        }
        return(0)
      }
    }

    if (!isTRUE(fs::file_exists(file_name_path))) {
      # Print to PDF
      message(glue::glue("Saving certificate: {ids_table$course[i]} - {ids_table$name[i]}"))
      pagedown::chrome_print(
        input = html_file_path,
        output = glue::glue("{file_name_path}"),
        wait = 5,
        format = "pdf",
        timeout = 120,
        verbose = 1
      )
    } else {
      message(glue::glue("{file_name_path} already exists."))
    }
  }

  # ids_table = id_table
  ids_table <- ids_table %>%
    dplyr::mutate(id_short = stringr::str_extract(id, ".{8}")) %>%
    dplyr::arrange(name)


  # Check number of certificates
  vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs/"))
  cert_number <- length(vector_pdf_path)

  # Just stop when every file is ready
  while (nrow(ids_table) != cert_number) {
    #cert_number <- cert_number + 1
    # i = 1
    return_list <- 1:nrow(ids_table) %>%
      purrr::map(gen_cert_pdf)
    # update value for while condition
    vector_pdf_path <- fs::dir_ls(fs::dir_ls("certs/"))
    cert_number <- length(vector_pdf_path)
    cert_missing_number <- nrow(ids_table) - cert_number
    print(cert_missing_number)
    wrong_size_files <- sum(as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf)
    cert_number <- cert_number - wrong_size_files
    message("Certs with wrong size:", "\n", paste0("    ",vector_pdf_path[as.numeric(fs::file_size(vector_pdf_path))/1024 <= min_size_pdf], sep = "\n"))
  }
}



