#' Send E-mails with certificates to atendees
#' @inheritParams prepare_data
#' @export
send_certificates <- function(ids_table, participant_table) {
  # ids_table <- id_table
  # Create a credentials file for sending
  # email through GMail, if it don't exist
  if (!isTRUE(fs::file_exists("email_creds"))) {
    # create credential files, will open a window asking for the password
    blastula::create_smtp_creds_file(
      file = "email_creds",
      user = "cursobioinfoufmg@gmail.com",
      provider = "gmail"
    )
  }

  # ids_table <- id_table
  ids_table <- ids_table %>%
    dplyr::mutate(id_short = stringr::str_extract(id, ".{8}")) %>%
    dplyr::arrange(name) %>%
    dplyr::filter(!(type == "minicurso")) %>%
    dplyr::distinct()
  #

  email_df <- participant_table %>%
    dplyr::select(valid_email, participant_name) %>%
    dplyr::rename(valid_email = 1)

  ids_table <- ids_table %>%
    dplyr::left_join(email_df, by = c("name" = "participant_name")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(name)
  # i = 1
  send_email_with_attachment <- function(i) {
    if (ids_table$type[i] == "minicurso") {
      return(0)
    }
    email_template_path <- glue::glue("templates/email/template_email-{ids_table$type[i]}.Rmd")
    email_obj <- blastula::render_email(email_template_path)

    participant_name_path <- stringr::str_replace_all(ids_table$name[i], "[[:blank:]]", "_")
    p_name_path_wt_accent <- stringi::stri_trans_general(participant_name_path, "Latin-ASCII")

    # create vector of attachment files
    if (ids_table$type[i] == "participante") {
      attachment_vector <- fs::dir_ls(glue::glue("certs/{p_name_path_wt_accent}"))
    } else {
      #fs::dir_ls(glue::glue("certs/{p_name_path_wt_accent}"))
      course_name_ascii <- ids_table$course[i] %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("[[:blank:]]", "_") %>%
        stringr::str_replace_all("/", "-") %>%
        stringi::stri_trans_general("Latin-ASCII")
      attachment_vector <- fs::dir_ls(glue::glue("certs/{p_name_path_wt_accent}"))
      attachment_vector <- attachment_vector[stringr::str_detect(attachment_vector, course_name_ascii)]
    }

    # p_name_path_wt_accent <- "Andre_Saraiva_Leao_Marcelo_Antunes"
    # attachment_file <- attachment_vector[1]
    for (attachment_file in attachment_vector) {
      # Add attachment
      email_obj <- blastula::add_attachment(
        email = email_obj,
        file = attachment_file,
        content_type = mime::guess_type(attachment_file),
        filename = basename(attachment_file)
      )
    }

    email_to_send <- ids_table$valid_email[i]
    # Send email
    # to = "heronoh@gmail.com",
    # to = "luciorqueiroz@gmail.com",
    # to = "alessandralima92@gmail.com",

    message(glue::glue("Sending email to: {email_to_send}"))
    blastula::smtp_send(
      email = email_obj,
      from = "cursobioinfoufmg@gmail.com",
      to = email_to_send,
      subject = "Certificado - IV Curso de Verão em Bioinformática da UFMG - 2020",
      credentials = blastula::creds_file(file = "email_creds")
    )
    Sys.sleep(round(runif(1, min = 3, max = 8)))

    glue::glue("{email_to_send},{ids_table$type[i]},{ids_table$course[i]}") %>%
      readr::write_lines("log_emails/sent_log.txt", append = TRUE)
  }

  fs::dir_create("log_emails")

  if (fs::file_exists("log_emails/sent_log.txt")) {
    sent_table <- readr::read_csv("log_emails/sent_log.txt", col_names = FALSE)
    colnames(sent_table) <- c("valid_email", "type", "course_name")
    sent_table <- sent_table %>%
      dplyr::mutate(sent = TRUE) %>%
      dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
      dplyr::distinct()
  } else {
    sent_table <- tibble::tibble(
      valid_email = character(),
      type = character(),
      course_name = character(),
      sent = logical()
    )
  }
  ids_table <- ids_table %>%
    dplyr::filter(type != "minicurso") %>%
    dplyr::left_join(sent_table, by = c("valid_email", "type")) %>%
    dplyr::distinct() %>%
    dplyr::filter(is.na(sent)) %>%
    dplyr::select(-sent) %>%
    dplyr::arrange(name) %>%
    dplyr::distinct()

  ids_table <- ids_table %>%
    dplyr::filter(!is.na(valid_email))

  if(nrow(ids_table) == 0) {
    message("No e-mail to send.")
    return(0)
  }

  return_list <- 1:nrow(ids_table) %>%
    purrr::map(send_email_with_attachment)
}

