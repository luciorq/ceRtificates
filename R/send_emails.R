#' Send E-mails with certificates to atendees
#' @inheritParams prepare_data
#' @export
send_certificates <- function(ids_list, event_data) {
   # Create a credentials file for sending
  # email through GMail, if it don't exist
  if (!isTRUE(fs::file_exists("email_creds"))) {
    # create credential files, will open a window asking for the password
    blastula::create_smtp_creds_file(
      file = "email_creds",
      user = "cursobioinfoufmg@gmail.com",
      provider = "gmail",
      use_ssl = TRUE
    )
  }

  # create dir to save sent emails
  fs::dir_create("log_emails")

  #if (fs::file_exists("log_emails/sent_log.txt")) {
  #  sent_table <- readr::read_csv("log_emails/sent_log.txt", col_names = FALSE)
  #  if (!nrow(sent_table) == 0) {
  #    colnames(sent_table) <- c("short_id","valid_email", "type", "course_name")
  #    sent_table <- sent_table %>%
  #      dplyr::mutate(sent = TRUE) %>%
  #      dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
  #      dplyr::distinct()
  #  } else {
  #    sent_table <- tibble::tibble(
  #      short_id = character(),
  #      valid_email = character(),
  #      type = character(),
  #      course_name = character(),
  #      sent = logical()
  #  }
  #} else {
  #  sent_table <- tibble::tibble(
  #    short_id = character(),
  #    valid_email = character(),
  #    type = character(),
  #    course_name = character(),
  #    sent = logical()
  #  )
  #}
  # ids_list

  # ids_table <- ids_table %>%
  #   dplyr::filter(type != "minicurso") %>%
  #   dplyr::left_join(sent_table, by = c("valid_email", "type")) %>%
  #   dplyr::distinct() %>%
  #   dplyr::filter(is.na(sent)) %>%
  #   dplyr::select(-sent) %>%
  #   dplyr::arrange(name) %>%
  #   dplyr::distinct()
#
  # ids_table <- ids_table %>%
  #   dplyr::filter(!is.na(valid_email))
#
  if (length(ids_list) == 0) {
    message("No e-mail to send.")
    return(0)
  }
  return_list <- names(ids_list) %>%
    purrr::map(~{
      temp_list <- ids_list[[.x]]
      send_email_with_attachment(temp_list)
    })
}

#' Function to send email with attachments
send_email_with_attachment <- function(temp_list) {
  if (temp_list$type == "minicurso") {
    return(0)
  }
  if (temp_list$type == "monitor_mincurso") {
    return(0)
  }
  email_template_path <- glue::glue("templates/email/template_email-{temp_list$type}.Rmd")
  email_obj <- blastula::render_email(email_template_path)

  # create vector of attachment files
  if (temp_list$type %in% c("participante", "monitor")) {
    attachment_vector <- fs::dir_ls(glue::glue("certs/{temp_list$path_name}"))
  } else {
    course_name_ascii <- temp_list$path_course
    attachment_vector <- fs::dir_ls(glue::glue("certs/{temp_list$path_name}"))
    attachment_vector <- attachment_vector[stringr::str_detect(attachment_vector, course_name_ascii)]
  }

  # temp_list$path_name <- "Andre_Saraiva_Leao_Marcelo_Antunes"
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

  # Send email
 email_to_send <- temp_list$email
  # to = "heronoh@gmail.com",
  # email_to_send = "luciorqueiroz@gmail.com"
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

  glue::glue("{temp_list$id_short},{email_to_send},{temp_list$type},{temp_list$course}") %>%
    readr::write_lines("log_emails/sent_log.txt", append = TRUE)
  return(0)
}
