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
      user = event_data$organizer$email,
      provider = "gmail",
      use_ssl = TRUE
    )
  }

  # create dir to save sent emails
  fs::dir_create("log_emails")
  # if log file existes create a table with sent information to filter
  if (fs::file_exists("log_emails/sent_log.txt")) {
    suppressMessages({
      sent_table <- readr::read_csv("log_emails/sent_log.txt", col_names = FALSE)
    })
    if (!nrow(sent_table) == 0) {
      colnames(sent_table) <- c("id_short", "email", "pdf_path")
      sent_table <- sent_table %>%
        dplyr::distinct()
    } else {
      sent_table <- tibble::tibble(
        id_short = character(),
        email = character(),
        pdf_path = character()
      )
    }
  } else {
    sent_table <- tibble::tibble(
      id_short = character(),
      email = character(),
      pdf_path = character()
    )
  }

  # define short hash to filter
  ids_to_remove <- unique(sent_table$id_short)
  filtered_list <- ids_list
  for (i in ids_to_remove) {
    filtered_list[[i]] <- NULL
  }
  message(glue::glue("E-mails to send: {length(filtered_list)}."))
  # length(filtered_list)

  if (length(filtered_list) == 0) {
    message("No e-mail to send.")
    return(0)
  }

  # send emails
  return_list <- names(filtered_list) %>%
    purrr::map(~{
      temp_list <- filtered_list[[.x]]
      send_email_with_attachment(temp_list, event_data)
    })

  # FIXME replace short ids that are repeated in the sent_log txt
  message("Sent all e-mails.")
}

#' Function to send email with attachments
send_email_with_attachment <- function(temp_list, event_data) {
  email_template_path <- glue::glue("templates/email/template_email-{temp_list$type}.Rmd")
  email_obj <- blastula::render_email(email_template_path)

  if (fs::file_exists("log_emails/sent_log.txt")) {
    suppressMessages({
      sent_table <- readr::read_csv("log_emails/sent_log.txt", col_names = FALSE)
    })
    if (!nrow(sent_table) == 0) {
      colnames(sent_table) <- c("id_short", "email", "pdf_path")
      sent_table <- sent_table %>%
        dplyr::distinct()
    } else {
      sent_table <- tibble::tibble(
        id_short = character(),
        email = character(),
        pdf_path = character()
      )
    }
    already_sent_vector <- unique(sent_table$pdf_path)
    if (temp_list$pdf_path %in% already_sent_vector) {
      return(0)
    }
  }


  # create vector of attachment files for category
  certs_vector <- fs::path("certs", temp_list$path_name) %>%
    fs::dir_ls()
  attachment_vector <- certs_vector[stringr::str_detect(certs_vector, glue::glue("-{temp_list$type}-"))]

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
  # format email subject
  if (event_data$event_lang == "ptbr") {
    subject_string <- "Certificado"
  } else {
    subject_string <- "Certificate"
  }
  category_to_print <- temp_list$type %>%
    stringr::str_replace("_", " ") %>%
    stringr::str_to_sentence()
  email_subject <- glue::glue("{subject_string} - {category_to_print} - {event_data$event_name} - {event_data$edition}")
  # Send email
  email_to_send <- temp_list$email
  #email_to_send = "test_email@gmail.com"
  message(glue::glue("Sending email to: {email_to_send}"))
  blastula::smtp_send(
    email = email_obj,
    from = event_data$organizer$email,
    to = email_to_send,
    subject = email_subject,
    credentials = blastula::creds_file(file = "email_creds")
  )

  # use random times between send to do not spam, not sure if it works
  Sys.sleep(round(runif(1, min = 3, max = 8)))
  # FIXME testing
  for (i in attachment_vector) {
    glue::glue("{temp_list$id_short},{email_to_send},{i}") %>%
      readr::write_lines("log_emails/sent_log.txt", append = TRUE)
  }
  # end of testing
  return(0)
}
