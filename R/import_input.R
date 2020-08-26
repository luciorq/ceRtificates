
#' Read Atendes's table from Google Sheets
#' TODO add support to Google Sheets
#'   googlesheets::gs_ls(), googleshhets::gs_ws_ls(), googleshhets::gs_read()
#'   https://docs.google.com/spreadsheets/d/{SHEET_CODE}/
#'   https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html
#' @export
import_gsheets <- function() {

}

#' Read Atendes's table from Excel file
#' @param input_table Path to a XLSX file containing presence
#'   it can contain separate tabs/sheets for each course or category
#' @inheritParams import_table
#' @export
import_excel <- function(input_table, course_table) {

  participants_file <- input_table
  sheets_df <- course_table

  if (!isTRUE(readxl::excel_format(participants_file) == "xlsx")) {
    stop("File is not a correctly fomated XLSX file.")
  }
  sheets_vector <- readxl::excel_sheets(participants_file)

  # retrieve main event certs
  main_event_course <- dplyr::pull(dplyr::filter(sheets_df, course %in% "Evento Principal"), sheet_name)
  main_event_df <- readxl::read_xlsx(participants_file, sheet = main_event_course) %>%
    dplyr::mutate(sheet_name = main_event_course) %>%
    dplyr::left_join(
      dplyr::filter(sheets_df, sheet_name %in% main_event_course),
      by = "sheet_name"
    ) %>%
    dplyr::select(-sheet_name) %>%
    dplyr::rename(`Presença` = Credenciamento)

  # retrieve courses lists
  sheets_df <- sheets_df %>%
    dplyr::filter(!(category %in% "not_used")) %>%
    dplyr::filter(!(course %in% "Evento Principal"))

  participant_df <- tibble::tibble()
  # i <- sheets_df$sheet_name[1]
  for (i in sheets_df$sheet_name){
    temp_participant_df <- readxl::read_xlsx(participants_file, sheet = i)
    temp_participant_df <- temp_participant_df %>%
      dplyr::select(`Presença`, `Nome do Aluno`, `Email`, `Minicurso`) %>%
      dplyr::mutate(sheet_name = i)
    temp_participant_df <- temp_participant_df %>%
      dplyr::left_join(
        dplyr::filter(sheets_df, sheet_name %in% i),
        by = c("sheet_name")
      ) %>%
      dplyr::select(-sheet_name)
    participant_df <- dplyr::bind_rows(participant_df, temp_participant_df)
  }

  participant_df <- participant_df %>%
    dplyr::bind_rows(main_event_df)

  return(participant_df)
}

#' Format input table
#' @param input_table A Google Sheets code or XLSX file path containing sheets
#'   organized with sheets for each category of certificate
#' @param course_table `tibble`, `data.frame` or coercible object,
#'   containing course description
#' @param ext Default = ".xlsx"; File extension used for input table,
#'   can be replaced to Google Sheets(.gsheet)
#' @export
import_table <- function(input_table, course_table, ext = c(".xlsx", ".gs")) {
  # file converted from Google Sheets
  participants_file <- input_table
  sheets_df <- course_table
  if (isTRUE(ext == ".xlsx")) {
    participant_df <- import_excel(input_table, course_table)
  } else if (isTRUE(ext == ".gs")) {
    participant_df <- import_gsheets(input_table)
  } else {
    stop("File extension not known.")
  }

  # Validate email through RegExp
  email_regex <- "^[[:alnum:].\\-_]+@[[:alnum:].\\-]+$"
  # Format strings
  participant_table <- participant_df %>%
    dplyr::filter(`Presença` %in% c("Presente", "Realizado")) %>%
    dplyr::mutate(participant_name = stringr::str_to_title(`Nome do Aluno`)) %>%
    dplyr::mutate(participant_name = stringr::str_squish(participant_name)) %>%
    dplyr::mutate(valid_email = stringr::str_match(`Email`, email_regex)) %>%
    dplyr::mutate(valid_email = as.vector(.$valid_email)) %>%
    dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
    dplyr::mutate(course_name = stringr::str_squish(Minicurso)) %>%
    dplyr::select(
      course, category, valid_email,
      participant_name,
      event_date, cert_hours, edition
    )
  return(participant_table)
}

#' Format input table for missing certs
#' @param input_table A Google Sheets code or XLSX file path containing sheets
#'   organized with sheets for each category of certificate
#' @param course_table `tibble`, `data.frame` or coercible object,
#'   containing course description
#' @param ext Default = ".xlsx"; File extension used for input table,
#'   can be replaced to Google Sheets(.gsheet)
#' @export

import_table_missing <- function(input_table2, course_table, ext) {
  sheets_vector <- readxl::excel_sheets(input_table2)
  missing_table <- readxl::read_xlsx(input_table2, sheet = sheets_vector[2])
  missing_table <- missing_table %>%
    dplyr::filter(situacao %in% c("requisitou certificado faltante")) %>%
    dplyr::select(Minicurso, `Nome do Aluno`, Email)

  # Validate email through RegExp
  email_regex <- "^[[:alnum:].\\-_]+@[[:alnum:].\\-]+$"
  missing_table <- missing_table %>%
    dplyr::mutate(participant_name = stringr::str_to_title(`Nome do Aluno`)) %>%
    dplyr::mutate(participant_name = stringr::str_squish(participant_name)) %>%
    dplyr::mutate(valid_email = stringr::str_match(`Email`, email_regex)) %>%
    dplyr::mutate(valid_email = as.vector(.$valid_email)) %>%
    dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
    dplyr::mutate(course_name = stringr::str_squish(Minicurso))

  course_table2 <- dplyr::select(course_table, -sheet_name)

  missing_table <- missing_table %>%
    dplyr::distinct() %>%
    dplyr::select(course_name, participant_name, valid_email) %>%
    # dplyr::mutate(cetegory = dplyr::if_else(
    #     course_name == "Evento Principal", "participante", course_name
    # )) %>%
    dplyr::left_join(course_table2, by = c("course_name" = "course")) %>%
    dplyr::rename(course = course_name)
  return(missing_table)
}
