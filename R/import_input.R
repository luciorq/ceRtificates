
#' Read Atendes's table from Google Sheets
#' TODO add support to Google Sheets
#'   googlesheets::gs_ls(), googleshhets::gs_ws_ls(), googleshhets::gs_read()
#'   https://docs.google.com/spreadsheets/d/{SHEET_CODE}/
#'   https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html
#' @export
import_gsheets <- function() {

}

#' Import course and workshops description
#' @inheritParams import_table
#' @export
import_course_table <- function(event_data) {
  course_table <- event_data$participant_table$path %>%
    readxl::read_xlsx(sheet = "course_lookup") %>%
    dplyr::mutate(edition = event_data$edition)
  return(course_table)
}
#' Read Atendes's table from Excel file
#' @param input_table Path to a XLSX file containing presence
#'   it can contain separate tabs/sheets for each course or category
#' @inheritParams import_table
#' @export
import_excel <- function(event_data) {
  participants_file <- event_data$participant_table$path
  course_table <- import_course_table(event_data)

  if (!isTRUE(readxl::excel_format(participants_file) == "xlsx")) {
    stop("File is not a correctly formated XLSX file.")
  }

  categories_vector <- unique(event_data$cert_type)
  sheets_vector <- readxl::excel_sheets(participants_file)
  sheets_vector <- sheets_vector[!stringr::str_detect(sheets_vector, "_lookup$")]

  categories_list <- list()
  # i = categories_vector[1]; j = sheets_vector[1]
  for (i in categories_vector) {
    category_df <- tibble::tibble()
    for (j in sheets_vector) {
      temp_df <- readxl::read_xlsx(participants_file, sheet = j)
      if (isTRUE(i == j) && !any(colnames(temp_df) %in% "category")) {
        temp_df <- temp_df %>%
          dplyr::mutate(category = i)
      }
      # Check for registration in the main event
      if (any(colnames(temp_df) %in% "registration")) {
        temp_df <- temp_df %>%
          dplyr::filter(registration %in% event_data$registration)
      }
      # Check for absence in workshops
      if (any(colnames(temp_df) %in% "attendance")) {
        temp_df <- temp_df %>%
          dplyr::filter(attendance %in% event_data$attendance)
      }
      category_df <- dplyr::bind_rows(category_df, temp_df)
    }
    category_df <- category_df %>%
      dplyr::filter(category %in% i) %>%
      dplyr::distinct()
    categories_list[[i]] <- category_df
  }
  #categories_list$monitor_minicurso %>% View()

  # function to remove all NA cols
  drop_na_cols <- function(x_df) {
    for (i in colnames(x_df)) {
      if (all(is.na(dplyr::pull(x_df, {{ i }})))) {
        x_df <- x_df %>%
          dplyr::select(-{{ i }})
      }
    }
    return(x_df)
  }

  # flatten categories list
  full_df <- tibble::tibble()
  for (i in names(categories_list)) {
    temp_df <- categories_list[[i]] %>%
      dplyr::select(name, email, course_name, category) %>%
      drop_na_cols() %>%
      dplyr::left_join(course_table) %>%
      dplyr::distinct()
    full_df <- dplyr::bind_rows(full_df, temp_df)
  }
  return(full_df)
}

#' Format input table
#' @param event_data event data imported from YAML config file
#' @param input_table A Google Sheets code or XLSX file path containing sheets
#'   organized with sheets for each category of certificate
#' @param course_table `tibble`, `data.frame` or coercible object,
#'   containing course description
#' @param ext Default = "xlsx"; File extension used for input table,
#'   can be replaced to Google Sheets(gs gsheet)
#' @export
import_table <- function(event_data) {
  # file converted from Google Sheets
  participants_file <- event_data$participant_table$path
  ext <- event_data$participant_table$ext

  if (isTRUE(ext == "xlsx")) {
    participant_df <- import_excel(event_data)
  } else if (isTRUE(ext == "gs")) {
    participant_df <- import_gsheets(event_data)
  } else {
    stop("File extension not known.")
  }

  # Validate email through RegExp
  email_regex <- "^[[:alnum:].\\-_]+@[[:alnum:].\\-]+$"
  # Format strings
  participant_table <- participant_df %>%
    # dplyr::filter(`Presença` %in% c("Presente", "Realizado")) %>%
    dplyr::mutate(participant_name = stringr::str_to_title(name)) %>%
    dplyr::mutate(participant_name = stringr::str_squish(participant_name)) %>%
    dplyr::mutate(valid_email = stringr::str_match(email, email_regex)) %>%
    dplyr::mutate(valid_email = as.vector(.$valid_email)) %>%
    dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
    dplyr::mutate(course_name = stringr::str_squish(course_name)) %>%
    dplyr::select(
      course_name, category, valid_email,
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
