#' Read names of the tabs, sheets or workbooks
read_sheets_vector <- function(input_path, ext) {
  if (ext %in% c("xlsx", "xls", "excel")) {
    sheets_vector <- readxl::excel_sheets(input_path)
  } else if (ext %in% c("gs", "gsheets")) {
    sheets_vector <- googlesheets4::sheet_names(input_path)
  } else {
    stop("File extension not known.")
  }
}
#' Import specific tab, sheet or workbook
#' @param sheet tab, sheet or workbook to be imported
#' @inheritParams import_table
import_sheet <- function(input_path, sheet, ext) {
  if (ext %in% c("xlsx", "xls", "excel")) {
    sheet_df <- readxl::read_xlsx(
        input_path, sheet = sheet,
        col_types = "text", trim_ws = TRUE
      )
  } else if (ext %in% c("gs", "gsheets")) {
    sheet_df <- googlesheets4::read_sheet(
      input_path, sheet = sheet,
      col_types = "c", trim_ws = TRUE
    )
  } else {
    stop("File extension not known.")
  }
  return(sheet_df)
}
#' function to remove all NA cols
#' @param x_df a `tibble`
drop_na_cols <- function(x_df) {
  for (i in colnames(x_df)) {
    if (all(is.na(dplyr::pull(x_df, {{ i }})))) {
      x_df <- x_df %>%
        dplyr::select(-{{ i }})
    }
  }
  return(x_df)
}

#' Import and Format input table
#' @param event_data event data imported from YAML config file
#' @param input_path A Google Sheets URL (or code),
#'   or XLSX file path containing sheets
#'   it can contain separate tabs/sheets for each course or category
#' @param ext Default = NULL; File extension used for input table,
#'   can be replaced to Google Sheets(gs, gsheets) or Excel (xls, xlsx)
#' @export
import_table <- function(event_data, input_path = NULL, ext = NULL) {
  if (is.null(input_path)) {
    input_path <- event_data$participant_table$path
  }
  if (is.null(ext)) {
    ext <- event_data$participant_table$ext
  }

  categories_vector <- unique(event_data$cert_type)
  sheets_vector <- read_sheets_vector(input_path, ext)
  # remove lookup tables
  sheets_vector <- sheets_vector[!stringr::str_detect(sheets_vector, "_lookup$")]

  # join category tables
  categories_list <- list()
  # i = categories_vector[1]; j = sheets_vector[1]
  # i = categories_vector[1]; j = sheets_vector[2]
  for (i in categories_vector) {
    category_df <- tibble::tibble()
    for (j in sheets_vector) {
      temp_df <- import_sheet(input_path, sheet = j, ext = ext)
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
  # import course table
  course_table <- import_sheet(input_path, sheet = "course_lookup", ext = ext)
  course_table <- course_table %>%
    dplyr::mutate(edition = event_data$edition)
  # flatten categories list
  full_df <- tibble::tibble()
  # i = names(categories_list)[1]
  for (i in names(categories_list)) {
    temp_df <- categories_list[[i]] %>%
      dplyr::select(name, email, course_name, category) %>%
      drop_na_cols() %>%
      dplyr::left_join(course_table) %>%
      dplyr::distinct()
    full_df <- dplyr::bind_rows(full_df, temp_df)
  }
  # format and validate strings
  full_df <- format_cols(full_df)
  return(full_df)
}

#' Format columns
format_cols <- function(full_df) {
  # Validate email through RegExp
  email_regex <- "^[[:alnum:].\\-_]+@[[:alnum:].\\-]+$"
  # Format strings
  valid_df <- full_df %>%
    dplyr::mutate(participant_name = stringr::str_to_title(name)) %>%
    dplyr::mutate(participant_name = stringr::str_squish(participant_name)) %>%
    dplyr::mutate(valid_email = stringr::str_match(email, email_regex)) %>%
    dplyr::mutate(valid_email = as.vector(.$valid_email)) %>%
    dplyr::mutate(valid_email = stringr::str_to_lower(valid_email)) %>%
    dplyr::mutate(course = stringr::str_squish(course_name)) %>%
    dplyr::select(
      participant_name, valid_email, course, category,
      event_date, cert_hours, edition
    )
  return(valid_df)
  if (FALSE) {
    dplyr::filter(full_df, is.na(course_name))
    dplyr::filter(full_df, is.na(email))
    dplyr::filter(full_df, is.na(category))
    dplyr::filter(full_df, is.na(name))

    dplyr::filter(valid_df, is.na(course))
    dplyr::filter(valid_df, is.na(valid_email))
    dplyr::filter(valid_df, is.na(category))
    dplyr::filter(valid_df, is.na(participant_name))
  }
}


######################################### deprecated ############ to be removed
#' Format input table for missing certs
#' @inheritParams import_table
import_table_missing <- function(input_path2, course_table, ext) {
  sheets_vector <- readxl::excel_sheets(input_path2)
  missing_table <- readxl::read_xlsx(input_path2, sheet = sheets_vector[2])
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
