
#' Function to check for repeated and wrong entries in the table
check_integrity_table <- function(ids_table) {
  # check for certificates with different emails
  # check for hashes associated with different emails
  repeated_id_hashes <- id_table %>%
    dplyr::select(id) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(num = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-num) %>%
    dplyr::filter(num > 1) %>%
    dplyr::pull(id)
  if (isTRUE(length(repeated_id_hashes) > 0)) {
    id_table %>%
      dplyr::filter(id %in% repeated_id_hashes) %>%
      dplyr::distinct() %>%
      dplyr::arrange(email) %>%
      print()
    stop("Hashes not unique. Probably duplicate email entries.")
  }
  # check for emails associated with different names
  repeated_emails <- id_table %>%
    dplyr::select(email, name) %>%
    dplyr::distinct() %>%
    dplyr::group_by(email) %>%
    dplyr::summarise(num = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-num) %>%
    dplyr::filter(num > 1) %>%
    dplyr::pull(email)
  if (isTRUE(length(repeated_emails) > 0)) {
    id_table %>%
      dplyr::filter(email %in% repeated_emails) %>%
      dplyr::distinct() %>%
      dplyr::arrange(email) %>%
      dplyr::select(email, name) %>%
      print()
    stop("Emails are not unique. Probably different name entries for the same email.")
  }
  # check for emails associated with different names
  repeated_names <- id_table %>%
    dplyr::select(name, email) %>%
    dplyr::distinct() %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(num = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-num) %>%
    dplyr::filter(num > 1) %>%
    dplyr::pull(name)
  if (isTRUE(length(repeated_names) > 0)) {
    id_table %>%
      dplyr::filter(name %in% repeated_names) %>%
      dplyr::distinct() %>%
      dplyr::arrange(name) %>%
      dplyr::select(name, email) %>%
      print()
    stop("Names are not unique. Probably different email entries for the same name.")
  }
}
