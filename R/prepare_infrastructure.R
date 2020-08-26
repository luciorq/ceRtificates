#' Prepare Infrastructure
#' @export
prepare_infrastructure <- function() {
  if (!isTRUE(fs::dir_exists("certs"))) {
    fs::dir_create("certs")
  }
  if (!isTRUE(fs::dir_exists("temp"))) {
    fs::dir_create("temp")
  }
  fs::file_copy("templates/html/certificate.css", "temp/", overwrite = TRUE)
  fs::dir_copy("templates/html/images/", "temp/images/", overwrite = TRUE)
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
