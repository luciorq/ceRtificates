#' Generate qrcode to check certificates
#' @param identifier hash used to lookup
#' @param base_url URL to be added to the qrcode link
#' @param edition Edition of the event
#' @export
generate_qrcode <- function(identifier, base_url, edition) {
  # create shorter id
  #identifier = ids_table$id_short[i]; base_url = base_url; edition = ids_table$edition[i];
  qrcode_string <- as.character(glue::glue("{base_url}{edition}/{identifier}"))
  margin_value <- 2
  # square_side <- 7
  qrcode_string %>%
    qrencoder::qrencode_svg(level = 0, dpi = 1200, size = 300, margin = margin_value) %>%
    readr::write_lines(path = fs::path("temp", "qrcode", glue::glue("{identifier}.svg")))
  qrcode_svg <- readr::read_lines(fs::path("temp", "qrcode", glue::glue("{identifier}.svg")))

  qrcode_svg_done <- qrcode_svg
  # pattern_1 <- glue::glue("<rect x=\"{margin_value}\" y=\"{margin_value}\" width=\"7\" height=\"1\" fill=\"#000000\" />")
  # qrcode_svg_done[stringr::str_which(qrcode_svg, pattern_1)] <- qrcode_svg[stringr::str_which(qrcode_svg, pattern_1)] %>%
  #   stringr::str_replace("width=\"7\"", "width=\"3\"") %>%
  #   stringr::str_replace("x=\"2\"", "x=\"6\"")
  #
  # qrcode_svg[7] %>%
  #   stringr::str_replace("<rect.*/>", "<line x1=\"10\" x2=\"50\" y1=\"110\" y2=\"150\" stroke=\"orange\" stroke-width=\"5\"/>")
  # #----
  # header_lines <- qrcode_svg[1:4]
  #
  # qrcode_svg[7]

  qrcode_svg_done %>%
    readr::write_lines(path = fs::path("temp", "qrcode", glue::glue("{identifier}.svg")))
}

