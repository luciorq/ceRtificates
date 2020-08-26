#' Generate unique hash identifier from text
#' @param id_string text string to be used for hashing
#' @param type default = "long", sha256; "short";
#' @export
generate_hash <- function(id_string, type = "long") {
  concat_id <- base::paste(id_string, collapse = "\n")
  hash_id <- openssl::sha256(concat_id)
  # hash_raw <- openssl::sha256(base::charToRaw(concat_id))
  # luciolib::obj_size(hash_id) # 520 B
  # luciolib::obj_size(hash_raw) # 368 B
  if (type == "long") {
    return(hash_id)
  } else {
    return(jsonlite::base64_enc(hash_id))
  }
}
