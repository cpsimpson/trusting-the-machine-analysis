strip_z_prefix <- function(x) {
  sub("^z\\.", "", x)
}

cronbachs_alpha <- function(data, cols){
  return (data |>
            dplyr::select(all_of(strip_z_prefix(cols))) |>
    psych::alpha(discrete=FALSE)
  )
}