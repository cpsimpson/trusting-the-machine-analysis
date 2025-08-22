
cronbachs_alpha <- function(data, cols){
  return (data |>
            dplyr::select(all_of(cols)) |>
    psych::alpha(discrete=FALSE)
  )
}