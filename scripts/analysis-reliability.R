
cronbachs_alpha <- function(data, cols){
  return (data |>
    select(all_of(cols)) |>
    psych::alpha(discrete=FALSE)
  )
}