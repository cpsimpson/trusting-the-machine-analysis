# Create an svg that points at the site containing more info about the project.

library(qrcode)

generate_qr_code <- function(url, output_svg){
  code <- qr_code(url)
  generate_svg(code, filename = output_svg)
}

