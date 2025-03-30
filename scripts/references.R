
write_bib <- function(packages, file = "packages.bib") {
  bibs <- lapply(packages, citation)
  bib_entries <- unlist(lapply(bibs, format, style = "bibtex"))
  writeLines(bib_entries, file)
}

cite_packages_apa <- function(packages) {
  citations <- lapply(packages, function(pkg) {
    cit <- citation(pkg)
    # Use the first citation (some packages have multiple)
    format(cit[[1]], style = "text")
  })
  names(citations) <- packages
  return(citations)
}
