# htmlTable, svglite and xfun are used in the Quarto (.qmd) templates, which
# R CMD check doesn't look at. Referencing them here stops it flagging them
# as unused. Never called or exported.
# See R Packages (2e), section 11.4.1.1.
ignore_unused_imports <- function() {
  htmlTable::htmlTable
  svglite::svglite
  xfun::file_ext
}
