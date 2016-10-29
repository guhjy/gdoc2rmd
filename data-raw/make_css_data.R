# BSD_2_clause

##############################################################################
# I thought originally that I might read these CSS files directly, but after
# more development have decided to keep in inst/extdata and use `system.file`
# to retrieve and use.
##############################################################################

# new_styles <- paste(readLines("./inst/extdata/new_styles.css"),
#                  collapse = "\n")

# bootstrap <- paste(readLines("./inst/extdata/bootstrap/css/bootstrap.css"),
#                    collapse = "\n")

# use_data(new_styles, bootstrap, internal = TRUE, overwrite = TRUE)
