# BSD_2_clause

new_styles <- paste(readLines("./data-raw/new_styles.css"),
                 collapse = "\n")
use_data(new_styles, internal = TRUE, overwrite = TRUE)
