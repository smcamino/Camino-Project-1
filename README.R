# Code to knit .Rmd file
rmarkdown::render("README.Rmd",
                  output_format = "github_document",
                  output_options = list(html_preview = FALSE))
