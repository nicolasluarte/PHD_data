# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    ggtext
)

# publication theme
source("../../../publication_theme/publication_theme.R")
fill <- palette.colors(palette = "Okabe-Ito")
color <- palette.colors(palette = "Okabe-Ito")

# source lickometer library
devtools::source_url("https://github.com/lab-cpl/lickometer-library/blob/main/src/lickometer_functions_compilate.R?raw=TRUE")

# https://stackoverflow.com/questions/47044068/get-the-path-of-current-script
# get path of source file
getCurrentFileLocation <-  function()
{
    this_file <- commandArgs() %>% 
        tibble::enframe(name = NULL) %>%
        tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
        dplyr::filter(key == "--file") %>%
        dplyr::pull(value)
    if (length(this_file)==0)
    {
        this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
}
# sets path on source file location
script_path <- getCurrentFileLocation()
setwd(script_path)

# Create data-sets ----

source("../../../publication_theme/publication_theme.R")
iris %>% 
    ggplot(aes(
        Sepal.Length, Sepal.Width, fill = Species
    )) +
    geom_point(shape = 21, color = "white", size = 3) +
    scale_fill_manual(labels = paste("<span style='color:",
                                     fill,
                                     "'>",
                                     unique(iris$Species),
                                     "</span>"),
                      values = fill) +
    publication_theme() +
    guides(
        fill = guide_legend(override.aes = list(color = NA, fill = NA))
    )

