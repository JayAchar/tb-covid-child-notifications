library(remotes)

filename <- list.files()[grep(
    pattern = "covidchildtb",
    list.files()
)]

# remotes::install_deps(
#     path = "/usr/src",
#     dependencies = "Imports"
# )

remotes::install_local(
    path = paste0("/usr/src/", filename),
    dependencies = c("Imports"),
    upgrade = "always",
    build_vignettes = FALSE,
    build_manual = FALSE
)