# before editing:
# file.copy("vignettes/workflow.Rmd.orig", "vignettes/workflow.Rmd", overwrite = T)
# before knitting:
# file.copy("vignettes/workflow.Rmd", "vignettes/workflow.Rmd.orig", overwrite = T)

knitr::knit("vignettes/workflow.Rmd.orig", "vignettes/workflow.Rmd")

images <- list.files(pattern = ".png")
file.copy(images, "vignettes", overwrite = T)
file.remove(images)
