
# options(repos = c(CRAN = "https://cran.rstudio.com/"))
# utils::install.packages(c("cpp11", "dplyr", "magrittr", "rlang", "rmarkdown", "sourcetools", "tinytex"))


{require("rsconnect")
  source("https://raw.githubusercontent.com/agdamsbo/project.aid/refs/heads/main/R/deploy_shiny.R")

  renv::snapshot()

  # shinyapps_ready()
  # shiny::runApp(appDir = here::here("app_dev/app.R"))

  deploy_shiny(
    path = here::here(""),
    # path=NULL,
    # files = c(list.files(here::here("inst/apps/FreesearchR/www"),full.names = TRUE),
    #           list.files(here::here("inst/apps/FreesearchR/img"),full.names = TRUE),
    #           here::here("inst/apps/FreesearchR/server.R"),here::here("inst/apps/FreesearchR/ui.R")),
    files = "app.R",
    account.name = "agdamsbo",
    name.app = "Schedule",
    name.token = "rsconnect_agdamsbo_token",
    name.secret = "rsconnect_agdamsbo_secret"
  )
}



## Shinylive gets too big. For this to work, dependencies should be reduced..
# shinylive::export(appdir = here::here(""), destdir = "docs")
# #
# httpuv::runStaticServer(dir = "docs")
