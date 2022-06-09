site <- commandArgs(trailingOnly=TRUE)[1]
account <- "covid19mprdashboard"

print(getOption("repos"))

files <- list.files(all.files = TRUE)
deploy_files <- files[!files %in% c(".","..",".git",".gitignore",".Rhistory",".Rprofile",".Rdata",".Rproj.user","renv","renv.lock","rockefeller.Rproj","create_df_dashboard.R","deploy.R","HISTORY.html","HISTORY.md",
                                    "README.html","README.md","release.sh")]
print(deploy_files)

if(site == "prod"){
  appTitle <- "Rockefeller_COVID_PRPH"
} else {
  appTitle <- "INTERNAL_TESTING_Rockefeller_COVID_PRPH"
}
print(appTitle)

print(paste0("Deploying app to ", account, ".shinyapps.io/", appTitle))
rsconnect::deployApp(appFiles = deploy_files, account = account, appTitle = appTitle)