# Run validation pipeline (deactivated in production)
salesforecastr::dh_validation()

# Run training pipeline
salesforecastr::dh_forecasting()

# Deploy the app to shinyapps.io
rsconnect::deployApp(
  appDir = "app",
  appFileManifest = file.path("app", "files.txt"),
  appName = "salesforecastr",
  appTitle = "Sales Forecasting Dashboard",
  account = "sagravela",
  launch.browser = FALSE
)
