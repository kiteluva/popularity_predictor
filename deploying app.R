library(rsconnect)

# Set account info (only needs to be done once per machine)
rsconnect::setAccountInfo(name='jsf0vd-james-kitenye',
                          token='7EB408BDF861115ADC84AFB00F33A253',
                          secret='O8KbJRwE9U69/aLd8hgh0uhxI+nkejXib3AzdYh3')

# Deploy your Traditional Shiny App
# No need for appPrimaryDoc or appMode when using app.R
rsconnect::deployApp(
  appDir = "~/DATA ANALYSIS & VISUALIZATION/VISUAL CODE/popularity_predictor",
  appName = "Popularity_Predictor" 
)

