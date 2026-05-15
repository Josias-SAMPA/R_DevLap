# 1. Installer les packages nécessaires (une seule fois)
install.packages(c("shiny", "shinyjs"))

# 2. Lancer l'application (dans RStudio ou la console R)
shiny::runApp("app.R")

# 3. Pour accès réseau local (téléphone sur le même Wi-Fi)
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
# → Ouvrir http://<IP_de_votre_PC>:3838 sur le téléphone

# 4. Déploiement public gratuit sur shinyapps.io
#    install.packages("rsconnect")
#    rsconnect::deployApp(".")
