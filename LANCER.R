# Installer les packages nécessaires
install.packages(c("shiny", "shinyjs"))

# Lancer l'application  dans la consolle de R studio
shiny::runApp("app.R")

# Pour accès réseau local 
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
# Ouvrir http://<IP_de_votre_PC>:3838 sur le téléphone
