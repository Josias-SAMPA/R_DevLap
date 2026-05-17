FROM rocker/shiny:latest

# Installer shinyjs (seul package non-base nécessaire)
RUN R -e "install.packages('shinyjs', repos='https://cran.rstudio.com/')"

# Copier l'app
COPY app.R /srv/shiny-server/app/app.R

# Lancer sur le port Railway
CMD R -e "shiny::runApp('/srv/shiny-server/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"