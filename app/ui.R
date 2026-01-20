source('global.R')

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico')
    ),
    titlePanel('Skapa GIS-fil med punktlager'),
    sidebarLayout(
      sidebarPanel(
        h4('Exempelsida'),
        p('Byt ut detta innehåll mot din riktiga UI.')
      ),
      mainPanel(
        tabsetPanel(
          tabPanel('Tab 1', h3('Hej från karta_skapa_punktlager')),
          tabPanel('Om', p('Beskriv applikationen här.'))
        ),
        hr(),
        verbatimTextOutput('example_text')
      )
    )
  )
)
