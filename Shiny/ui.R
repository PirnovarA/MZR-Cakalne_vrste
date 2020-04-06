library(dplyr)
library(tidyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(plotly)

#### Vhodni podatki panel ####
panelVhodniPodatki <- tabPanel(
    "Vhodni podatki",
    uiOutput("text_uvodniTekst"),
    sidebarLayout(
        sidebarPanel(
            p(strong("Nalozi shranjeno datoteko vrste")),
            p(em("Nalozi se lahko datoteko RMD, ki je output te aplikacije.
                 Druga moznost je, da se vrsto zgenerira na novo s spodnjimi
                 parametri!")),
            fileInput(inputId = "upload_vrsta", label = NULL,
                      accept = c(".RMD", ".rmd")),
            disabled(actionButton(
                inputId = "btn_nalozi",
                label = "Nalozi"
            )),
            tags$hr(),
            numericInput(
                inputId = "input_k",
                value = 4,
                label = "Stevilo streznikov:",
                min = 0,
                step = 1
            ),
            numericInput(
                inputId = "input_n",
                value = 10,
                label = "Stevilo cakalnih mest:",
                min = 0,
                step = 1
            ),
            numericInput(
                inputId = "input_lambda",
                label = "Intenziteta prihodov
                                           (lambda poissonovega procesa):",
                value = 10,
                min = 0.001,
                step = 1
            ),
            checkboxInput(
                inputId = "checkbox_maxPrihodi",
                label = "Omeji stevilo prihodov s
                                           stevilom prihodov",
                value = FALSE
            ),
            uiOutput("inputUI_maxPrihodi"),
            checkboxInput(
                inputId = "checkbox_imp",
                label = "Nestrpni ljudje.",
                value = FALSE
            ),
            uiOutput("inputUI_imp"),
            checkboxInput(
                inputId = "checkbox_vip",
                label = "Prednostne osebe.",
                value = FALSE
            ),
            uiOutput("inputUI_vipImp"),
            uiOutput("inputUI_vipDelez"),
            uiOutput("inputUI_radio_porazdStr"),
            uiOutput("inputUI_enakoDist"),
            uiOutput("inputUI_enakoParam"),
            uiOutput("inputUI_skupineDist"),
            uiOutput("inputUI_skupineParam"),
            uiOutput("inputUI_checkbox_razlicniStr"),
            uiOutput("inputUI_stRazlicniStr"),
            uiOutput("inputUI_porazdRazlicniStrIsti"),
            uiOutput("inputUI_razlicniParam"),
            uiOutput("inputUI_razlicniSkupStr"),
            actionButton(inputId = "btn_zazeniVrsto",
                         label = "Simuliraj")
        ),
        mainPanel(fluidRow(column(1),
            # Graf na panelu vhodnih podatkov
            column(10,
                   plotlyOutput("plotUI_vhodni")
            )
            ),
            fluidRow(column(2),
                     column(10, uiOutput("statUI_vhodni")
                )
            ),
            uiOutput("btnUI_shraniVrsto")
        )
        
    )
)

ui <-
    navbarPage(useShinyjs(),
        theme = shinytheme("cyborg"),
        title = "Cakalne vrste",
        shiny::includeCSS("www/styles.css"),
        selected = "Vhodni podatki",
        panelVhodniPodatki,
        #### Statistika zgenerirane vrste panel ####
        tabPanel("Statistika zgenerirane vrste"),
        #### Simulacija poteka zgenerirane vrste panel ####
        tabPanel("Simulacija poteka zgenerirane vrste")
    )
