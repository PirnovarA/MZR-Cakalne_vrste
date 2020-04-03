
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    source("../lib/libraries.R")
    source("../main/generator.R", chdir = T)
    source("server_funkcije.R", chdir = T)
    source("../graphics/risanje_vrste.R", chdir = T)
    # Placeholder za reactive vrednosti ####
    
    # Uvodni tekst ####
    uvodniTekst <- paste(readLines("besedilo/Opis.txt"), collapse=" ")
    output$text_uvodniTekst <- renderUI({
        fluidRow(column(1), 
                 column(10, p(strong(uvodniTekst)), br()
                        )
                 )
    })
    
    # inputUI maxPrihodi #####
    output$inputUI_maxPrihodi <- renderUI({
        if (input$checkbox_maxPrihodi) {
            numericInput(
                "input_maxPrihodi",
                label = "Stevilo prihodov strank:",
                value = 100,
                min = 1
            )
        } else {
            numericInput(
                "input_maxCakanje",
                label = "Cas delovanja streznikov:",
                value = 8,
                min = 0.001
            )
        }
    })
    # inputUI nestrpni ####
    output$inputUI_imp <- renderUI({
        if (input$checkbox_imp) {
            list(
            numericInput(
                inputId = "input_impDelez",
                label = "Delez nestrpnih ljudi:",
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.05
            ),
            numericInput(
                inputId = "input_impCas",
                label = "Cas cakanja nepotrpezljivih oseb pred odhodom:",
                value = 10,
                min = 0,
                step = 0.2
                )
            )
        }
    })
    # inputUI VIP ####
    output$inputUI_vipImp <- renderUI({
        list(
            if (input$checkbox_vip & input$checkbox_imp) {
                checkboxInput(
                    inputId = "checkbox_vipImp",
                    label = "Nestrpni osebe naj bodo VIP (imajo prednost)!",
                    value = FALSE
                )
            }
        )
    })
    output$inputUI_vipDelez <- renderUI({
        inputTemp <- numericInput(
            inputId = "input_vipDelez",
            label = "Koliksen delez oseb ima prednost (je VIP):",
            value = 0.2,
            min = 0,
            max = 1,
            step = 0.05
        )
        if (input$checkbox_vip) {
            if (!input$checkbox_imp) {
                inputTemp
            } else if (!input$checkbox_vipImp) {
                inputTemp
            }
            
        }
        
    })
    # inputUI nacin porazdelitve strezb ####
    output$inputUI_radio_porazdStr <- renderUI({
        radioButtons(
            inputId = "radio_porazdStr",
            label = "Nacin dolocitve porazdelitve
                                           streznih casov:",
            choices = list(`Vsi enako porazdeljeni` = "enako",
                           `Preddolocimo streznike strankam` = "skupine",
                           `Strezniki imajo razlicne porazdelitve` = "razlicni"),
            selected = "enako",
            inline = T
        )
    })
    # inputUI enako ####
    output$inputUI_enakoDist <- renderUI({
        if (input$radio_porazdStr == "enako") {
            list(
                selectInput(
                    inputId = "select_enakoDist",
                    label = "Porazdelitve, uporabljene za čase streženja:",
                    choices = list(`Eksponentna porazdelitev` = "exp"),
                    selected = "exp"
                )
            )
        }
    })
    output$inputUI_enakoParam <- renderUI({
        if (input$radio_porazdStr == "enako") {
            if (input$select_enakoDist == "exp") {
                numericInput(
                    inputId = "input_enakoMu",
                    label = "Intenziteta eksponentne porazdelitve strezenja (mu):",
                    min = 0.001,
                    value = 10
                )
            }
        }
    })
    # inputUI skupine ####
    output$inputUI_skupineDist <- renderUI({
            if (input$radio_porazdStr == "skupine") {
                selectInput(
                    inputId = "select_skupineDist",
                    label = "Porazdelitve, uporabljene za čase streženja:",
                    choices = list(`Eksponentna porazdelitev` = "exp"),
                    selected = "exp"
                )
            }
    })
    output$inputUI_skupineParam <- renderUI({
        if (input$radio_porazdStr == "skupine") {
            list(p(em("Vnesemo toliko parametrov, kolikor je streznikov. Loceni
            naj bodo s ';'. Vsako zaporedno mesto predstavlja en streznik.")),
            if (input$select_skupineDist == "exp") {
                    textInput(
                        inputId = "input_skupineMu",
                        label = "Intenzitete eksponentne porazdelitve strezenja
                              (mu):",
                        placeholder = "10;5;3;...",
                        value = paste(rep(10, input$input_k), collapse = ";")
                    )
            })
        }
    })
    # inputUI razlicne porazdelitve streznikov ####
    output$inputUI_stRazlicniStr <- renderUI({
        if (input$radio_porazdStr == "razlicni") {
            list(
            selectInput(
                inputId = "select_stRazlicnihStr",
                label = "Stevilo razlicnih porazdelitev streznikov:",
                choices = 1:input$input_k,
                selected = input$input_k
            ),
            selectInput(
                inputId = "select_razlicniDistIsti",
                label = "Porazdelitve, uporabljene za čase streženja:",
                choices = list(`Eksponentna porazdelitev` = "exp"),
                selected = "exp",
            )
            )
        }
    })
    output$inputUI_razlicniParam <- renderUI({
        if (input$radio_porazdStr == "razlicni") {
            list(p(em("Vnesemo toliko parametrov, kolikor je skupin streznikov. 
            Loceni naj bodo s ';'. Vsako zaporedno mesto predstavlja en 
                      streznik.")),
            if (input$select_razlicniDistIsti == "exp") {
                textInput(
                    inputId = "input_razlicniMu",
                    label = "Intenzitete eksponentne porazdelitve strezenja
                              (mu):",
                    placeholder = "10;5;3;...",
                    value = paste(rep(10, input$select_stRazlicnihStr), 
                                  collapse = ";")
                )
            })
        }
    })
    output$inputUI_razlicniSkupStr <- renderUI({
        if (input$radio_porazdStr == "razlicni") {
            list(p(strong("Kateri skupini pripadajo strezniki:")),
            "Vnese se zaporedje stevilk od 1 do stevila skupin streznikov,
            locenih s ';'. Vsako mesto predstavlja streznik, stevilka pa kateri
            skupini ta streznik pripada.",
                textInput(
                    inputId = "input_dolocitevSkupinStr",
                    label = NULL,
                    placeholder = "1;2;3;...",
                    value = paste(rep(1:input$select_stRazlicnihStr, 
                                      length.out = input$input_k),
                                  collapse = ";")
                )
            )
        }
    })
    
    
    # Delovanje aplikacije ####
    # Osivljenost Simuliraj gumba ####
    observeEvent(c(input$input_k, input$input_n, input$input_lambda,
                   input$input_maxCakanje, input$input_maxPrihodi,
                   input$checkbox_maxPrihodi, input$checkbox_imp,
                   input$input_impDelez, input$input_impCas,
                   input$checkbox_vip, input$checkbox_vipImp,
                   input$input_vipDelez), {
            bool1 <- if(input$checkbox_maxPrihodi) {
                !is.na(input$input_maxPrihodi)
            } else {
                !is.na(input$input_maxCakanje)
            }
            bool2 <- if(input$checkbox_imp) {
                all(!is.na(c(input$input_impDelez, input$input_impCas)))
            } else {TRUE}
            bool3 <- if(input$checkbox_vip) {
                if(!input$checkbox_imp) {
                    !is.na(input$input_vipDelez)
                } else {
                    if (is.null(input$checkbox_vipImp)) {
                        !is.na(input$input_vipDelez)
                    } else if (!input$checkbox_vipImp) {
                        !is.na(input$input_vipDelez)
                    } else {TRUE}
                }
            }
            shinyjs::toggleState("btn_zazeniVrsto",{
            all(!is.na(c(input$input_k, input$input_n, input$input_lambda)), 
                bool1, bool2, bool3)
        })
                             
    })
    
    # Zagon simulacije ####
    observeEvent(
        input$btn_zazeniVrsto,
        {
            rezultati <- preberi.inpute(
                k = input$input_k,
                n = input$input_n,
                lambda = input$input_lambda,
                checkbox_maxPrihodi = input$checkbox_maxPrihodi,
                maxPrihodi = input$input_maxPrihodi,
                maxCakanje = input$input_maxCakanje,
                checkbox_imp = input$checkbox_imp,
                impDelez = input$input_impDelez,
                impCas = input$input_impCas,
                checkbox_vip = input$checkbox_vip,
                checkbox_vipImp = input$checkbox_vipImp,
                vipDelez = input$input_vipDelez,
                radio_porazdStr = input$radio_porazdStr,
                enakoDist = input$select_enakoDist,
                enakoMu = input$input_enakoMu,
                skupineDist = input$select_skupineDist,
                skupineMu = input$input_skupineMu,
                stRazlicnihStr = input$select_stRazlicnihStr,
                razlicniDistIsti = input$select_razlicniDistIsti,
                razlicniMu = input$input_razlicniMu,
                dolocitevSkupinStr = input$input_dolocitevSkupinStr
            )
            print(rezultati$status)
            output$react_simulirano <- eventReactive(input$btn_zazeniVrsto,
                                                     toString(rezultati$status))
            outputOptions(output, "react_simulirano", suspendWhenHidden=FALSE)
            
            # Pop-up sporocila glede na inpute
            if (rezultati$status == 2) {
                showModal(modalDialog(
                    title = "Problematicni input!",
                    "Stevilo streznikov in intenziteta prihodov morata
                    biti vecja od 0, velikost cakalnice pa 0 ali vec. 
                    Stevilo cakalnih mest in stevilo streznikov morata
                    biti celi stevili!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }else if (rezultati$status == 3) {
                showModal(modalDialog(
                    title = "Problematicni input!",
                    "Maksimalno stevilo prihodov ali maksimalen cas prihoda 
                    morata biti vecja od 0. Stevilo prihodov mora biti
                    celo stevilo!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }else if (rezultati$status == 4) {
                showModal(modalDialog(
                    title = "Delez nepotrpezljivih mora biti med 0 in 1!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }else if (rezultati$status == 5) {
                showModal(modalDialog(
                    title = "Problematicni input!",
                    "Delez VIP mora biti med 0 in 1!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }else if (rezultati$status == 6) {
                showModal(modalDialog(
                    title = "Problematicni input!",
                    "Parametri porazdelitev strezb so napacni!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }else if (rezultati$status == 7) {
                showModal(modalDialog(
                    title = "Problematicni input!",
                    "Napaka pri podajanju skupin ali streznikov!",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }
            
            
            # # TODO sporocila glede na status!
            # # grafi glede na output, ce status 1
            # 
            output$plotUI_vhodni <- renderPlotly({
                # Na vhodni strani
                if (rezultati$status == 1) {
                    #narisi.vrsto.cum(rezultati$zaGraf)
                    narisi.vrsto.cum.plotly(rezultati$zaGraf)
                } else {
                    NULL
                }
            })
            
    })

})
