#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(plotly)
library(DT)

d <- read_csv("arsreikningagogn.csv") 

ui <- navbarPage("Ársreikningar sveitarfélaga",
                 
                 tabPanel(title = "Þróun",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  selectInput(
                                      inputId = "sveitarfelag",
                                      label = "Sveitarfélag",
                                      choices = unique(d$sveitarfelag),
                                      selected = c("Reykjavíkurborg", "Kópavogsbær", "Hafnarfjarðarkaupstaður",
                                                   "Garðabær", "Mosfellsbær", "Seltjarnarnesbær"),
                                      multiple = TRUE,
                                      selectize = TRUE
                                  ),
                                  selectInput(
                                      inputId = "hluti",
                                      label = "Hluti",
                                      choices = c("A-hluti", "A og B-hluti"),
                                      selected = c("A-hluti")
                                  ),
                                  selectInput(
                                      inputId = "y_var",
                                      label = "Myndrit",
                                      choices = c(
                                          "Eiginfjárhlutfall",
                                          "Framlegð sem hlutfall af tekjum",
                                          "Handbært fé per íbúi",
                                          "Jöfnunarsjóðsframlög per íbúi",
                                          "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum",
                                          "Nettó jöfnunarsjóðsframlög per íbúi",
                                          "Rekstrarniðurstaða sem hlutfall af tekjum",
                                          "Skuldir per íbúi", 
                                          "Skuldir sem hlutfall af tekjum",
                                          "Skuldaaukning",
                                          "Skuldahlutfall",
                                          "Útsvar og fasteignaskattur per íbúi",
                                          "Veltufé frá rekstri sem hlutfall af tekjum",
                                          "Veltufjárhlutfall"
                                      ),
                                      selected = c("Skuldahlutfall")
                                  ),
                                  selectInput(
                                      inputId = "ar_fra", 
                                      label = "Frá hvaða ári viltu sjá þróunina?", 
                                      choices = c(2002, 2006, 2010, 2014, 2018), 
                                      selected = 2002, 
                                      multiple = FALSE, 
                                      selectize = FALSE
                                  ),
                                  selectInput(
                                      inputId = "verdlag",
                                      label = "Verðlag",
                                      choices = c("Verðlag hvers árs", "Verðlag 2021"),
                                      selected = "Verðlag 2021"
                                  ),
                                  div(
                                      actionButton(
                                          inputId = "goButton",
                                          label = "Sækja gögn",
                                          width = "120px"
                                      ),
                                      class = "center", align = "middle"
                                  ), 
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur:"),
                                  p("Brynjólfur Gauti Guðrúnar Jónsson"),
                                  HTML("<a href='https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/'> Kóði og gögn </a>")
                              ),
                              
                              mainPanel(
                                  tabsetPanel(
                                      type = "tabs",
                                      tabPanel("Myndrit", plotOutput("throun_plot", height = 500)),
                                      tabPanel("Tafla", DTOutput("throun_tafla"))
                                  )
                                  
                              )
                          )
                 ),
                 
                 tabPanel("Dreifing",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  selectInput(
                                      inputId = "vidmid",
                                      label = "Sveitarfélag til viðmiðunar",
                                      choices = unique(d$sveitarfelag),
                                      selected = c("Reykjavíkurborg"),
                                      multiple = FALSE,
                                      selectize = FALSE
                                  ),
                                  selectInput(
                                      inputId = "hluti_distribution",
                                      label = "Hluti",
                                      choices = c("A-hluti", "A og B-hluti"),
                                      selected = c("A-hluti")
                                  ),
                                  
                                  selectInput(
                                      inputId = "y_var_distribution",
                                      label = "Myndrit",
                                      choices = c(
                                          "Eiginfjárhlutfall",
                                          "Handbært fé per íbúi",
                                          "Jöfnunarsjóðsframlög per íbúi",
                                          "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum",
                                          "Nettó jöfnunarsjóðsframlög per íbúi",
                                          "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)",
                                          "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)",
                                          "Skuldir per íbúi", 
                                          "Skuldir sem hlutfall af tekjum",
                                          "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)",
                                          "Skuldahlutfall",
                                          "Útsvar og fasteignaskattur per íbúi",
                                          "Veltufé frá rekstri sem hlutfall af tekjum",
                                          "Veltufjárhlutfall"
                                      ),
                                      selected = c("Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)")
                                  ),
                                  div(
                                      actionButton(
                                          inputId = "goButton_distribution",
                                          label = "Sækja gögn",
                                          width = "120px"
                                      ),
                                      class = "center", align = "middle"
                                  ),
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur"),
                                  p("Brynjólfur Gauti Guðrúnar Jónsson"),
                                  HTML("<a href='https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/'> Kóði og gögn </a>")
                                  
                              ),
                              
                              
                              mainPanel(
                                  h3("Tölur miða við síðasta aðgengilega ársreikning sveitarfélags"),
                                  br(" "),
                                  tabsetPanel(
                                      tabPanel("Myndrit", plotOutput("dreifing_plot", height = 1000, width = "100%")),
                                      tabPanel("Tafla", DTOutput("dreifing_tafla"))
                                  )
                              )
                          )
                          
                 ),
                 tabPanel("Viðmið",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  selectInput(
                                      inputId = "sveitarfelag_vidmid",
                                      label = "Sveitarfélag",
                                      choices = unique(d$sveitarfelag),
                                      selected = c("Reykjavíkurborg", "Kópavogsbær", "Hafnarfjarðarkaupstaður",
                                                   "Garðabær", "Mosfellsbær", "Seltjarnarnesbær"),
                                      multiple = TRUE,
                                      selectize = TRUE
                                  ),
                                  selectInput(
                                      inputId = "hluti_vidmid",
                                      label = "Hluti",
                                      choices = c("A-hluti", "A og B-hluti"),
                                      selected = c("A-hluti")
                                  ),
                                  div(
                                      actionButton(
                                          inputId = "goButton_vidmid",
                                          label = "Sækja gögn",
                                          width = "120px"
                                      ),
                                      class = "center", align = "middle"
                                  ),
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur"),
                                  p("Brynjólfur Gauti Guðrúnar Jónsson"),
                                  HTML("<a href='https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/'> Kóði og gögn </a>")
                                  
                              ),
                              
                              
                              mainPanel(
                                  plotOutput("plot_vidmid", height = 800, width = "100%")
                              )
                          )
                          
                 )
                 
)

server <- function(input, output) {
    
    #### Þróun ####
    
    throun_df <- eventReactive(input$goButton, {
        
        
        y_vars <- list(
            "Eiginfjárhlutfall" = "eiginfjarhlutfall",
            "Framlegð sem hlutfall af tekjum" = "framlegd_hlutf",
            "Handbært fé per íbúi" = "handbaert_fe_per_ibui",
            "Jöfnunarsjóðsframlög per íbúi" = "jofnunarsjodur_a_ibua",
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = "hlutf_jofnunarsjods_skottum",
            "Nettó jöfnunarsjóðsframlög per íbúi" = "netto_jofnunarsjod_per_ibui",
            "Rekstrarniðurstaða sem hlutfall af tekjum" = "rekstrarnidurstada_hlutf",
            "Skuldir per íbúi"  = "skuldir_per_ibui",
            "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
            "Skuldaaukning" = "skuldaaukning",
            "Skuldahlutfall" = "skuldahlutfall",
            "Útsvar og fasteignaskattur per íbúi" = "skattur_a_ibua",
            "Veltufé frá rekstri sem hlutfall af tekjum" = "veltufe_hlutf_tekjur",
            "Veltufjárhlutfall" = "veltufjarhlutfall"
        )
        
        my_functions <- list(
            "Skuldir per íbúi" = ifelse(input$verdlag == "Verðlag hvers árs", 
                                        function(data) data,  
                                        function(data) data |> mutate(y = y / visitala_2021)),
            "Skuldaaukning" = ifelse(input$verdlag == "Verðlag hvers árs",
                                     function(data) data |> group_by(sveitarfelag) |> mutate(y = (y + 1) / (y[ar == input$ar_fra] + 1) - 1) |> ungroup(),
                                     function(data) data |> group_by(sveitarfelag) |> mutate(y = ((y + 1) / visitala_2021) / ((y[ar == input$ar_fra] + 1) / visitala_2021[ar == input$ar_fra]) - 1) |> ungroup()),
            "Handbært fé per íbúi" = ifelse(input$verdlag == "Verðlag hvers árs", 
                                            function(data) data,  
                                            function(data) data |> mutate(y = y / visitala_2021)),
            "Nettó jöfnunarsjóðsframlög per íbúi" = ifelse(input$verdlag == "Verðlag hvers árs", 
                                                           function(data) data,  
                                                           function(data) data |> mutate(y = y / visitala_2021)),
            "Útsvar og fasteignaskattur per íbúi" = ifelse(input$verdlag == "Verðlag hvers árs", 
                                                           function(data) data,  
                                                           function(data) data |> mutate(y = y / visitala_2021))
        )
        
        y_var <- y_vars[[input$y_var]]
        
        
        
        
        
        plot_dat <- d |> 
            filter(hluti %in% input$hluti, sveitarfelag %in% input$sveitarfelag, ar >= input$ar_fra) |> 
            select(ar, sveitarfelag, y = all_of(y_var), visitala_2021) |> 
            mutate(x = ar)
        
        
        if (!is.null(my_functions[[input$y_var]])) {
            plot_dat <- plot_dat |> 
                my_functions[[input$y_var]]()
        }
        
        plot_dat
        
    }) 
    
    throun_plot <- eventReactive(input$goButton, {
        
        
        
        
        y_scales <- list(
            "Eiginfjárhlutfall" = scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.25), expand = expansion()),
            "Framlegð sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Handbært fé per íbúi" = scale_y_continuous(label = label_number(suffix = " kr"), 
                                                        expand = expansion(mult = 0.005),
                                                        breaks = c(0, 1e1, 1e2, 1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6, 3e6),
                                                        limits = c(NA, NA),
                                                        trans = pseudo_log_trans()),
            "Jöfnunarsjóðsframlög per íbúi" = scale_y_continuous(label = label_number(suffix = " kr"), limits = c(0, NA), expand = expansion()),
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = scale_y_continuous(labels = label_percent(), limits = c(0, NA), expand = expansion()),
            "Nettó jöfnunarsjóðsframlög per íbúi" = scale_y_continuous(label = label_number(suffix = " kr")),
            "Rekstrarniðurstaða sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldir per íbúi" = scale_y_continuous(label = label_number(suffix = " kr"), expand = expansion()),
            "Skuldir sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldaaukning" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldahlutfall" = scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.25), expand = expansion()),
            "Útsvar og fasteignaskattur per íbúi" =  scale_y_continuous(label = label_number(suffix = " kr"), limits = c(0, NA), expand = expansion()),
            "Veltufé frá rekstri sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), breaks = pretty_breaks(6),
                                                                              limits= c(0, NA), expand = expansion()),
            "Veltufjárhlutfall" = scale_y_continuous(labels = label_percent(), expand = expansion())
        )
        
        x_scales_year <- list(
            "2002" = scale_x_continuous(breaks = c(2002, 2005, 2008, 2010, 2015, 2021)),
            "2006" = scale_x_continuous(breaks = c(2002, 2006, 2008, 2010, 2015, 2021)),
            "2010" = scale_x_continuous(breaks = c(2010:2021)),
            "2014" = scale_x_continuous(breaks = c(2014:2021)),
            "2018" = scale_x_continuous(breaks = c(2018:2021))
        )
        
        x_scales <- list(
            "Eiginfjárhlutfall" = x_scales_year[[as.character(input$ar_fra)]],
            "Framlegð sem hlutfall af tekjum" = x_scales_year[[as.character(input$ar_fra)]],
            "Rekstrarniðurstaða sem hlutfall af tekjum" = x_scales_year[[as.character(input$ar_fra)]],
            "Skuldir per íbúi" = x_scales_year[[as.character(input$ar_fra)]],
            "Skuldir sem hlutfall af tekjum" = x_scales_year[[as.character(input$ar_fra)]],
            "Skuldaaukning" = scale_x_continuous(breaks = 2002:2021, limits = c(NA, NA)),
            "Skuldahlutfall" = x_scales_year[[as.character(input$ar_fra)]],
            "Veltufjárhlutfall" = x_scales_year[[as.character(input$ar_fra)]]
            
        )
        
        
        
        
        subtitles <- list(
            "Eiginfjárhlutfall" = "Eiginfjárhlutfall (100% - skuldahlutfall) sýnir hlutfall eigna sem er fjármagnað með hagnaði og hlutafé (restin eru skuldasöfnun).\nHér þýðir 100% að skuldir séu engar og 0% að eigin eignir eru engar.",
            "Framlegð sem hlutfall af tekjum" = "Framlegð er reglulegar tekjur mínus gjöld að frádregnum rekstrargjöldum",
            "Handbært fé per íbúi" = "Handbært fé er það fé sem sveitarfélög eiga eftir þegar búið er að greiða skuldir og skuldbindingar.\nAthugið að myndin er á lograkvarða (fjarlægð milli 1.000, 10.000 og 100.000 er sú sama)",
            "Jöfnunarsjóðsframlög per íbúi" = "Peningamagn sem sveitarfélag fær frá jöfnunarsjóð deilt með íbúafjölda.",
            "Nettó jöfnunarsjóðsframlög per íbúi" = "Framlög frá jöfnunarsjóði að frádregnum útgjöldum til jöfnunarsjóðs deilt með íbúafjölda sveitarfélags",
            "Skuldahlutfall" = "Skuldahlutfall sýnir hve stór hluti heildareigna er fjármagnaður með lánum.\nHér þýðir 0% að skuldir séu engar og 100% að eigin eignir eru engar.\nOft er sama orðið notað fyrir skuldir sveitarfélaga sem hlutfall af tekjum, en það á ekki við hér.",
            "Veltufjárhlutfall" = "Veltufjárhlutfall er hlutfall skammtímaskulda deilt upp í eignir sem er hægt að nota í að borga í skammtímaskuldir"
        )
        
        hlines <- list(
            "Eiginfjárhlutfall" = c(0, 1),
            "Framlegð sem hlutfall af tekjum" = 0,
            "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum" = 0,
            "Skuldir per íbúi" = NULL,
            "Skuldir sem hlutfall af tekjum" = 1,
            "Skuldaaukning" = 0,
            "Skuldahlutfall" = c(0, 1),
            "Veltufé frá rekstri sem hlutfall af tekjum" = 0,
            "Veltufjárhlutfall" = 1
        )
        
        
        plot_dat <- throun_df()
        
        
        
        coords <- list(
            "Eiginfjárhlutfall" = coord_cartesian(ylim = c(pmin(0, min(plot_dat$y)), pmax(1, max(plot_dat$y)))),
            "Skuldahlutfall" = coord_cartesian(ylim = c(pmin(0, min(plot_dat$y)), pmax(1, max(plot_dat$y)))),
            "Veltufjárhlutfall" = coord_cartesian(ylim = c(0, 3))
        )
        
        p <- plot_dat |> 
            ggplot(aes(ar, y, col = sveitarfelag)) +
            geom_hline(yintercept = hlines[[input$y_var]], lty = 2, alpha = 0.5) +
            geom_line() +
            geom_point() +
            x_scales[[input$y_var]] +
            y_scales[[input$y_var]] +
            scale_colour_brewer(type = "qual", palette = "Set1") +
            coords[[input$y_var]] +
            theme_half_open(font_size = 16) +
            theme(legend.position = "top") +
            labs(x = NULL,
                 y = NULL,
                 col = NULL,
                 title = input$y_var,
                 subtitle = subtitles[[input$y_var]],
                 caption = "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/")
        
        p
    })
    
    throun_tafla <- eventReactive(input$goButton, {
        
        
        
        my_digits <- list(
            "Eiginfjárhlutfall" = 2,
            "Framlegð sem hlutfall af tekjum" = 2,
            "Handbært fé per íbúi" = 0,
            "Jöfnunarsjóðsframlög per íbúi" = 0,
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 2,
            "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum" = 2,
            "Skuldir per íbúi"  = 0,
            "Skuldir sem hlutfall af tekjum" = 2,
            "Skuldaaukning" = 3,
            "Skuldahlutfall" = 2,
            "Útsvar og fasteignaskattur per íbúi" = 0,
            "Veltufé frá rekstri sem hlutfall af tekjum" = 2,
            "Veltufjárhlutfall" = 2
        )
        
        if (is.null(my_digits[[input$y_var]])) my_digits[[input$y_var]] <- 0
        
        table_dat <- throun_df() |> 
            select(-visitala_2021, -x) |> 
            select(Ár = ar, sveitarfelag, y) |> 
            mutate(y = round(y, digits = my_digits[[input$y_var]])) |> 
            pivot_wider(names_from = sveitarfelag, values_from = y)
        
        caption <- str_c(input$y_var, " (", input$verdlag, ")")
        
        datatable(
            table_dat,
            extensions = "Buttons",
            rownames = FALSE,
            caption = caption,
            options = list(
                dom = "Bfrtip",
                buttons = c("csv", "excel", "pdf"),
                pageLength = 20,
                lengthChange = FALSE,
                searching = FALSE,
                language = list(
                    decimal = ",",
                    thousands = ".",
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                )
            )
        )
        
    })
    
    output$throun_plot <- renderPlot({
        throun_plot()
    })
    
    output$throun_tafla <- renderDT({
        
        throun_tafla()
    })
    
    
    #### Dreifing ####
    
    dreifing_df <- reactive({
        
        req(input$y_var_distribution)
        
        y_vars <- list(
            "Eiginfjárhlutfall" = "eiginfjarhlutfall",
            "Framlegð per íbúi (kjörtímabil í heild)" = "framlegd_per_ibui_kjortimabil",
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = "framlegd_hlutf_kjortimabil",
            "Handbært fé per íbúi" = "handbaert_fe_per_ibui",
            "Jöfnunarsjóðsframlög per íbúi" = "jofnunarsjodur_a_ibua",
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = "hlutf_jofnunarsjods_skottum",
            "Nettó jöfnunarsjóðsframlög per íbúi" = "netto_jofnunarsjod_per_ibui",
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = "rekstrarnidurstada_per_ibui_kjortimabil",
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = "rekstrarnidurstada_hlutf_kjortimabil",
            "Útsvar og fasteignaskattur per íbúi" = "skattur_a_ibua",
            "Skuldir per íbúi"  = "skuldir_per_ibui",
            "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = "skuldaaukning_2021",
            "Skuldahlutfall" = "skuldahlutfall",
            "Veltufé frá rekstri sem hlutfall af tekjum" = "veltufe_hlutf_tekjur",
            "Veltufjárhlutfall" = "veltufjarhlutfall"
        )
        
        
        y_var <- y_vars[[input$y_var_distribution]]
        
        plot_dat <- d |> 
            group_by(sveitarfelag) |> 
            filter(ar == max(ar), hluti == input$hluti_distribution) |> 
            ungroup() |> 
            select(sveitarfelag, ar, y = all_of(y_var)) |> 
            drop_na(y) 
        
        plot_dat
        
    }) |> 
        bindEvent(input$goButton_distribution)
    
    dreifing_plot <- reactive({
        
        plot_dat <- dreifing_df() |> 
            mutate(my_colour = 1 * (sveitarfelag %in% input$vidmid) + 2 * (sveitarfelag == "Heild"),
                   # sveitarfelag = ifelse(my_colour == 1, 
                   #                       str_c("<b style='color:#2171b5'>", sveitarfelag, " (Til ", ar, ")", "</b>"), 
                   #                       ifelse(sveitarfelag == "Heild",
                   #                              "<b style='color:#b2182b'>", sveitarfelag, " (Til ", ar, ")", "</b>",
                   #                              str_c(sveitarfelag, " (Til ", ar, ")"))
                   #                       ), 
                   sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                            sveitarfelag == "Heild" ~ str_c("<b style='color:#b2182b'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                            TRUE ~ str_c(sveitarfelag, " (Til ", ar, ")")),
                   sveitarfelag = fct_reorder(sveitarfelag, y))
        
        x_scales <- list(
            "Eiginfjárhlutfall" = scale_x_continuous(labels = label_percent()),
            "Framlegð per íbúi (kjörtímabil í heild)" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
            "Handbært fé per íbúi" = scale_x_continuous(label = label_number(suffix = " kr"), trans = pseudo_log_trans(base = 10),
                                                        breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6)),
            "Jöfnunarsjóðsframlög per íbúi" = scale_x_continuous(label = label_number(suffix = " kr"), limits = c(0, NA), expand = expansion()),
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = scale_x_continuous(labels = label_percent(), limits = c(0, NA), expand = expansion()),
            "Nettó jöfnunarsjóðsframlög per íbúi" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
            "Skuldir per íbúi" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Skuldir sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = scale_x_continuous(labels = label_percent()),
            "Skuldahlutfall" = scale_x_continuous(labels = label_percent()),
            "Útsvar og fasteignaskattur per íbúi" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Veltufé frá rekstri sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
            "Veltufjárhlutfall" = scale_x_continuous(labels = label_percent())
        )
        
        subtitles <- list(
            "Eiginfjárhlutfall" = "Eiginfjárhlutfall sýnir hlutfall fjármagns sem er í eigu sveitarfélagsins (restin eru skuldir)",
            "Framlegð sem hlutfall af tekjum" = "Framlegð er reglulegar tekjur að frádregnum rekstrargjöldum",
            "Handbært fé per íbúi" = "Handbært fé er það fé sem sveitarfélög eiga eftir þegar búið er að greiða skuldir og skuldbindingar",
            "Nettó jöfnunarsjóðsframlög per íbúi" = "Framlög frá jöfnunarsjóði að frádregnum útgjöldum til jöfnunarsjóðs deilt með íbúafjölda sveitarfélags",
            "Skuldahlutfall" = "Skuldahlutfall sýnir hve stór hluti heildareigna er fjármagnaður með lánum.\nHér þýðir 0% að skuldir séu engar og 100% að eigin eignir eru engar.\nOft er sama orðið notað fyrir skuldir sveitarfélaga sem hlutfall af tekjum, en það á ekki við hér.",
            "Veltufjárhlutfall" = "Veltufjárhlutfall er hlutfall skammtímaskulda deilt upp í eignir sem er hægt að nota í að borga í skammtímaskuldir",
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = "Skuldir eru leiðréttar fyrir vísitölu neysluverðs áður en aukningin er reiknuð"
        )
        
        
        
        
        
        
        
        
        vlines <- list(
            "Eiginfjárhlutfall" = 0,
            "Skuldir per íbúi" = 0,
            "Framlegð per íbúi (kjörtímabil í heild)" = 0,
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = 0,
            "Handbært fé per íbúi" = 0,
            "Jöfnunarsjóðsframlög per íbúi" = 0,
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 0,
            "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = 0,
            "Skuldir sem hlutfall af tekjum" = 1,
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = 0,
            "Skuldahlutfall" = 0,
            "Útsvar og fasteignaskattur per íbúi" = 0,
            "Veltufé frá rekstri sem hlutfall af tekjum" = 0,
            "Veltufjárhlutfall" = 1
        )
        
        
        segments <- list(
            "Eiginfjárhlutfall" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Framlegð per íbúi (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Handbært fé per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Jöfnunarsjóðsframlög per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Nettó jöfnunarsjóðsframlög per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldir per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldir sem hlutfall af tekjum" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldahlutfall" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Útsvar og fasteignaskattur per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Veltufé frá rekstri sem hlutfall af tekjum"  = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Veltufjárhlutfall" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3)
        )
        
        
        
        
        coords <- list(
            "Eiginfjárhlutfall" = coord_cartesian(xlim = c(pmin(0, min(plot_dat$y)), pmax(1, max(plot_dat$y)))),
            "Skuldahlutfall" = coord_cartesian(xlim = c(pmin(0, min(plot_dat$y)), pmax(1, max(plot_dat$y)))),
            "Veltufjárhlutfall" = coord_cartesian(xlim = c(0, 3))
        )
        
        my_segment <- segments[[input$y_var_distribution]]
        my_x_scale <- x_scales[[input$y_var_distribution]]
        my_vline <- vlines[[input$y_var_distribution]]
        my_coords <- coords[[input$y_var_distribution]]
        my_subtitle <- subtitles[[input$y_var_distribution]]
        my_title <- input$y_var_distribution
        
        p <- plot_dat |> 
            ggplot(aes(y, sveitarfelag)) +
            geom_vline(xintercept = vlines[[input$y_var_distribution]], lty = 2, alpha = 0.5) +
            geom_point(aes(col = factor(my_colour), size = factor(my_colour))) +
            segments[[input$y_var_distribution]] +
            x_scales[[input$y_var_distribution]] +
            scale_colour_manual(values = c("#525252", "#2171b5", "#b2182b")) +
            scale_size_manual(values = c(2, 4, 4)) +
            my_coords +
            theme_half_open() +
            theme(legend.position = "none",
                  axis.text.y = element_markdown(),
                  plot.margin = margin(t = 5, r = 15, b = 5, l = 5)) +
            labs(x = NULL,
                 y = NULL,
                 col = NULL,
                 title = input$y_var_distribution,
                 subtitle = subtitles[[input$y_var_distribution]],
                 caption = "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/")
        
        p
        
        
    }) |> 
        bindCache(input$y_var_distribution, input$hluti_distribution, input$vidmid) |> 
        bindEvent(input$goButton_distribution)
    
    output$dreifing_plot <- renderPlot({
        dreifing_plot()
    })
    
    dreifing_tafla <- eventReactive(input$goButton_distribution, {
        
        
        
        my_digits <- list(
            "Eiginfjárhlutfall" = 2,
            "Framlegð per íbúi (kjörtímabil í heild)" = 0,
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = 2,
            "Handbært fé per íbúi" = 0,
            "Jöfnunarsjóðsframlög per íbúi" = 0,
            "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 2,
            "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = 2,
            "Útsvar og fasteignaskattur per íbúi" = 0,
            "Skuldir per íbúi"  = 0,
            "Skuldir sem hlutfall af tekjum" = 2,
            "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = 3,
            "Skuldahlutfall" = 2,
            "Veltufé frá rekstri sem hlutfall af tekjum" = 2,
            "Veltufjárhlutfall" = 2
        )
        
        if (is.null(my_digits[[input$y_var]])) my_digits[[input$y_var]] <- 0
        
        y_name <- input$y_var_distribution
        
        table_dat <- dreifing_df() |> 
            select(sveitarfelag, ar, y) |> 
            arrange(desc(y)) |> 
            mutate(y = round(y, digits = my_digits[[input$y_var]])) |> 
            rename(Sveitarfélag = sveitarfelag, "Síðasti ársreikningur" = ar, !!y_name := y)
        
        caption <- str_c(input$y_var_distritubion)
        
        datatable(
            table_dat,
            extensions = "Buttons",
            rownames = FALSE,
            caption = caption,
            options = list(
                dom = "Bfrtip",
                buttons = c("csv", "excel", "pdf"),
                pageLength = 200,
                lengthChange = FALSE,
                searching = FALSE,
                language = list(
                    decimal = ",",
                    thousands = ".",
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                )
            )
        ) |> 
            formatStyle(
                target = 'row', columns = 'Sveitarfélag',  
                backgroundColor = styleEqual(input$vidmid, c("#2171b5")),
                color = styleEqual(input$vidmid, "#ffffff")
            ) |> 
            formatStyle(
                target = 'row', columns = 'Sveitarfélag',  
                backgroundColor = styleEqual("Heild", c("#b2182b")),
                color = styleEqual("Heild", "#ffffff")
            )
        
    })
    
    output$dreifing_tafla <- renderDT({
        
        dreifing_tafla()
    })
    
    #### Viðmið ####
    
    my_plot_vidmid <- eventReactive(input$goButton_vidmid, {
        
        
        plot_dat <- d |> 
            filter(ar >= 2010,
                   sveitarfelag %in% input$sveitarfelag_vidmid,
                   hluti %in% input$hluti_vidmid) |> 
            select(sveitarfelag, ar, 
                   nettoskuldir_obs = nettoskuldir_hlutf_tekjur, 
                   rekstrarnidurstada_obs = rekstrarnidurstada_hlutf, 
                   framlegd_obs = framlegd_hlutf, 
                   veltufe_obs = veltufe_hlutf_tekjur,
                   veltufjarhlutfall_obs = veltufjarhlutfall) |> 
            mutate(framlegd_vidmid = nettoskuldir_obs/10,
                   veltufe_vidmid = nettoskuldir_obs/20,
                   rekstrarnidurstada_vidmid = 0,
                   veltufjarhlutfall_vidmid = 1,
                   nettoskuldir_vidmid = 1,
            ) |> 
            pivot_longer(c(-sveitarfelag, -ar), names_to = c("name", "type"), values_to = "value", names_sep = "_") |> 
            pivot_wider(names_from = type, values_from  = value) |> 
            mutate(diff = obs - vidmid,
                   colour = obs >= vidmid,
                   name = fct_recode(name,
                                     "Framlegðarhlutfall" = "framlegd",
                                     "Rekstrarniðurstaða" = "rekstrarnidurstada",
                                     "Veltufé frá rekstri\n(hlutfall af tekjum)" = "veltufe",
                                     "Nettóskuldir\n(hlutfall af tekjum)" = "nettoskuldir",
                                     "Veltufjárhlutfall" = "veltufjarhlutfall") |> 
                       fct_relevel("Nettóskuldir\n(hlutfall af tekjum)")) 
        
        
        p <- plot_dat |> 
            filter(name != "Nettóskuldir\n(hlutfall af tekjum)") |> 
            ggplot() +
            geom_line(aes(ar, vidmid), lty = 2) +
            geom_point(aes(ar, obs, col = colour)) +
            # geom_line(data = plot_dat |> filter(name == "Nettóskuldahlutfall"), 
            #           aes(ar, vidmid), lty = 2, inherit.aes = FALSE, col = "black") +
            geom_point(data = plot_dat |> filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
                       aes(ar, obs), inherit.aes = FALSE, col = "black") + 
            geom_line(data = plot_dat |> filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
                      aes(ar, obs), inherit.aes = FALSE, col = "black") + 
            scale_x_continuous(breaks = c(2021, 2018, 2014)) +
            scale_y_continuous(labels = label_percent(),
                               breaks = pretty_breaks(4)) +
            scale_colour_brewer(type = "qual", palette = "Set1") +
            facet_grid(name ~ sveitarfelag, scales = "free_y") +
            theme_half_open() +
            theme(legend.position = "none", 
                  panel.spacing.y = unit(0.04, units= "npc")) +
            labs(x = NULL,
                 y = NULL,
                 title = str_c("Hvernig gengur sveitarfélögum að standast viðmið Eftirlitsnefndar með fjármálum sveitarfélaga (", input$hluti_vidmid,")?"),
                 subtitle = "Brotin lína er viðmið og er reiknuð út frá nettóskuldum sem hlutfall af tekjum",
                 caption = "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/")
        
        
        p
    })
    
    output$plot_vidmid <- renderPlot({
        my_plot_vidmid()
    })
    
    
}

shinyApp(ui = ui, server = server)
