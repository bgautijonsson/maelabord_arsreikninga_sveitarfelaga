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
                                          "Rekstrarniðurstaða sem hlutfall af tekjum",
                                          "Skuldir per íbúi", 
                                          "Skuldir sem hlutfall af tekjum",
                                          "Skuldaaukning á kjörtímabilinu",
                                          "Skuldahlutfall",
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
                                  actionButton(
                                      inputId = "goButton",
                                      label = "Sækja myndrit"
                                  ), 
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur:"),
                                  p("Brynjólfur Gauti Guðrúnar Jónsson")
                              ),
                              
                              mainPanel(
                                  plotOutput("plot", height = 500)
                              )
                          )
                 ),
                 
                 tabPanel("Dreifing",
                          
                          sidebarLayout(
                              sidebarPanel(
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
                                          "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)",
                                          "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)",
                                          "Skuldir per íbúi", 
                                          "Skuldir sem hlutfall af tekjum",
                                          "Skuldaaukning (kjörtímabil í heild)",
                                          "Skuldahlutfall",
                                          "Veltufjárhlutfall"
                                      ),
                                      selected = c("Skuldaaukning (kjörtímabil í heild)")
                                  ),
                                  actionButton(
                                      inputId = "goButton_distribution",
                                      label = "Sækja myndrit"
                                  ),
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur"),
                                  h6("Brynjólfur Gauti Guðrúnar Jónsson")
                                  
                              ),
                              
                              
                              mainPanel(
                                  plotOutput("plot_distribution", height = 1000, width = "80%")
                              )
                          )
                          
                 ),
                 tabPanel("Viðmið",
                          
                          sidebarLayout(
                              sidebarPanel(
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
                                  actionButton(
                                      inputId = "goButton_vidmid",
                                      label = "Sækja myndrit"
                                  ),
                                  br(" "),
                                  br(" "),
                                  h5("Höfundur"),
                                  h6("Brynjólfur Gauti Guðrúnar Jónsson")
                                  
                              ),
                              
                              
                              mainPanel(
                                  plotOutput("plot_vidmid", height = 800, width = "100%")
                              )
                          )
                          
                 )
                 
)

server <- function(input, output) {
    
    my_plot <- eventReactive(input$goButton, {
        y_vars <- list(
            "Eiginfjárhlutfall" = "eiginfjarhlutfall",
            "Framlegð sem hlutfall af tekjum" = "framlegd_hlutf",
            "Rekstrarniðurstaða sem hlutfall af tekjum" = "rekstrarnidurstada_hlutf",
            "Skuldir per íbúi"  = "skuldir_per_ibui",
            "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
            "Skuldaaukning á kjörtímabilinu" = "skuldaaukning",
            "Skuldahlutfall" = "skuldahlutfall",
            "Veltufjárhlutfall" = "veltufjarhlutfall"
        )
        
        y_scales <- list(
            "Eiginfjárhlutfall" = scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.25), expand = expansion()),
            "Framlegð sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Rekstrarniðurstaða sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldir per íbúi" = scale_y_continuous(label = label_number(suffix = " kr"), expand = expansion()),
            "Skuldir sem hlutfall af tekjum" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldaaukning á kjörtímabilinu" = scale_y_continuous(labels = label_percent(), expand = expansion()),
            "Skuldahlutfall" = scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.25), expand = expansion()),
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
            "Skuldaaukning á kjörtímabilinu" = scale_x_continuous(breaks = 2018:2021,
                                                                  limits = c(2018, NA)),
            "Skuldahlutfall" = x_scales_year[[as.character(input$ar_fra)]],
            "Veltufjárhlutfall" = x_scales_year[[as.character(input$ar_fra)]]
            
        )
        
        
        
        
        subtitles <- list(
            "Eiginfjárhlutfall" = "Eiginfjárhlutfall (100% - skuldahlutfall) sýnir hlutfall eigna sem er fjármagnað með hagnaði og hlutafé (restin eru skuldasöfnun)",
            "Framlegð sem hlutfall af tekjum" = "Framlegð er reglulegar tekjur mínus gjöld að frádregnum rekstrargjöldum",
            "Skuldahlutfall" = "Skuldahlutfall sýnir hve stór hluti heildareigna er fjármagnaður með lánum",
            "Veltufjárhlutfall" = "Veltufjárhlutfall er hlutfall skammtímaskulda deilt upp í eignir sem er hægt að nota í að borga í skammtímaskuldir"
        )
        
        hlines <- list(
            "Eiginfjárhlutfall" = c(0, 1),
            "Framlegð sem hlutfall af tekjum" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum" = 0,
            "Skuldir per íbúi" = NULL,
            "Skuldir sem hlutfall af tekjum" = 1,
            "Skuldaaukning á kjörtímabilinu" = 0,
            "Skuldahlutfall" = c(0, 1),
            "Veltufjárhlutfall" = 1
        )
        
        
        
        y_var <- y_vars[[input$y_var]]
        
        plot_dat <- d |> 
            filter(hluti %in% input$hluti, sveitarfelag %in% input$sveitarfelag, ar >= input$ar_fra) |> 
            select(ar, sveitarfelag, y = all_of(y_var)) |> 
            mutate(x = ar)
        
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
                 subtitle = subtitles[[input$y_var]])
        
        p
    })
    
    output$plot <- renderPlot({
        my_plot()
    })
    
    
    my_plot_distribution <- eventReactive(input$goButton_distribution, {
        y_vars <- list(
            "Eiginfjárhlutfall" = "eiginfjarhlutfall",
            "Framlegð per íbúi (kjörtímabil í heild)" = "framlegd_per_ibui_kjortimabil",
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = "framlegd_hlutf_kjortimabil",
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = "rekstrarnidurstada_per_ibui_kjortimabil",
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = "rekstrarnidurstada_hlutf_kjortimabil",
            "Skuldir per íbúi"  = "skuldir_per_ibui",
            "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
            "Skuldaaukning (kjörtímabil í heild)" = "skuldaaukning",
            "Skuldahlutfall" = "skuldahlutfall",
            "Veltufjárhlutfall" = "veltufjarhlutfall"
        )
        
        x_scales <- list(
            "Eiginfjárhlutfall" = scale_x_continuous(labels = label_percent()),
            "Framlegð per íbúi (kjörtímabil í heild)" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
            "Skuldir per íbúi" = scale_x_continuous(label = label_number(suffix = " kr")),
            "Skuldir sem hlutfall af tekjum" = scale_x_continuous(labels = label_percent()),
            "Skuldaaukning (kjörtímabil í heild)" = scale_x_continuous(labels = label_percent()),
            "Skuldahlutfall" = scale_x_continuous(labels = label_percent()),
            "Veltufjárhlutfall" = scale_x_continuous(labels = label_percent())
        )
        
        subtitles <- list(
            "Eiginfjárhlutfall" = "Eiginfjárhlutfall sýnir hlutfall fjármagns sem er í eigu sveitarfélagsins (restin eru skuldir)",
            "Framlegð sem hlutfall af tekjum" = "Framlegð er reglulegar tekjur að frádregnum rekstrargjöldum",
            "Skuldahlutfall" = "Skuldahlutfall sýnir hve stór hluti heildareigna er fjármagnaður með lánum",
            "Veltufjárhlutfall" = "Veltufjárhlutfall er hlutfall skammtímaskulda deilt upp í eignir sem er hægt að nota í að borga í skammtímaskuldir"
        )
        
        
        
        
        
        
        y_var <- y_vars[[input$y_var_distribution]]
        
        plot_dat <- d |> 
            group_by(sveitarfelag) |> 
            filter(ar == max(ar), hluti == input$hluti_distribution) |> 
            ungroup() |> 
            select(sveitarfelag, ar, y = all_of(y_var)) |> 
            drop_na(y) |> 
            mutate(my_colour = 1 * (sveitarfelag == input$vidmid),
                   sveitarfelag = ifelse(my_colour == 1, 
                                         str_c("<b style='color:#2171b5'>", sveitarfelag, " (Til ", ar, ")", "</b>"), 
                                         str_c(sveitarfelag, " (Til ", ar, ")")), 
                   sveitarfelag = fct_reorder(sveitarfelag, y))
        
        vlines <- list(
            "Eiginfjárhlutfall" = 0,
            "Skuldir per íbúi" = 0,
            "Framlegð per íbúi (kjörtímabil í heild)" = 0,
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = 0,
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = 0,
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = 0,
            "Skuldir sem hlutfall af tekjum" = 1,
            "Skuldaaukning (kjörtímabil í heild)" = 0,
            "Skuldahlutfall" = 0,
            "Veltufjárhlutfall" = 1
        )
        
        
        segments <- list(
            "Eiginfjárhlutfall" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Framlegð per íbúi (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldir per íbúi" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldir sem hlutfall af tekjum" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
            "Skuldaaukning (kjörtímabil í heild)" = isolate(geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3)),
            "Skuldahlutfall" = geom_segment(aes(xend = vlines[[input$y_var_distribution]], yend = sveitarfelag, col = factor(my_colour)), size = 0.3),
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
            scale_colour_manual(values = c("#525252", "#2171b5")) +
            scale_size_manual(values = c(2, 4)) +
            my_coords +
            theme_half_open() +
            theme(legend.position = "none",
                  axis.text.y = element_markdown(),
                  plot.margin = margin(t = 5, r = 15, b = 5, l = 5)) +
            labs(x = NULL,
                 y = NULL,
                 col = NULL,
                 title = input$y_var_distribution,
                 subtitle = subtitles[[input$y_var_distribution]])
        
        p
        
        
    })
    
    output$plot_distribution <- renderPlot({
        my_plot_distribution()
    })
    
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
                                     "Nettóskuldahlutfall" = "nettoskuldir",
                                     "Veltufjárhlutfall" = "veltufjarhlutfall") |> 
                       fct_relevel("Nettóskuldahlutfall")) 
        
        
        p <- plot_dat |> 
            filter(name != "Nettóskuldahlutfall") |> 
            ggplot() +
            geom_line(aes(ar, vidmid), lty = 2) +
            geom_point(aes(ar, obs, col = colour)) +
            # geom_line(data = plot_dat |> filter(name == "Nettóskuldahlutfall"), 
            #           aes(ar, vidmid), lty = 2, inherit.aes = FALSE, col = "black") +
            geom_point(data = plot_dat |> filter(name == "Nettóskuldahlutfall"),
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
                 subtitle = "Brotin lína er viðmið og er reiknuð út frá nettóskuldum sem hlutfall af tekjum")
        
        
        p
    })
    
    output$plot_vidmid <- renderPlot({
        my_plot_vidmid()
    })
    
    
}

shinyApp(ui = ui, server = server)
