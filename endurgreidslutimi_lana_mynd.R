p <- d |> 
    filter(ar >= 2010,
           sveitarfelag %in% c("Reykjavíkurborg", "Garðabær", "Hafnarfjarðarkaupstaður", 
                               "Seltjarnarnesbær", "Kópavogsbær",  "Akureyrarkaupstaður",
                               "Akraneskaupstaður", "Reykjanesbær", "Múlaþing", 
                               "Sveitarfélagið Árborg", "Fjarðabyggð", "Mosfellsbær")) |> 
    mutate(timi_borga_skuldir = ifelse(veltufe <= 0, 1e5, timi_borga_skuldir)) |> 
    ggplot(aes(ar, timi_borga_skuldir)) +
    geom_point(aes(col = hluti)) +
    geom_line(aes(col = hluti)) +
    geom_hline(yintercept = 20, lty = 2) +
    geom_rangeframe() +
    scale_x_continuous(breaks = c(2014, 2018, 2021),
                       limits = c(2009.8, 2021.2)) +
    scale_y_continuous(labels = label_number(suffix = " ár")) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    facet_wrap("sveitarfelag") +
    coord_cartesian(ylim = c(0, 40), expand = FALSE) +
    theme_tufte() +
    theme(legend.position = "top", 
          panel.spacing.y = unit(0.04, units= "npc"),
          plot.title = element_text(face = "bold"),
          strip.background = element_rect(colour = "grey80", fill = "grey90")) +
    labs(x = NULL, y = NULL, col = NULL,
         title = "Árafjöldi sem það tæki sveitarfélög að borga nettóskuldir",
         subtitle = "Reiknað eins og ef allt veltufé frá rekstri færi í afborgun skulda",
         caption = "Kóði og gögn: https://github.com/bgautijonsson/maelabord_arsreikninga_sveitarfelaga")



ggsave(plot = p, filename = "endurgreidslutimi_skulda.png",
       height = 8, width = 1.1 * 8, scale = 1.5, bg = "white")
