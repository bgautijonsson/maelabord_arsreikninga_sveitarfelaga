library(cowplot)
library(tidyverse)
library(scales)
library(pxweb)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)




efnahagur <- read_excel("net-efnahagsreikningur.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti) |> 
    filter(
        tegund2 %in% c("Veltufjármunir Total", "Varanlegir rekstrarfjármunir", "Áhættufjármunir og langtímakröfur",
                       "Skuldbindingar", "Langtímaskuldir", "Skammtímaskuldir", "Eigið fé") | tegund %in% c("Skammtímakröfur á eigin fyrirtæki")
    ) |> 
    mutate(tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |> str_replace(" Total", "")) |> 
    select(-tegund) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6))

rekstur <- read_excel("net-rekstrarreikningur.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti) |> 
    filter(tegund2 %in% c("Gjöld Total", "Tekjur Total") | tegund %in% c("Afskriftir", "Fjármagnsliðir")) |> 
    mutate(tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |> str_replace(" Total", "")) |> 
    select(-tegund) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6))


sjodsstreymi <- read_excel("net-sjodsstreymi.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti, tegund2) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6)) |> 
    filter(tegund2 == "Veltufé frá rekstri Total" | tegund %in% c("Afborganir langtímalána", "Aðrar fjármögnunarhreyfingar")) |>
    mutate(tegund2 = ifelse(is.na(tegund), tegund2, tegund) |> str_replace(" Total", "")) |> 
    select(-tegund)


mannfjoldi <- pxweb_get(
    url ="https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px", 
    query = list(
        "Sveitarfélag" = c("*"),
        "Aldur" = c("-1"),
        "Ár" = c("*"),
        "Kyn" = c("0")
    ),
    verbose = FALSE
) |> 
    as.data.frame() |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    rename(mannfjoldi = mannfjoldi_eftir_sveitarfelagi_kyni_og_aldri_1_januar_1998_2022) |> 
    mutate(ar = parse_number(ar)) |> 
    filter(sveitarfelag != "Alls") |> 
    select(sveitarfelag, ar, mannfjoldi) |> 
    mutate(sveitarfelag = fct_recode(sveitarfelag,
                                     "Akureyrarkaupstaður" = "Akureyrarbær"))

gogn_2021 <- read_csv("arsreikningar_2021_drive.csv")

d <- efnahagur |> 
    select(-tegund3) |> 
    pivot_wider(names_from = tegund2, values_from = total) |> 
    inner_join(
        rekstur |> 
            pivot_wider(names_from = tegund2, values_from = total),
        by = c("ar", "sveitarfelag", "hluti")
    ) |> 
    inner_join(
        sjodsstreymi |> 
            pivot_wider(names_from = tegund2, values_from = total),
        by = c("ar", "sveitarfelag", "hluti")
    ) |> 
    # filter(sveitarfelag %in% gogn_2021$sveitarfelag) |> 
    mutate(heildarskuldir = `Skuldbindingar` + `Langtímaskuldir` + `Skammtímaskuldir`,
           eignir = `Varanlegir rekstrarfjármunir` + `Áhættufjármunir og langtímakröfur` + `Veltufjármunir`) |> 
    select(ar, sveitarfelag, hluti, heildarskuldir, eignir, tekjur = "Tekjur", gjold = "Gjöld", afskriftir = "Afskriftir", fjarmagnslidir = "Fjármagnsliðir",
           eigid_fe = "Eigið fé", veltufjarmunir = "Veltufjármunir", skammtimakrofur_eigin_fyrirtaeki = "Skammtímakröfur á eigin fyrirtæki",
           skammtimaskuldir = "Skammtímaskuldir", 
           veltufe = "Veltufé frá rekstri") |> 
    bind_rows(
        gogn_2021
    ) |> 
    mutate_at(vars(heildarskuldir:veltufe), ~ .x * 1000) |> 
    inner_join(mannfjoldi,
               by = c("ar", "sveitarfelag")) |> 
    mutate(nettoskuldir = heildarskuldir - veltufjarmunir + skammtimakrofur_eigin_fyrirtaeki,
           skuldir_hlutf_tekjur = heildarskuldir / tekjur,
           nettoskuldir_hlutf_tekjur = nettoskuldir / tekjur,
           veltufjarhlutfall = veltufjarmunir / skammtimaskuldir,
           eignahlutf = eignir / heildarskuldir,
           rekstrarnidurstada = tekjur - gjold + fjarmagnslidir,
           rekstrarnidurstada_hlutf = rekstrarnidurstada / tekjur,
           framlegd = rekstrarnidurstada + afskriftir,
           framlegd_hlutf = framlegd / tekjur,
           eiginfjarhlutfall = eigid_fe / eignir,
           skuldahlutfall = 1 - eiginfjarhlutfall,
           skuldir_per_ibui = heildarskuldir / mannfjoldi,
           veltufe_hlutf_tekjur = veltufe / tekjur,
           hluti = fct_recode(hluti,
                              "A-hluti" = "A_hluti",
                              "A og B-hluti" = "A_og_B_hluti")) |> 
    group_by(sveitarfelag, hluti) |> 
    mutate(skuldaaukning = ifelse(sveitarfelag != "Múlaþing" & ar >= 2018, heildarskuldir / heildarskuldir[ar == 2018] - 1, NA),
           skuldaaukning_hlutf_tekjur = ifelse(sveitarfelag != "Múlaþing" & ar >= 2018, skuldir_hlutf_tekjur / skuldir_hlutf_tekjur[ar == 2018] - 1, NA),
           rekstrarnidurstada_kjortimabil = sum(rekstrarnidurstada * (ar >= 2018)),
           framlegd_kjortimabil = sum(framlegd * (ar >= 2018)),
           tekjur_kjortimabil = sum(tekjur * (ar >= 2018)),
           rekstrarnidurstada_hlutf_kjortimabil = rekstrarnidurstada_kjortimabil / tekjur_kjortimabil,
           rekstrarnidurstada_per_ibui_kjortimabil = rekstrarnidurstada_kjortimabil / mean(mannfjoldi[ar %in% 2018:2021]),
           framlegd_hlutf_kjortimabil = framlegd_kjortimabil / tekjur_kjortimabil,
           framlegd_per_ibui_kjortimabil = framlegd_kjortimabil / mean(mannfjoldi[ar %in% 2018:2021])) |> 
    ungroup()


d |> write_csv(here("maelabord_arsreikninga_sveitarfelaga", "arsreikningagogn.csv"))

