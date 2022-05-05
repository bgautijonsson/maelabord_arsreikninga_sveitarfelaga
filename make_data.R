library(cowplot)
library(tidyverse)
library(scales)
library(pxweb)
library(ggthemes)
library(kableExtra)
library(lubridate)
library(ggtext)
library(here)
library(readxl)
library(janitor)




visitala <- pxweb_get(
    url ="https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/1_vnv/VIS01000.px", 
    query = list(
        "Mánuður" = c("*"),
        "Vísitala"  = c("CPI"),
        "Liður" = c("index")
    ),
    verbose = FALSE
) |> 
    as.data.frame() |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    separate(manudur, into = c("ar", "manudur"), sep = "M", convert = T) |> 
    mutate(manudur = str_pad(manudur, width = 2, side = "left", pad = "0"),
           date = str_c(ar, "-", manudur, "-01") |> ymd()) |> 
    select(-manudur, -ar, -visitala, -lidur) |> 
    mutate(ar = year(date)) |> 
    group_by(ar) |> 
    filter(date == min(date)) |> 
    ungroup() |> 
    mutate(visitala_2021 = visitala_neysluverds / visitala_neysluverds[ar == 2021]) |> 
    select(-date, -visitala_neysluverds)


efnahagur <- read_excel("net-efnahagsreikningur.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti) |> 
    filter(
        tegund2 %in% c("Veltufjármunir Total", "Varanlegir rekstrarfjármunir", "Áhættufjármunir og langtímakröfur",
                       "Skuldbindingar", "Langtímaskuldir", "Skammtímaskuldir", "Eigið fé") | tegund %in% c("Skammtímakröfur á eigin fyrirtæki",
                                                                                                            "Aðrir veltufjármunir")
    ) |> 
    mutate(tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |> str_replace(" Total", "")) |> 
    select(-tegund) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6))

rekstur <- read_excel("net-rekstrarreikningur.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti) |> 
    filter(tegund2 %in% c("Gjöld Total", "Tekjur Total") | tegund %in% c("Afskriftir", 
                                                                         "Fjármagnsliðir",
                                                                         "Framlag Jöfnunarsjóðs", 
                                                                         "Skatttekjur án Jöfnunarsjóðs",
                                                                         "Laun og launatengd gjöld")) |> 
    mutate(tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |> str_replace(" Total", "")) |> 
    select(-tegund) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6))


sjodsstreymi <- read_excel("net-sjodsstreymi.xlsx", skip = 4) |> 
    clean_names() |> 
    fill(ar, sveitarfelag, hluti, tegund2) |> 
    mutate(ar = parse_number(ar),
           sveitarfelag = str_sub(sveitarfelag, start = 6)) |> 
    filter(tegund2 %in% c("Veltufé frá rekstri Total", "Fjárfestingarhreyfingar Total") | tegund %in% c("Afborganir langtímalána",
                                                                                                        "Aðrar fjármögnunarhreyfingar",
                                                                                                        "Tekin ný langtímalán")) |>
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
           eignir = `Varanlegir rekstrarfjármunir` + `Áhættufjármunir og langtímakröfur` + `Veltufjármunir`,
           sveitarfelag = case_when(sveitarfelag %in% c("Múlaþing", "Fljótsdalshérað", "Seyðisfjarðarkaupstaður",
                                                        "Borgarfjarðarhreppur", "Djúpavogshreppur") ~ "Múlaþing",
                                    sveitarfelag %in% c("Sandgerðisbær", "Sveitarfélagið Garður") ~ "Suðurnesjabær",
                                    sveitarfelag %in% c("Breiðdalshreppur", "Fjarðabyggð") ~ "Fjarðabyggð",
                                    sveitarfelag %in% c("Sveitarfélagið Álftanes", "Garðabær") ~ "Garðabær",
                                    TRUE ~ sveitarfelag)) |> 
    select(ar, sveitarfelag, hluti, heildarskuldir, eignir,
           tekjur = "Tekjur", skatttekjur_an_jofnundarsjóðs = "Skatttekjur án Jöfnunarsjóðs", framlag_jofnunarsjods = "Framlag Jöfnunarsjóðs",
           gjold = "Gjöld", afskriftir = "Afskriftir", fjarmagnslidir = "Fjármagnsliðir",
           eigid_fe = "Eigið fé", 
           veltufjarmunir = "Veltufjármunir", skammtimakrofur_eigin_fyrirtaeki = "Skammtímakröfur á eigin fyrirtæki",
           handbaert_fe = "Aðrir veltufjármunir",
           skammtimaskuldir = "Skammtímaskuldir", 
           fjarfestingarhreyfingar = "Fjárfestingarhreyfingar", nyjar_langtimaskuldir = "Tekin ný langtímalán",
           launagjold = "Laun og launatengd gjöld",
           veltufe = "Veltufé frá rekstri") |> 
    group_by(sveitarfelag, ar, hluti) |> 
    summarise_at(vars(heildarskuldir:veltufe), sum) |> 
    ungroup() |> 
    bind_rows(
        gogn_2021
    ) |> 
    mutate_at(vars(heildarskuldir:veltufe), ~ .x * 1000) |> 
    inner_join(mannfjoldi,
               by = c("ar", "sveitarfelag"))

heild <- d |> 
    filter(ar < 2021) |> 
    group_by(ar, hluti) |> 
    summarise_at(vars(heildarskuldir:mannfjoldi), .funs = list(sum)) |> 
    ungroup() |> 
    mutate(sveitarfelag = "Heild")



d <- d |> 
    bind_rows(
        heild
    ) |> 
    mutate(nettoskuldir = heildarskuldir - veltufjarmunir + skammtimakrofur_eigin_fyrirtaeki,
           skuldir_hlutf_tekjur = heildarskuldir / tekjur,
           nettoskuldir_hlutf_tekjur = nettoskuldir / tekjur,
           veltufjarhlutfall = veltufjarmunir / skammtimaskuldir,
           eignahlutf = eigid_fe / (eigid_fe + heildarskuldir),
           rekstrarnidurstada = tekjur - gjold + fjarmagnslidir,
           rekstrarnidurstada_hlutf = rekstrarnidurstada / tekjur,
           framlegd = rekstrarnidurstada + afskriftir,
           framlegd_hlutf = framlegd / tekjur,
           eiginfjarhlutfall = eigid_fe / eignir,
           skuldahlutfall = 1 - eiginfjarhlutfall,
           skuldir_per_ibui = heildarskuldir / mannfjoldi,
           veltufe_hlutf_tekjur = veltufe / tekjur,
           handbaert_fe_per_ibui = handbaert_fe / mannfjoldi,
           hlutf_jofnunarsjods_tekjum = framlag_jofnunarsjods / tekjur,
           hlutf_jofnunarsjods_skottum = framlag_jofnunarsjods / (framlag_jofnunarsjods + skatttekjur_an_jofnundarsjóðs),
           jofnunarsjodur_a_ibua = framlag_jofnunarsjods / mannfjoldi,
           skattur_a_ibua = skatttekjur_an_jofnundarsjóðs / mannfjoldi,
           utgjold_jofnunarsjod = (0.0077 + 0.0099) * tekjur,
           netto_jofnunarsjod = framlag_jofnunarsjods - utgjold_jofnunarsjod,
           netto_jofnunarsjod_per_ibui = netto_jofnunarsjod / mannfjoldi,
           nyjar_fjarfestinga_skulda_hreyfingar = fjarfestingarhreyfingar + nyjar_langtimaskuldir,
           nyjar_fjarfestinga_skulda_hreyfingar_hlutf_skuldir = nyjar_fjarfestinga_skulda_hreyfingar / heildarskuldir,
           launagjold_per_ibui = launagjold / mannfjoldi,
           launagjold_hlutf_gjold = launagjold / gjold,
           timi_borga_skuldir = nettoskuldir / veltufe,
           hluti = fct_recode(hluti,
                              "A-hluti" = "A_hluti",
                              "A og B-hluti" = "A_og_B_hluti")) |> 
    inner_join(
        visitala,
        by = "ar"
    ) |> 
    group_by(ar) |> 
    mutate(hlutf_jofnunarsjod_utgjold = utgjold_jofnunarsjod / sum(utgjold_jofnunarsjod)) |> 
    ungroup() |> 
    arrange(ar, sveitarfelag, hluti) |> 
    group_by(sveitarfelag, hluti) |> 
    mutate(skuldaaukning_2021 = ifelse(sveitarfelag != "Múlaþing" & ar >= 2018, (heildarskuldir / visitala_2021) / (heildarskuldir[ar == 2018] / visitala_2021[ar == 2018]) - 1, NA),
           skuldaaukning = heildarskuldir / heildarskuldir[ar == max(ar)],
           skuldaaukning_hlutf_tekjur = ifelse(sveitarfelag != "Múlaþing" & ar >= 2018, skuldir_hlutf_tekjur / skuldir_hlutf_tekjur[ar == max(ar)] - 1, NA),
           rekstrarnidurstada_kjortimabil = sum(rekstrarnidurstada * (ar >= 2018)),
           framlegd_kjortimabil = sum(framlegd * (ar >= 2018)),
           tekjur_kjortimabil = sum(tekjur * (ar >= 2018)),
           rekstur_3_ar_hlutf_tekjur = rekstrarnidurstada_hlutf + lag(rekstrarnidurstada_hlutf, 1) + lag(rekstrarnidurstada_hlutf, 2),
           veltufe_4ar = (veltufe + lag(veltufe, 1) + lag(veltufe, 2) + lag(veltufe, 3) + lag(veltufe, 4)) / 4,
           timi_borga_skuldir_4ar = nettoskuldir / veltufe_4ar,
           rekstrarnidurstada_hlutf_kjortimabil = rekstrarnidurstada_kjortimabil / tekjur_kjortimabil,
           rekstrarnidurstada_per_ibui_kjortimabil = rekstrarnidurstada_kjortimabil / mean(mannfjoldi[ar %in% 2018:2021]),
           framlegd_hlutf_kjortimabil = framlegd_kjortimabil / tekjur_kjortimabil,
           framlegd_per_ibui_kjortimabil = framlegd_kjortimabil / mean(mannfjoldi[ar %in% 2018:2021])) |> 
    ungroup() 





d |> write_csv(here("maelabord_arsreikninga_sveitarfelaga", "arsreikningagogn.csv"))

