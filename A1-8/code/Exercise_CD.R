# DEPENDENCIES

getwd()
# dir.create("code")
# 
# script_folder <- "code"
# script_name1 <- "__packages.R"
# script_name2 <- "__functions.R"
# script_path1 <- file.path(script_folder, script_name1)
# script_path2 <- file.path(script_folder, script_name2)
# file.create(script_path1); file.create(script_path2)

# TO DO ------------------------------------------------------------------------
# Check satisfaction with visualizations

# HEADER -----------------------------------------------------------------------

# Assignement 1 - Elia's part

# SOURCING --------------------------------------------------------------------- 

source("./code/__packages.R")
#source("./code/__functions.R")

# Excercise C ------------------------------------------------------------------
## 1.C) ------------------------------------------------------------------------
türkiye <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021,
  country = "TR")

#Projection information
türkiye$geometry
st_crs(türkiye)

plot_türkiye <- ggplot() +
  geom_sf(data = türkiye, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "WGS 84 Projection", size = 12)

türkiye_eqearth <- st_transform(türkiye, crs = "+proj=eqearth")
plot_eqearth <- ggplot() +
  geom_sf(data = türkiye_eqearth, color = "blue", fill = NA) +
  theme_minimal() +
  labs(title = "Equal Earth Projection", size = 12)

türkiye_laea <- st_transform(türkiye, crs = "+proj=laea +lat_0=45 +lon_0=30")
plot_laea <- ggplot() +
  geom_sf(data = türkiye_laea, color = "red", fill = NA) +
  theme_minimal() +
  labs(title = "Lambert Azimuthal Equal Area Projection", size = 12)

combined_plot <- plot_türkiye + plot_eqearth + plot_laea +
  plot_layout(ncol = 1)+
  labs(caption="Source: Eurostat")
combined_plot

## 2.C) ------------------------------------------------------------------------
# tgs00111 (Nights spent at tourist accommodation establishments by NUTS 2 regions)
data_night <- get_eurostat("tgs00111",
                   time_format = "raw",
                   filters = list(
                     TIME_PERIOD = "2022"
                   )) %>% 
  filter(grepl("^TR.*", geo)) %>%
  merge(., türkiye, by = "geo") %>% 
  pivot_wider(., names_from = c_resid, values_from = values) %>% 
  select(-c(2:10,12:15)) %>% 
  mutate(DOM_SHARE = DOM/TOTAL,
         FOR_SHARE = FOR/TOTAL,
         TURIST = factor(ifelse(DOM_SHARE > 0.5, 0, 1), levels = c(1, 0), labels = c("Foreign Tourist", "Domestic Tourist")))

# For Labels
centroids <- st_centroid(data_night$geometry)
centroids_df <- st_coordinates(centroids) %>%
  as.data.frame() %>%
  setNames(c("x", "y"))  # Rename columns
data_night <- cbind(data_night, centroids_df)

# Discrete Plot
TR_plot_1 <- ggplot(data_night) +
  geom_sf(aes(fill = TURIST, geometry = geometry)) +
  geom_text(data = data_night[data_night$TURIST == "Foreign Tourist", ], aes(x = x, y = y, label = NUTS_NAME),
            size = 3, color = "black", nudge_y = 0.1) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Tourist Overnight Stay Distribution by Origin",
       subtitle = "Turkey - NUTS2 Level",
       caption = "Source: Eurostat")+
  guides(fill = guide_legend(title = "Tourist Type:")) +
  theme(legend.position = "bottom")

# Continuous Plot
TR_plot_2 <- ggplot(data_night) +
  geom_sf(aes(fill = DOM_SHARE, geometry = geometry)) +
  geom_text_repel(data = data_night[data_night$DOM_SHARE > 0.9, ],
                  aes(x = x, y = y, label = NUTS_NAME),
                  size = 3, color = "black", nudge_y = 0.1,
                  box.padding = 0.5) +  # Adjust padding around labels
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Domestic Tourist Overnight Stay Distribution",
       subtitle = "Turkey - NUTS2 Level",
       caption = "Source: Eurostat") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "viridis",
                     direction = 1, 
                     name = "Share of Domestic Tourist Overnights",
                     guide = guide_colorbar(direction = "horizontal", 
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(30, units = "mm"),
                                            draw.ulim = TRUE,
                                            title.position = "top"))


# Alternatively:
# tour_occ_anor2 (Net occupancy rate of bed-places in hotels and similar accommodation (NACE Rev. 2 activity I55.1) by NUTS 2 regions)
# glimpse(data_night)
# data_bedh <- get_eurostat("tour_occ_anor2",
#                        time_format = "raw",
#                        filters = list(
#                          TIME_PERIOD = "2022"
#                        ))%>% 
#   filter(grepl("^TR.*", geo)) %>% 
#   select(-c(1,3)) %>% 
#   merge(., türkiye, by = "geo")%>% 
#   filter(accomunit == "BEDPL")
# 
# ggplot(data_bedh) +
#   geom_sf(aes(fill = values, geometry = geometry)) +  # Specify the geometry aesthetic
#   theme_map() +
#   labs(x = NULL, y = NULL,
#        title = "Net occupancy rate of bed-places in hotels",
#        subtitle = "Turkey - NUTS2 Level",
#        caption = "Source: Eurostat") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis(option = "magma",
#                      direction = -1, 
#                      name = "Occupacy Rate",
#                      guide = guide_colorbar(direction="horizontal", 
#                                             barheight = unit(2, units= "mm"),
#                                             barwidth = unit(30, units= "mm"),
#                                             draw.ulim = T,
#                                             title.position="top"))

## 3.C) ------------------------------------------------------------------------
'There are two conceptually different ways to store visualizations: raster-based and vector-based formats. 
1. **Raster-Based Formats**:
   - **Examples**: PNG, JPEG
   - **Description**: Raster graphics store image data as a grid of pixels, 
   where each pixel contains color information. They are resolution-dependent, 
   meaning they can lose quality when scaled up.
   - **Appropriate for**: Visualizations with complex color gradients, photographs, 
   and detailed images where pixel-level accuracy is essential. 
   They are commonly used for web graphics and photographs.

2. **Vector-Based Formats**:
   - **Examples**: SVG, PDF
   - **Description**: Vector graphics store image data using mathematical formulas
   to define shapes, lines, and colors. They are resolution-independent, meaning 
   they can be scaled without losing quality.
   - **Appropriate for**: Visualizations with geometric shapes, charts, maps, 
   and illustrations where scalability and high-quality printing are important. 
   They are commonly used for logos, maps, and diagrams.

For storing visualizations generated using R and ggplot2, a vector-based format 
such as SVG (Scalable Vector Graphics) or PDF (Portable Document Format) is more appropriate. 
This is because ggplot2 generates vector graphics by default, making it easy to save plots
in formats that retain their quality when scaled or printed. SVG is especially useful for
web-based graphics and interactive visualizations, while PDF is suitable for high-quality 
printing and sharing across platforms.

Example code to save a ggplot2 plot in SVG/PNG format:

```r
# Save the plot as PNG
ggsave("TR_plot_2.png", plot = TR_plot_2, device = "png")

# Save the plot as SVG
ggsave("TR_plot_2.svg", plot = TR_plot_2, device = "svg")
```
'

# Exercise D -------------------------------------------------------------------
## 1.D) ------------------------------------------------------------------------
glimpse(pol_pres15)

df <- pol_pres15 %>% 
  mutate(Winner = factor(ifelse(II_Duda_share > 0.5, 1, 0), levels = c(1, 0), labels = c("Duda", "Komorowski")),
         Majority_votes_brkdwn =ifelse(II_Duda_share > 0.5, II_Duda_share, II_Komorowski_share))

PL_plot_1 <- ggplot(df) +
  geom_sf(aes(fill = Winner, alpha = Majority_votes_brkdwn, geometry = geometry)) +
  scale_fill_manual(values = c("Duda" = "red", "Komorowski" = "blue")) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "2015 Polish Presidential Election: Duda vs. Komorowski",
       subtitle = "Poland - Municipality Level",
       caption = "Source: PKW") +
  guides(fill=guide_legend(title = "Winner:"))

PL_plot_1

## 2.D) ------------------------------------------------------------------------
glimpse(data)

data <- pol_pres15 %>%
  select(1,4,6, 13:19,21,22, 44:50,52,53) %>%
  mutate(I_share_no_answer = (I_voters_sent_postal_voting_package - I_postal_voting_envelopes_received)/I_voters_sent_postal_voting_package,
         I_share_pve_invalid = I_invalid_voting_papers/I_postal_voting_envelopes_received,
         II_share_no_answer = (II_voters_sent_postal_voting_package - II_postal_voting_envelopes_received)/II_voters_sent_postal_voting_package,
         II_share_pve_invalid = II_invalid_voting_papers / II_postal_voting_envelopes_received,
         tot_share_no_answer = (I_voters_sent_postal_voting_package + II_voters_sent_postal_voting_package - I_postal_voting_envelopes_received - II_postal_voting_envelopes_received)/(I_voters_sent_postal_voting_package + II_voters_sent_postal_voting_package),
         tot_share_pve_invalid = (I_invalid_voting_papers + II_invalid_voting_papers)/(I_postal_voting_envelopes_received + II_postal_voting_envelopes_received)) %>%
  mutate(across(where(is.numeric), ~ replace(., is.infinite(.) | is.nan(.), 0)))

ggplot(data) +
  geom_sf(aes(fill =  tot_share_pve_invalid)) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "2015 Polish Presidential Election: investigation",
       subtitle = "Poland - Municipality Level",
       caption = "Source: PKW") +
  scale_fill_viridis(option = "plasma",
                     direction = -1)

## 3.D) ------------------------------------------------------------------------
ds <- pol_pres15 %>% 
  mutate(I_share_Braun = I_Grzegorz.Michal.Braun/I_candidates_total,
         I_share_Jarubas = I_Adam.Sebastian.Jarubas/I_candidates_total,
         I_share_Mikke = I_Janusz.Ryszard.Korwin.Mikke/I_candidates_total,
         I_share_Kowalski = I_Marian.Janusz.Kowalski/I_candidates_total,
         I_share_Kukiz = I_Pawel.Piotr.Kukiz/I_candidates_total,
         I_share_Ogorek = I_Magdalena.Agnieszka.Ogorek/I_candidates_total,
         I_share_Palikot = I_Janusz.Marian.Palikot/I_candidates_total,
         I_share_Tanajno = I_Pawel.Jan.Tanajno/I_candidates_total,
         I_share_Wilk = I_Jacek.Wilk/I_candidates_total,
         avg_turnout = (I_turnout+II_turnout)/2) %>% # only relevant transformation, the others are experiments
  rename(I_share_Duda = I_Duda_share,
         I_share_Komorwski = I_Komorowski_share)

PL_plot_3 <- tm_shape(ds) +
  tm_fill("avg_turnout", palette = "plasma") +
  tm_borders(lwd = 0.5, alpha = 0.4) +
  tm_facets(by = "types", free.scales = FALSE) +
  tm_layout(main.title = "Average Turnout by Municipality",
            title.position = c("center", "top"),
            legend.text.size = 0.8) +
  tm_credits("Source: PKW", position = "left")
PL_plot_3

