# DEPENDENCIES

getwd()
# dir.create("code")
# 
# script_folder <- "code"
# script_name <- "__packages.R"
# script_path1 <- file.path(script_folder, script_name)
# file.create(script_path)

# TO DO ------------------------------------------------------------------------

# HEADER -----------------------------------------------------------------------

# Assignment 1

# SOURCING --------------------------------------------------------------------- 

source("./code/__packages.R")

# Exercise C ------------------------------------------------------------------
## 1.C) ------------------------------------------------------------------------
Türkiye <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021,
  country = "TR")

#Projection information
Türkiye$geometry
st_crs(Türkiye)

plot_Türkiye <- ggplot() +
  geom_sf(data = Türkiye, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "WGS 84 Projection", size = 12)

Türkiye_eqearth <- st_transform(Türkiye, crs = "+proj=eqearth")
plot_eqearth <- ggplot() +
  geom_sf(data = Türkiye_eqearth, color = "blue", fill = NA) +
  theme_minimal() +
  labs(title = "Equal Earth Projection", size = 12)

Türkiye_laea <- st_transform(Türkiye, crs = "+proj=laea +lat_0=45 +lon_0=30")
plot_laea <- ggplot() +
  geom_sf(data = Türkiye_laea, color = "red", fill = NA) +
  theme_minimal() +
  labs(title = "Lambert Azimuthal Projection", size = 12)

TR_plot_1 <- plot_Türkiye + plot_eqearth + plot_laea +
  plot_layout(ncol = 2) +
  labs(caption="Source: Eurostat")

## 2.C) ------------------------------------------------------------------------
# tgs00111 (Nights spent at tourist accommodation establishments by NUTS 2 regions)
data_night <- get_eurostat("tgs00111",
                   time_format = "raw",
                   filters = list(
                     TIME_PERIOD = "2022"
                   )) %>% 
  filter(grepl("^TR.*", geo)) %>%
  merge(., Türkiye, by = "geo") %>% 
  pivot_wider(., names_from = c_resid, values_from = values) %>% 
  select(-c(2:10, 12:15)) %>% 
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
TR_plot_2 <- ggplot(data_night) +
  geom_sf(aes(fill = TURIST, geometry = geometry)) +
  geom_text(data = data_night[data_night$TURIST == "Foreign Tourist", ], aes(x = x, y = y, label = NUTS_NAME),
            size = 3, color = "black", nudge_y = 0.1) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Distribution of Tourist Overnight-Stay by Origin",
       subtitle = "Türkiye - NUTS2 Level",
       caption = "Source: Eurostat")+
  guides(fill = guide_legend(title = "Tourist Type:")) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# Continuous Plot
TR_plot_3 <- ggplot(data_night) +
  geom_sf(aes(fill = DOM_SHARE, geometry = geometry)) +
  geom_text_repel(data = data_night[data_night$DOM_SHARE > 0.95, ],
                  aes(x = x, y = y, label = NUTS_NAME),
                  size = 3, color = "black", nudge_y = 0.1,
                  box.padding = 0.5,
                  arrow = arrow(length = unit(0.75, "mm") , ends = "last", type = "open")) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Distribution of Domestic Tourist Overnight-stay",
       subtitle = "Türkiye - NUTS2 Level",
       caption = "Source: Eurostat") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis(option = "viridis",
                     direction = 1, 
                     name = "Share of Domestic Tourist Overnights",
                     guide = guide_colorbar(direction = "horizontal", 
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(54, units = "mm"),
                                            draw.ulim = TRUE,
                                            title.position = "top"))

## 3.C) ------------------------------------------------------------------------
#Raster-based or Vector-based, see R_markdown file

# Final Plots Exercise C -------------------------------------------------------

TR_plot_1
TR_plot_2
TR_plot_3

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
  guides(fill=guide_legend(title = "Winner:")) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10))

## 2.D) ------------------------------------------------------------------------
data <- pol_pres15 %>%
  select(1,4,6, 13:19,21,22, 44:50,52,53) %>% # just to better visualize the relevant columns
  mutate(anomaly_invalid = ifelse(I_invalid_voting_papers > I_postal_voting_envelopes_received | II_invalid_voting_papers > II_postal_voting_envelopes_received, 1, 0),
         anomaly_spelling = ifelse(I_invalid_voting_papers > 0 & I_PVE_of_which_no_declaration == 0 & I_PVE_of_which_no_signature == 0 & I_PVE_of_which_no_voting_envelope == 0 & I_PVE_of_which_voting_envelope_open == 0 |
                                     II_invalid_voting_papers > 0 & II_PVE_of_which_no_declaration == 0 & II_PVE_of_which_no_signature == 0 & II_PVE_of_which_no_voting_envelope == 0 & II_PVE_of_which_voting_envelope_open == 0, 1, 0),
         anomaly_count = ifelse(I_voting_envelopes_placed_in_ballot_box != I_of_which_voting_papers_taken_from_voting_envelopes | II_voting_envelopes_placed_in_ballot_box != II_of_which_voting_papers_taken_from_voting_envelopes, 1,0),
         anomaly = ifelse(anomaly_invalid == 1 | anomaly_spelling == 1 | anomaly_count == 1, anomaly_invalid + anomaly_spelling + anomaly_count, 0)) %>% 
  mutate(anomaly_type = case_when(
    anomaly_invalid == 0 & anomaly_spelling == 0 & anomaly_count == 0 ~ "None",
    anomaly_invalid == 1 & anomaly_spelling == 0 & anomaly_count == 0 ~ "Invalid Anomaly",
    anomaly_invalid == 0 & anomaly_spelling == 1 & anomaly_count == 0 ~ "Spelling Anomaly",
    anomaly_invalid == 0 & anomaly_spelling == 0 & anomaly_count == 1 ~ "Extraction Anomaly",
    anomaly_invalid == 1 & anomaly_spelling == 1 & anomaly_count == 0 ~ "Invalid + Spelling Anomaly",
    anomaly_invalid == 1 & anomaly_spelling == 0 & anomaly_count == 1 ~ "Invalid + Extraction Anomaly",
    anomaly_invalid == 0 & anomaly_spelling == 1 & anomaly_count == 1 ~ "Extraction + Spelling Anomaly",
    TRUE ~ "Other"
  ))

# Bubble plot: https://r-graph-gallery.com/330-bubble-map-with-ggplot2.html
# Need x and y coordinates thus convert the geometry column to sf object
data_sf <- st_as_sf(data, wkt = "geometry")
centroid <- st_centroid(data_sf)
data_with_centroid <- cbind(data, st_coordinates(centroid))

PL_plot_2 <- ggplot(data_with_centroid) +
  geom_sf(fill = "white") +
  geom_point(data = subset(data_with_centroid, anomaly > 0), aes(x = X, y = Y, color = as.factor(anomaly_type), size = as.factor(anomaly)), alpha = 0.6) +
  theme_void() +
  labs(title = "2015 Polish Presidential Election: anomalies in PVE",
       subtitle = "Poland - Municipality Level",
       caption = "Data source: PKW",
       color = "Anomaly Type:",
       size = "Anomaly Count:") +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10))

## 3.D) ------------------------------------------------------------------------
ds <- pol_pres15 %>%
  pivot_longer(cols = c(I_turnout, II_turnout), names_to = "election", values_to = "turnout")

#attempt to customize palette for borders
#border_colors <- c("Rural" = "red", "Urban" = "yellow", "Urban/rural" = "green", "Warsaw Borough" = "purple")

PL_plot_3 <- tm_shape(ds) +
  tm_fill(col = "turnout", title = "Turnout Share:", palette = "plasma" ) +
  tm_borders(invisible()) +
  tm_facets(by = "election", free.scales = FALSE) +
  tm_layout(main.title = "Turnout Comparison between I and II round",
            title.position = c("center", "top"),
            frame = TRUE) +
  tm_credits("Source: PKW", position = "left")

# Final Plots Exercise D -------------------------------------------------------
PL_plot_1
PL_plot_2
PL_plot_3
