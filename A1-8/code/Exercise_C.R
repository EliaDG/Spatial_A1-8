getwd()
# dir.create("code")
# dir.create("plot")
# script_folder <- "code"
# script_name <- "__packages.R"
# script_path1 <- file.path(script_folder, script_name)
# file.create(script_path)

# TO DO ------------------------------------------------------------------------

# HEADER -----------------------------------------------------------------------

# Assignment 1

# SOURCING --------------------------------------------------------------------- 

source("./code/__packages.R")

#Original Version exe C, misunderstood I had to do it just for one country
# Türkiye <- get_eurostat_geospatial(
#   resolution = "01",
#   nuts_level = 2,
#   year = 2021,
#   country = "TR")
# 
# #Projection information
# Türkiye$geometry
# st_crs(Türkiye)
# 
# plot_Türkiye <- ggplot() +
#   geom_sf(data = Türkiye, color = "black", fill = NA) +
#   theme_minimal() +
#   labs(title = "WGS 84 Projection", size = 12)
# 
# Türkiye_eqearth <- st_transform(Türkiye, crs = "+proj=eqearth")
# plot_eqearth <- ggplot() +
#   geom_sf(data = Türkiye_eqearth, color = "blue", fill = NA) +
#   theme_minimal() +
#   labs(title = "Equal Earth Projection", size = 12)
# 
# Türkiye_laea <- st_transform(Türkiye, crs = "+proj=laea +lat_0=45 +lon_0=30")
# plot_laea <- ggplot() +
#   geom_sf(data = Türkiye_laea, color = "red", fill = NA) +
#   theme_minimal() +
#   labs(title = "Lambert Azimuthal Projection", size = 12)
# 
# TR_plot_1 <- plot_Türkiye + plot_eqearth + plot_laea +
#   plot_layout(ncol = 2) +
#   labs(caption="Source: Eurostat")

# tgs00111 (Nights spent at tourist accommodation establishments by NUTS 2 regions)
# data_night <- get_eurostat("tgs00111",
#                    time_format = "raw",
#                    filters = list(
#                      TIME_PERIOD = "2022"
#                    )) %>% 
#   filter(grepl("^TR.*", geo)) %>%
#   merge(., Türkiye, by = "geo") %>% 
#   pivot_wider(., names_from = c_resid, values_from = values) %>% 
#   select(-c(2:10, 12:15)) %>% 
#   mutate(DOM_SHARE = DOM/TOTAL,
#          FOR_SHARE = FOR/TOTAL,
#          TURIST = factor(ifelse(DOM_SHARE > 0.5, 0, 1), levels = c(1, 0), labels = c("Foreign Tourist", "Domestic Tourist")))
# 
# # For Labels
# centroids <- st_centroid(data_night$geometry)
# centroids_df <- st_coordinates(centroids) %>%
#   as.data.frame() %>%
#   setNames(c("x", "y"))  # Rename columns
# data_night <- cbind(data_night, centroids_df)
# 
# # Discrete Plot
# TR_plot_2 <- ggplot(data_night) +
#   geom_sf(aes(fill = TURIST, geometry = geometry)) +
#   geom_text(data = data_night[data_night$TURIST == "Foreign Tourist", ], aes(x = x, y = y, label = NUTS_NAME),
#             size = 3, color = "black", nudge_y = 0.1) +
#   theme_map() +
#   labs(x = NULL, y = NULL,
#        title = "Distribution of Tourist Overnight-Stay by Origin",
#        subtitle = "Türkiye - NUTS2 Level",
#        caption = "Source: Eurostat")+
#   guides(fill = guide_legend(title = "Tourist Type:")) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(face = "bold"))
# 
# # Continuous Plot
# TR_plot_3 <- ggplot(data_night) +
#   geom_sf(aes(fill = DOM_SHARE, geometry = geometry)) +
#   geom_text_repel(data = data_night[data_night$DOM_SHARE > 0.95, ],
#                   aes(x = x, y = y, label = NUTS_NAME),
#                   size = 3, color = "black", nudge_y = 0.1,
#                   box.padding = 0.5,
#                   arrow = arrow(length = unit(0.75, "mm") , ends = "last", type = "open")) +
#   theme_map() +
#   labs(x = NULL, y = NULL,
#        title = "Distribution of Domestic Tourist Overnight-stay",
#        subtitle = "Türkiye - NUTS2 Level",
#        caption = "Source: Eurostat") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(face = "bold")) +
#   scale_fill_viridis(option = "viridis",
#                      direction = 1, 
#                      name = "Share of Domestic Tourist Overnights",
#                      guide = guide_colorbar(direction = "horizontal", 
#                                             barheight = unit(2, units = "mm"),
#                                             barwidth = unit(54, units = "mm"),
#                                             draw.ulim = TRUE,
#                                             title.position = "top"))
# 

# Exercise C -------------------------------------------------------------------
## 1.C) ------------------------------------------------------------------------
Europe <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021) %>%
  filter(!NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRZZ",
                         "PT20", "PT30", "PTZZ", "ES70", "ESZZ", "NO0B", "NOZZ"))

#Projection information
Europe$geometry
st_crs(Europe)

plot_Europe <- ggplot() +
  geom_sf(data = Europe, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "WGS 84 Projection", size = 12)

Europe_laea <- st_transform(Europe, crs = "+proj=laea +lat_0=45 +lon_0=30")
plot_laea <- ggplot() +
  geom_sf(data = Europe_laea, color = "red", fill = NA) +
  theme_minimal() +
  labs(title = "Lambert Azimuthal Projection", size = 12)

EU_plot_1 <- plot_Europe + plot_laea +
  plot_layout(ncol = 2) +
  labs(caption="Source: Eurostat")

## 2.C) ------------------------------------------------------------------------
# tgs00111 (Nights spent at tourist accommodation establishments by NUTS 2 regions)
data_nightstay <- get_eurostat("tgs00111",
                               time_format = "raw",
                               filters = list(
                                 TIME_PERIOD = "2022"
                               )) %>% 
  merge(.,Europe, by = "geo", all=TRUE) %>%
  filter(!NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRZZ",
                         "PT20", "PT30", "PTZZ", "ES70", "ESZZ", "NO0B", "NOZZ")) %>% 
  pivot_wider(., names_from = c_resid, values_from = values) %>%
  mutate(DOM_SHARE = DOM/TOTAL,
         FOR_SHARE = FOR/TOTAL,
         TURIST = factor(ifelse(DOM_SHARE > 0.5, 0, 1), levels = c(1, 0), labels = c("Foreign Tourist", "Domestic Tourist")))

# Discrete Plot
EU_plot_2 <- ggplot(data_nightstay) +
  geom_sf(aes(fill = TURIST, geometry = geometry)) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Distribution of Tourist Overnight-Stay by Origin",
       subtitle = "Europe - NUTS2 Level",
       caption = "Source: Eurostat")+
  guides(fill = guide_legend(title = "Tourist Type:")) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# Continuous Plot
EU_plot_3 <- ggplot(data_nightstay) +
  geom_sf(aes(fill = DOM_SHARE, geometry = geometry)) +
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Distribution of Domestic Tourist Overnight-stay",
       subtitle = "Europe - NUTS2 Level",
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
# ggsave("EU_plot_1.svg", plot = EU_plot_1, path = "./plot", device = "svg", width = 30, height = 15, units = "cm")
# ggsave("EU_plot_2.svg", plot = EU_plot_2, path = "./plot", device = "svg", width = 30, height = 15, units = "cm")
# ggsave("EU_plot_3.svg", plot = EU_plot_3, path = "./plot", device = "svg", width = 30, height = 15, units = "cm")

EU_plot_1
EU_plot_2
EU_plot_3

save.image(file = "Workspace_C.RData")
