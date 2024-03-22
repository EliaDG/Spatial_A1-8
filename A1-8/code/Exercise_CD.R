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

# TO DO -----------------------------------------------------------------------
# Check satisfaction with visualizations

# HEADER --------------------------------------------------------------------

# Assignement 1 - Elia's part

# SOURCING ----------------------------------------------------------------- 

source("./code/__packages.R")
#source("./code/__functions.R")

# Excercise C ------
## 1.C) -----
türkiye <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021,
  country = "TR"
)

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

## 2.C) -------
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

## 3.C) -----
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

For storing visualizations generated using R and ggplot2, a vector-based format such as SVG (Scalable Vector Graphics) 
or PDF (Portable Document Format) is more appropriate. 
This is because ggplot2 generates vector graphics by default, making it easy to save plots
in formats that retain their quality when scaled or printed. SVG is especially useful for
web-based graphics and interactive visualizations, while PDF is suitable for high-quality 
printing and sharing across platforms.

Example code to save a ggplot2 plot in SVG format:

```r
# Save the plot as PNG
ggsave("TR_plot_2.png", plot = TR_plot_2, device = "png")

# Save the plot as SVG
ggsave("TR_plot_2.svg", plot = TR_plot_2, device = "svg")
```
'


# Excercise D ---------
## 1.D) -------

## 2.D) -------

## 3.D) -------