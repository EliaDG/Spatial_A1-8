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

# INFO -----------------------------------------------------------------------

# HEADER --------------------------------------------------------------------

# Exercise C

# SOURCING ----------------------------------------------------------------- 

source("./code/__packages.R")
#source("./code/__functions.R")

# 1.C) -----
turkiye <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021,
  country = "TR"
)

turkiye$geometry
st_crs(turkiye)

plot_turkiye <- ggplot() +
  geom_sf(data = turkiye, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "Original Projection")

turkiye_eqearth <- st_transform(turkiye, crs = "+proj=eqearth")
plot_eqearth <- ggplot() +
  geom_sf(data = turkiye_eqearth, color = "blue", fill = NA) +
  theme_minimal() +
  labs(title = "Equal Earth Projection")

turkiye_laea <- st_transform(turkiye, crs = "+proj=laea +lat_0=45 +lon_0=30")
plot_laea <- ggplot() +
  geom_sf(data = turkiye_laea, color = "red", fill = NA) +
  theme_minimal() +
  labs(title = "Lambert Azimuthal Equal Area Projection")

combined_plot <- plot_turkiye + plot_eqearth + plot_laea +
  plot_layout(ncol = 1)
combined_plot

#2.C) -------
# tgs00111 (Nights spent at tourist accommodation establishments by NUTS 2 regions)
# tour_occ_anor2 (Net occupancy rate of bed-places in hotels and similar accommodation (NACE Rev. 2 activity I55.1) by NUTS 2 regions)
data_night <- get_eurostat("tgs00111",
                   time_format = "raw",
                   filters = list(
                     TIME_PERIOD = "2022"
                   )) %>% 
  filter(grepl("^TR.*", geo)) %>%
  select(-c(1,3,4)) %>% 
  merge(., turkiye, by = "geo")

ggplot(data_night[,c_resid == "FOR",]) +
  geom_sf(aes(fill = values, geometry = geometry, group = c_resid)) +  # Specify the geometry aesthetic
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Nights spent at tourist accommodation",
       subtitle = "Turkey - NUTS2 Level",
       caption = "Source: Eurostat") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "magma",
                     direction = -1, 
                     name = "Occupacy Rate",
                     guide = guide_colorbar(direction="horizontal", 
                                            barheight = unit(2, units= "mm"),
                                            barwidth = unit(50, units= "mm"),
                                            draw.ulim = T,
                                            title.position="top"))

data_bedh <- get_eurostat("tour_occ_anor2",
                       time_format = "raw",
                       filters = list(
                         TIME_PERIOD = "2022"
                       ))%>% 
  filter(grepl("^TR.*", geo)) %>% 
  select(-c(1,3)) %>% 
  merge(., turkiye, by = "geo")%>% 
  filter(accomunit == "BEDPL")

ggplot(data_bedh) +
  geom_sf(aes(fill = values, geometry = geometry)) +  # Specify the geometry aesthetic
  theme_map() +
  labs(x = NULL, y = NULL,
       title = "Net occupancy rate of bed-places in hotels",
       subtitle = "Turkey - NUTS2 Level",
       caption = "Source: Eurostat") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "magma",
                     direction = -1, 
                     name = "Occupacy Rate",
                     guide = guide_colorbar(direction="horizontal", 
                                            barheight = unit(2, units= "mm"),
                                            barwidth = unit(30, units= "mm"),
                                            draw.ulim = T,
                                            title.position="top"))

# 3.C) -----

