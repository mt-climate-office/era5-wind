##ERA5-Land
# Demonstration with Nevada

# install.packages("pak")
pak::pkg_install(c("tidyverse",
                   "multidplyr",
                   "magrittr",
                   # "terra",
                   # "sf",
                   "rgee",
                   "rmapshaper",
                   "FedData",
                   "geojsonio",
                   "future",
                   "stars"
))
# install.packages("terra", type = "source", configure.args = "--with-proj-lib=$(brew --prefix)/lib/")
# install.packages("sf", type = "source", configure.args = "--with-proj-lib=$(brew --prefix)/lib/")

library(tidyverse)
library(multidplyr)
library(magrittr)
library(terra)
library(sf)
library(rgee)
library(ggquiver)
ee_Initialize(drive = TRUE)

sf::sf_use_s2(FALSE)

get_era5 <-
  function(element,
           mask,
           file = tempfile(fileext = ".tif")){
    
    if(file.exists(file)){
      return(
        terra::rast(file) %>%
          terra::sources(file)
      )
    }
    
    collection <-
      ee$ImageCollection("ECMWF/ERA5/MONTHLY")$
      filterDate('1951-01-01', '2025-01-01')$
      select(element)$
      toBands()
    
    # From ImageCollection to local directory
    ee_crs <- collection$projection()$getInfo()$crs
    geometry <- mask %>%
      sf::st_transform(ee_crs) %>%
      sf::st_geometry() %>%
      rgee::sf_as_ee()
    
    rgee::ee_as_rast(
      image = collection,
      region = geometry,
      dsn = file,
      quiet = TRUE
    ) %>%
      terra::sources()
  }

# https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip

aoi <-
  tigris::states() %>%
  dplyr::filter(STUSPS == "NV") %>%
  rmapshaper::ms_simplify() %>%
  sf::st_transform("EPSG:3675") %>%
  sf::st_buffer(50000) %>%
  dplyr::summarise()


if(!file.exists("u_component_of_wind_10m.tif")){
  u_component_of_wind_10m <-
    get_era5(element = "u_component_of_wind_10m",
             mask = aoi,
             file = "u_component_of_wind_10m.tif") %>%
    terra::rast()
}

if(!file.exists("v_component_of_wind_10m.tif")){
  u_component_of_wind_10m <-
    get_era5(element = "v_component_of_wind_10m",
             mask = aoi,
             file = "v_component_of_wind_10m.tif") %>%
    terra::rast()
}

states <-
  rnaturalearthdata::states50 %>%
  sf::st_transform("EPSG:3675") %>%
  sf::st_intersection(aoi) %>%
  dplyr::select()

u_component_of_wind_10m <-
  terra::rast("u_component_of_wind_10m.tif") %>%
  terra::project("EPSG:3675") %>%
  # terra::crop(mcor::mt_state_simple %>%
  #               sf::st_transform("EPSG:7114"), mask = TRUE) %>%
  as.data.frame(xy = TRUE) %>%
  tidyr::pivot_longer(-x:-y) %>%
  dplyr::mutate(year = stringr::str_sub(name, 1, 4) %>%
                  as.integer(),
                month = stringr::str_sub(name, 5, 6) %>%
                  as.integer() %>%
                  magrittr::extract(month.name, .) %>%
                  factor(levels = month.name, ordered = TRUE),
                value = units::set_units(value, "m/s") %>%
                  units::set_units("mi/hr"))

v_component_of_wind_10m <-
  terra::rast("v_component_of_wind_10m.tif") %>%
  terra::project("EPSG:3675") %>%
  # terra::crop(mcor::mt_state_simple %>%
  #               sf::st_transform("EPSG:7114"), mask = TRUE) %>%
  as.data.frame(xy = TRUE) %>%
  tidyr::pivot_longer(-x:-y) %>%
  dplyr::mutate(year = stringr::str_sub(name, 1, 4) %>%
                  as.integer(),
                month = stringr::str_sub(name, 5, 6) %>%
                  as.integer() %>%
                  magrittr::extract(month.name, .) %>%
                  factor(levels = month.name, ordered = TRUE),
                value = units::set_units(value, "m/s") %>%
                  units::set_units("mi/hr"))

u_component_of_wind_10m_trend <-
  u_component_of_wind_10m %>%
  dplyr::filter(year < 2024) %>%
  dplyr::group_by(x, y, month) %>%
  dplyr::summarize(trend = lm(value ~ year) %>%
                     coef() %>%
                     magrittr::extract("year") %>%
                     magrittr::multiply_by(10)
  )

v_component_of_wind_10m_trend <-
  v_component_of_wind_10m %>%
  dplyr::filter(year < 2024) %>%
  dplyr::group_by(x, y, month) %>%
  dplyr::summarize(trend = lm(value ~ year) %>%
                     coef() %>%
                     magrittr::extract("year") %>%
                     magrittr::multiply_by(10)
  )




list(u = 
       u_component_of_wind_10m_trend,
     v = 
       v_component_of_wind_10m_trend
) %>%
  dplyr::bind_rows(.id = "component") %>%
  tidyr::pivot_wider(names_from = component,
                     values_from = trend) %>%
  # dplyr::filter(month == "June") %>%
  ggplot() +
  geom_sf(data = tigris::states() %>%
            dplyr::filter(STUSPS == "NV") %>%
            rmapshaper::ms_simplify() %>%
            sf::st_transform("EPSG:3675"),
          fill = NA,
          color = "black") +
  # geom_sf(data = nv_tribes %>%
  #           sf::st_transform("EPSG:3675"),
  #         fill = NA,
  #         color = "dodgerblue",
  #         linewidth = 0.5) +
  geom_quiver(mapping = aes(x = x,
                            y = y,
                            u = u,
                            v = v),
              color = "black",
              linewidth = 0.1,
              vecsize = 1.5) +
  # facet_wrap(vars(month),
  #            nrow = 4) +
  theme_void() +
  facet_wrap("month") +
  ggtitle("Trend in Average Wind Speed, 1979-2020") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("wind_trend.pdf",
       width = 10,
       height = 10,
       bg = "white")




u_component_of_wind_10m_climate <-
  u_component_of_wind_10m %>%
  dplyr::filter(year > 1990,
                year < 2021) %>%
  dplyr::group_by(x, y, month) %>%
  dplyr::summarize(mean = mean(value) %>%
                     magrittr::multiply_by(10)
  )

v_component_of_wind_10m_climate <-
  v_component_of_wind_10m %>%
  dplyr::filter(year > 1990,
                year < 2021) %>%
  dplyr::group_by(x, y, month) %>%
  dplyr::summarize(mean = mean(value) %>%
                     magrittr::multiply_by(10)
  )





list(u = 
       u_component_of_wind_10m_climate,
     v = 
       v_component_of_wind_10m_climate
) %>%
  dplyr::bind_rows(.id = "component") %>%
  dplyr::mutate(mean = units::drop_units(mean)) %>%
  tidyr::pivot_wider(names_from = component,
                     values_from = mean) %>%
  # dplyr::filter(month == "June") %>%
  ggplot() +
  geom_sf(data = tigris::states() %>%
            dplyr::filter(STUSPS == "NV") %>%
            rmapshaper::ms_simplify() %>%
            sf::st_transform("EPSG:3675"),
          fill = NA,
          color = "black") +
  # geom_sf(data = nv_tribes %>%
  #           sf::st_transform("EPSG:3675"),
  #         fill = NA,
  #         color = "dodgerblue",
  #         linewidth = 0.5) +
  geom_quiver(mapping = aes(x = x,
                            y = y,
                            u = u,
                            v = v),
              color = "black",
              linewidth = 0.1,
              vecsize = 1.5) +
  # facet_wrap(vars(month),
  #            nrow = 4) +
  theme_void() +
  facet_wrap("month") +
  ggtitle("Average Wind Speed, 1991-2020") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("wind_climate.pdf",
       width = 10,
       height = 10,
       bg = "white")


