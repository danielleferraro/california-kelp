Overview
========

Comparison of kelp canopy coverage in 1911 to current levels.

Setup
=====

    library(here)
    library(sf)
    library(tidyverse)
    library(ggspatial) # For north arrow and scale bar
    library(cowplot)
    library(paletteer)

Load data
=========

Historical kelp coverage
------------------------

A quick look at the geodatabase of kelp coverage from 1911 indicates
that area calculations for each polygon are already included.

    kelp_historical <- st_read(here("data", "CaliforniaKelp.gdb"))
    glimpse(kelp_historical)
    st_area(kelp_historical) # The areas for each polygon match the values in the "Shape_Area" column

Current kelp coverage
---------------------

Read in kelp coverage shapefiles from 1989, 1999, and 2002-2012. There
appear to be two files associated with 2010.

    (files <- list.files(here("data"), pattern = ".shp$", full.names = TRUE))
    kelp_current <- map(files, st_read) # Note: this is a large (5.7 GB) object.
    kelp_current <- map2(.x = kelp_current, .y = files, ~mutate(.x, 
                                                                file = basename(.y),
                                                                year = parse_number(file)))
    glimpse(kelp_current[[1]])

Inspect datasets
================

Check CRS
---------

Harmonize the coordinate reference systems used by converting the
historical map to use the California Albers CRS.

    st_crs(kelp_historical)
    st_crs(kelp_current[[1]])

    kelp_historical <- st_transform(kelp_historical, st_crs(kelp_current[[1]]))
    st_crs(kelp_historical) == st_crs(kelp_current[[1]])

Make preliminary plots
----------------------

Make simple maps of each shapefile. Here I’ll use the `rnaturalearth`
package to directly download a boundary of California from [Natural
Earth](https://www.naturalearthdata.com/).

    ca_boundary <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>% 
      filter(name == "California") %>% 
      st_transform(st_crs(kelp_historical)) # Harmonize the CRSs

    kelp_survey_plots <- kelp_current %>% 
      # DFW kelp maps
      map(~ggplot() +
            geom_sf(data = ca_boundary, fill = NA) + 
            geom_sf(data = ., fill = "forestgreen", color = "forestgreen") +
            labs(title = .$file) +
            theme_void()
      ) %>% 
      # 1911 map
      rlist::list.prepend(
        ggplot(kelp_historical) +
          geom_sf(data = ca_boundary, fill = NA) + 
          geom_sf(fill = "forestgreen", color = "forestgreen") +
          labs(title = "Digitized 1911 map") +
          theme_void()
      )

    # Save
    pdf(here("output", "annual_kelp_survey_plots.pdf"), height = 4, width = 3)
    ggpubr::ggarrange(plotlist = kelp_survey_plots, ncol = 1, nrow = 1)
    dev.off()

Can these datasets be compared?
-------------------------------

According to the DFW
[website](https://wildlife.ca.gov/Conservation/Marine/Kelp/Aerial-Kelp-Surveys),
not all regions of California were covered in each annual survey. It
appears the two most recent surveys included (2015 and 2015) did cover
all regions:

![Image pulled from DFW website.](data/dfw_survey_plot_from_website.jpg)
\# Comparisons of kelp area

Time series of total kelp area
------------------------------

    # Drop geometry and convert to data frame
    kelp_current_df <- map(kelp_current, st_drop_geometry) %>% 
      bind_rows()
    kelp_historical_df <- st_drop_geometry(kelp_historical)

    # Build data frame of total kelp per year
    area_ts <- kelp_current_df %>% 
      # mutate(Class_Name = tolower(Class_Name),
      #        Class_name = tolower(Class_name),
      #        Type = tolower(Type)) %>% 
      # filter(str_detect(Class_Name, "kelp canopy") | 
      #          str_detect(Class_name, "kelp canopy") |
      #          str_detect(Type, "kelp canopy")) %>% 
      group_by(year) %>%
      summarize(total_area = ifelse(!is.na(first(Shape_Area)), sum(Shape_Area), sum(Area))) %>% 
      add_row(year = 1911, 
              total_area = sum(kelp_historical_df$Shape_Area)) %>% 
      mutate(data_type = factor(if_else(year == 1911, "Historic", "Current"), levels = c("Historic", "Current"))) %>% 
      mutate(complete_survey = if_else(year %in% c(1911, 1989, 1999, 2002, 2003, 2004, 2005, 2008, 2015, 2016), TRUE, FALSE))

    # Plot
    ggplot(data = area_ts, aes(x = year, y = total_area/1e6, fill = complete_survey)) +
      geom_col() +
      scale_x_continuous(breaks = seq(1910, 2020, by = 10)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = c("gray50", "black"), labels = c("Incomplete survey", "Complete survey")) +
        labs(x = NULL,
           y = "Total kelp area (km^2)",
           title = "Kelp area time series",
           fill = NULL) +
      theme_minimal_hgrid() +
      theme(legend.position = "top")

    # ggplot(data = area_ts, aes(x = year, y = total_area/1e6)) +
    #   geom_col() +
    #   labs(x = NULL,
    #        y = "Total kelp area (km^2)",
    #        title = "Annual time series of kelp area") +
    #   scale_x_continuous(breaks = c(1911, seq(1980, 2020, by = 5))) +
    #   facet_grid(~data_type, scales = "free_x", space = "free_x") +
    #   theme_minimal_hgrid() +
    #   theme(strip.text = element_blank())

    ggsave(here("output", "ca_kelp_time_series.pdf"), height = 6, width = 9, dpi = 300)

Plot historical vs. current coverage
------------------------------------

    # a <- ggplot() +
    #   geom_sf(data = ca_boundary, fill = "ivory2") +
    #   geom_sf(data = kelp_historical, size = 1.1, color = "forestgreen") +
    #   annotation_scale(location = "bl", style = "ticks") + # Scale bar
    #   annotation_north_arrow(location = "bl", pad_y = unit(.8, "cm"), height = unit(.4, "cm"), width = unit(.4, "cm")) +
    #   labs(title = "Kelp coverage, 1911", 
    #        fill = "Density", 
    #        color = "Density",
    #        caption = "") +
    #   theme_void()
    # 
    # b <- ggplot() +
    #   geom_sf(data = ca_boundary, fill = "ivory2") +
    #   geom_sf(data = kelp_current[[8]], size = 1.1, color = "forestgreen") +
    #   labs(title = "Kelp coverage, 2016", 
    #        caption = "Scale of kelp areas exaggerated for clarity") +
    #   theme_void()
    # 
    # final <- cowplot::plot_grid(a, b, nrow = 1, align = "hv")
    # 
    # ggsave(here("output", "kelp_map_1911_vs_2016.pdf"), height = 6, width = 8, dpi = 300)

Calculate area changes
----------------------

What is the difference between the area of kelp coverage in 1911 and
that of the most recent year(s) of data?

    sum(kelp_historical_df$Shape_Area) -
    sum(kelp_current_df$Shape_Area[kelp_current_df$year == 2016])

Figure: Historical kelp coverage
================================

    pal <- "RColorBrewer::Greens"
    density_levels <- c("Very heavy", "Heavy", "Medium heavy", "Medium", "Reported", "Thin", "Very thin")

    ggplot() +
      geom_sf(data = ca_boundary, fill = "ivory2") +
      geom_sf(data = kelp_historical, size = 1.1, aes(fill = factor(Kelp, levels = density_levels), color = factor(Kelp, levels = density_levels))) +
      annotation_scale(location = "bl", style = "ticks") + # Scale bar
      annotation_north_arrow(location = "bl", pad_y = unit(.8, "cm"), height = unit(.4, "cm"), width = unit(.4, "cm")) +
      scale_fill_paletteer_d(palette = pal, direction = -1) +
      scale_color_paletteer_d(palette = pal, direction = -1) +
      labs(title = "Kelp coverage, 1911", 
           fill = "Density", 
           color = "Density",
           caption = "Scale of kelp areas exaggerated for clarity") +
      theme_void()

    ggsave(here("output", "kelp_map_1911.pdf"), height = 6, width = 6, dpi = 300)
