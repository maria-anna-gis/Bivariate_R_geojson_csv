# Libraries
library(sf)            
library(biscale)       
library(ggplot2)       
library(cowplot)
library(tidyr)
library(dplyr)
library(grid)

# Load geojson and the CSV - change this to match file directory. It was not possible to host via github due to file size.
boundaries <- st_read("/Users/maria/Documents/PLUS/2_Modules/SS25/Spatial_analysis_R_python/age_imd_biv/boundaries.geojson")
data <- read.csv("/Users/maria/Documents/PLUS/2_Modules/SS25/Spatial_analysis_R_python/age_imd_biv/comb.csv")

# Clean names in boundaries and data
boundaries <- boundaries %>%
  mutate(LAD21NM = trimws(tolower(LAD21NM)))
data <- data %>%
  mutate(place = trimws(tolower(place)))

# Merge the datasets
merge_data <- merge(boundaries, data, by.x ="LAD21NM" , by.y = "place")
data_biv <- bi_class(merge_data, x = 'imd', y = "median.age", style = "quantile", dim=4 )
data_biv <- st_make_valid(data_biv)

# Create plot
map_biv <- ggplot () +
  geom_sf(data = data_biv, aes(fill = bi_class), colour = "white", size= 0.1) +
  bi_scale_fill(pal = "PinkGrn", dim = 4) +
  labs(
    title = "A comparison of Index of Multiple Deprivation (IMD) decile\n and Median Age within Local Authorities in England",
  ) +
  bi_theme() +
  theme(legend.position = "none",  # Remove the default legend
        plot.title = element_text(size = rel(0.8), vjust = -0.5, hjust = 0.5),
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),   
        axis.text.y = element_blank(),   
        axis.ticks = element_blank()    
)+
  annotation_custom(
    grob = textGrob("Source: Local Authority Districts boundaries (2019): ONS; Median ages from Population Estimates 2001-2017: ONS; \n IMD 2019: Ministry of Housing, Communities and Local Government (GOV.UK)\nAuthor: Maria Fedyszyn",
    gp = gpar(fontsize = 7)),
    xmin = -Inf, xmax = Inf, ymin = -1,  ymax = -1
)

# Create the bivariate legend
legend <- bi_legend(pal = "PinkGrn", dim = 4, 
                               xlab = "Higher IMD",  
                               ylab = "Ageing population",  
                               size = 9)  

# Positioning of the "legend box" 
final_plot <- ggdraw() +
  draw_plot(map_biv, 0, 0, 1, 1) +
  draw_plot(legend, 0.15, 0.4, 0.2, 0.2) +
  draw_label(
    "This choropleth map illustrates the relationship between\nIMD decile and the median age across Local Authorities\nin England.\n\nA higher IMD decile indicates lower levels of\ndeprivation and better living standards while variations\nin median age highlight demographic contrasts.\n\nQuantile classification presents the\nintersection of deprivation levels and age,\nallowing for the visualisation of spatial\npatterns where socio-economic\ndisadvantage and population age\nstructure converge.", 
    x = 0.14, y = 0.8, size = 10, hjust = 0) #added as a label to work around cowplot off centre placement

# Save the map
ggsave("imd_age_bivar.png", width = 10, height = 10, dpi = 300)