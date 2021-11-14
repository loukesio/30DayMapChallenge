# ██████╗  ██████╗ ████████╗ █████╗ ███╗   ███╗██╗ █████╗ 
# ██╔══██╗██╔═══██╗╚══██╔══╝██╔══██╗████╗ ████║██║██╔══██╗
# ██████╔╝██║   ██║   ██║   ███████║██╔████╔██║██║███████║
# ██╔═══╝ ██║   ██║   ██║   ██╔══██║██║╚██╔╝██║██║██╔══██║
# ██║     ╚██████╔╝   ██║   ██║  ██║██║ ╚═╝ ██║██║██║  ██║
# ╚═╝      ╚═════╝    ╚═╝   ╚═╝  ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═╝

rm(list=ls())
# libraries 
library(rnaturalearth)
library(tidyverse)   
library(ggtext)
library(here)
library(sf)
library(ggfx)
library(showtext)

font_add_google("Oswald", "Oswald")

# modify plot elements globally (for all following plots)

gr <-  rnaturalearth::ne_countries(  
  scale = "medium", returnclass = "sf") %>%  
  select(name, continent, geometry) %>%  
  filter(name == 'Greece') 

greece <- read_sf(here("data","GRC_adm","GRC_adm0.shp"))
rivers <- read_sf(here("data","potamoi","potamoi.shp"))

map3 <- ggplot() +
  geom_sf(data = greece, fill="#333333", color="#333333") +
  geom_sf(data = rivers, color="blue") +
  with_outer_glow(geom_sf(data = rivers, color="#86b4bc"),colour="#86b4bc",sigma=5,expand=0.5)+
  theme_void() +
  labs(
    title = "Rivers of Greece",
    subtitle ='Greece contains 21 major rivers.\nThe three bigest rivers are Aliakmonas, Acheloos and Pineios.',
    caption = "Data: https://geodata.gov.gr/ · Graph: Loukas Theodosiou"
  ) +
  theme(
    plot.background = element_rect(fill = "#999999", 
                                   color = "#999999"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(family = "Oswald", 
                              color = "black",
                              size = 34,
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(t = 20, b = 2)),
    plot.subtitle = element_text(family = "Oswald", 
                                 color = "black",
                                 size = 12,
                                 face = "bold",
                                 hjust = 0.5),
    plot.caption = element_text(family = "Oswald", 
                                color = "black",
                                size = 9,
                                hjust = 0,
                                lineheight = 1.5,
                                margin = margin(0, 12, 6, 200)))


ggsave("hex_map_GR.pdf", width = 12, height = 11.63, device = cairo_pdf)
