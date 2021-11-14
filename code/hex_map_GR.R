#  ____  __   ___               __  __               ___  _           _  _                       
# |__ / /  \ |   \  __ _  _  _ |  \/  | __ _  _ __  / __|| |_   __ _ | || | ___  _ _   __ _  ___ 
#  |_ \| () || |) |/ _` || || || |\/| |/ _` || '_ \| (__ | ' \ / _` || || |/ -_)| ' \ / _` |/ -_)
# |___/ \__/ |___/ \__,_| \_, ||_|  |_|\__,_|| .__/ \___||_||_|\__,_||_||_|\___||_||_|\__, |\___|
#                         |__/               |_|                                      |___/      


# ██╗  ██╗███████╗██╗  ██╗    ███╗   ███╗ █████╗ ██████╗ 
# ██║  ██║██╔════╝╚██╗██╔╝    ████╗ ████║██╔══██╗██╔══██╗
# ███████║█████╗   ╚███╔╝     ██╔████╔██║███████║██████╔╝
# ██╔══██║██╔══╝   ██╔██╗     ██║╚██╔╝██║██╔══██║██╔═══╝ 
# ██║  ██║███████╗██╔╝ ██╗    ██║ ╚═╝ ██║██║  ██║██║     
# ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     

############################
# ┬  ┬┌┐ ┬─┐┌─┐┬─┐┬┌─┐┌─┐
# │  │├┴┐├┬┘├─┤├┬┘│├┤ └─┐
# ┴─┘┴└─┘┴└─┴ ┴┴└─┴└─┘└─┘
###########################

library(tidyverse)
library(sf)
library(here)
library(geogrid)
library(ggtext)
library(ltc)
library(beepr)

############################
# ┌┬┐┌─┐┌┬┐┌─┐┌─┐┌─┐┌┬┐┌─┐
#  ││├─┤ │ ├─┤└─┐├┤  │ └─┐
# ─┴┘┴ ┴ ┴ ┴ ┴└─┘└─┘ ┴ └─┘
############################

# 1. read datasets
greece <- read_sf(here("data","GRC_adm","GRC_adm3.shp"))     # greece

souslik <- read_sf(here("data","Souslik","data_0.shp")) %>%   # c.endangered species
  st_as_sf() 
callitriche <- read_sf(here("data","Callitriche_pulchra","data_0.shp")) %>% 
  st_as_sf()
orodopsima <- read_sf(here("data","Oropodisma_lagrecai","data_0.shp")) %>% 
  st_as_sf()
parnasiana <- read_sf(here("data","parnasiana_gionica","data_0.shp")) %>% 
  st_as_sf()

# 2. Change the crs to follow the crs of greece
st_crs(souslik)     <- st_crs(greece) 
st_crs(callitriche) <- st_crs(greece)
st_crs(orodopsima)  <- st_crs(greece)
st_crs(parnasiana)  <- st_crs(greece)

# 3. Join the datasets with st_join and st_intersects
p1 <- st_join(greece, souslik, join = st_intersects) %>%  
  rename(Souslik=PRESENCE) 

p2 <- st_join(p1, callitriche, join = st_intersects) %>% 
  rename(Callitriche=PRESENCE) %>% 
  mutate(Callitriche=replace(Callitriche,Callitriche==1,2))

p3 <- st_join(p2, orodopsima, join = st_intersects) %>% 
  rename(Orodopsima=PRESENCE) %>% 
  mutate(Orodopsima=replace(Orodopsima,Orodopsima==1,3))

p4 <- st_join(p3, parnasiana, join = st_intersects) %>% 
  rename(Parnasiana=PRESENCE) %>% 
  mutate(Parnasiana=replace(Parnasiana,Parnasiana==1,4)) %>% 
  mutate(species = coalesce(Souslik,Callitriche,Orodopsima,Parnasiana))

# 4. use geogrid to calcuate the grid and assign polygons
gr_grid <- calculate_grid(shape = p4, grid_type = "hexagonal", seed = 1612)

gr_grid_results_sf <- assign_polygons(shape = p4, new_polygons = gr_grid) # it takes some time
beep("fanfare")   

################
# ┌─┐┬  ┌─┐┌┬┐
# ├─┘│  │ │ │ 
# ┴  ┴─┘└─┘ ┴ 
###############

minou=ltc("minou")
library(grid)
draw_key_hex <- function (data, params, size) {
  # hexagon vertex coordinates 
  v <- list(x = c(0.95, 0.725, 0.275, 0.05, 0.275, 0.725), 
            y = c(0.5, 0.110288568297003, 0.110288568297003, 0.5, 0.889711431702997, 0.889711431702997))
  # hexagon grob
  polygonGrob(v$x, v$y, 
              gp = gpar(col = data$colour,
                        fill = alpha(data$fill, data$alpha)))
}

gr_grid_results_sf %>% 
  mutate_at(vars(species), ~replace_na(.,0)) %>% 
  mutate(species=as.factor(species)) %>% 
  mutate(species=fct_relevel(species,"0", after=4)) %>% 
  ggplot(.) +
  geom_sf(aes(fill=as.factor(species)), color="#666666",key_glyph=draw_key_hex) +
  scale_fill_manual(values=c(minou[2],minou[3],minou[1],minou[5],"white"), 
                    name=c("Species"), 
                    labels=  c("*S.citellus*", "*O.lagrecai*", "*P.gionica*", "*C.pulchra*","No presence")) +
  labs(
    title = "Critically endangered species of Greece ",
    subtitle = "<b> The biodiversity of Greece is at high risk.</b> <br> IUCN has marked more than 50 species as critically endangered.<br> The following plot shows the distribution of four critically endangered species:<br> *Spermophilus citellus*, *Oropodisma lagrecai*, *Parnassiana gionica*, *Callitriche pulchra*.",
    caption = "Data: https://www.iucnredlist.org/ · Graph: Loukas Theodosiou"
  ) +
  theme_void() +
  theme(plot.title = element_text(color="black",size=38,face="bold", hjust=0.5),
        plot.subtitle = ggtext::element_markdown(color="#333333",size=16, hjust=0.5),
        plot.background = element_rect(fill = "#eae3dc", color = "#eae3dc"),
        plot.margin = margin(20, 40, 20, 40),
        legend.position = "top", 
        legend.text = element_markdown(size=12),
        legend.margin = margin(15,20,15,20),
        plot.caption = element_text(hjust=0.5))


ggsave("hex_map_GR.pdf", width = 12, height = 11.63, device = cairo_pdf)


