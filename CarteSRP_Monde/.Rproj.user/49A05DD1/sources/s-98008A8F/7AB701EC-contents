library(rnaturalearth)
library(sf)
library(ggplot2) # with support for geom_sf
library(glue)

#Planisphère
w <- ne_countries(scale=50, returnclass = "sf")
w %>% 
  ggplot() +
  geom_sf(fill = "grey", colour = "black") +
  coord_sf( xlim = c(-30, 52), ylim = c(30, 71))



#Sphère :
latitude_centre <- 52
longitude_centre <- 10
europe=TRUE
w2 <- st_transform(w, 
                   crs = glue("+proj=laea +lat_0={latitude_centre} +lon_0={longitude_centre} +datum=WGS84 +units=m +no_defs"))
w2 %>% 
  ggplot() +
  geom_sf(fill = "grey", colour = "black") +
  coord_sf( xlim = c(-42, 52), ylim = c(12, 84))





# all countries at scale 10m
ctrys <- ne_countries(scale = 10, type = "countries", returnclass = "sf")

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# the bouding box polygon in long/lat projection, i.e. axis-aligned
bb <- st_sfc(
  st_polygon(list(cbind(
    c(-42, 52, 52, -42, -42), # x-coordinates (longitudes) of points A,B,C,D
    c(12, 12, 84, 84, 12)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)


# now in in LAEA projection
laeabb <- st_transform(bb, crs = crsLAEA)

# the extent of the bounding box in the new projection
b <- st_bbox(laeabb)
b

gg2 <- ggplot(data = ctrys) +
  geom_sf(fill = "grey", colour = "black") +
  coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))
gg2



########
# IDEM DEPTL93
########

rm(list=ls())
library(sf)
library(rgdal)
library(tidyverse)

#Fond de carte département
data_ue <- read_rds("data_ue.rds")

world <- st_read(dsn = "./PAYS", layer = "PAYS", quiet = TRUE) %>% 
  # st_transform(2154)%>% 
  st_as_sf()
world <- fortify(world)


world %>% 
  select(NAME_FR,CONTINENT,REGION_UN,SUBREGION,REGION_WB)

world %>% 
  ggplot() +
  geom_sf(aes(fill= CONTINENT))+
  coord_sf(datum=NA)

world %>% 
  ggplot() +
  geom_sf(aes(fill= REGION_UN))+
  coord_sf(datum=NA)

world %>% 
  ggplot() +
  geom_sf(aes(fill= SUBREGION))+
  coord_sf(datum=NA)

world %>% 
  ggplot() +
  geom_sf(aes(fill= REGION_WB))+
  coord_sf(datum=NA)


world %>% 
  mutate(VALUE = case_when(
    CONTINENT != "Europe" ~ NA_real_,
    TRUE ~ POP_EST)) %>% 
  # filter(CONTINENT=="Europe",NAME_FR!="Russie") %>% 
  ggplot() +
  geom_sf(aes(fill= VALUE))+
  coord_sf(xlim=c(-10,60),ylim=c(30,80))



world %>% filter(NAME_FR %in% c("Belgique","Luxembourg"))

ref2 <- world %>% 
  as_tibble() %>% 
  select(NAME_FR,CONTINENT,REGION_UN,SUBREGION,REGION_WB) %>% 
  mutate(NAME_FR=case_when(
    NAME_FR %in% c("Belgique","Luxembourg") ~ as.character("Belgique/Lux."),
    TRUE ~ as.character(NAME_FR))) %>% 
  distinct()


world <- world %>% 
  mutate(NAME_FR=case_when(
    NAME_FR %in% c("Belgique","Luxembourg") ~ as.character("Belgique/Lux."),
    TRUE ~ as.character(NAME_FR))) %>% 
  group_by(NAME_FR) %>% 
  summarise() %>% 
  full_join(ref2)


data_ue %>% 
  mutate_if(is.character,as.factor) %>%
  full_join(ref2,by=c("PAYS"="NAME_FR")) %>%
  complete(CRITERE,ANNEE,CULTURE,PAYS) %>% 
  filter(!is.na(CULTURE)) %>% 
  full_join(pays,by=c("PAYS"="NAME_FR")) %>% 
  filter(CULTURE %in% "Colza", CRITERE %in% "Surface", ANNEE %in% 2016) %>% 
  ggplot() +
  geom_sf(aes(fill=VALUE))

coord_sf(xlim=c(-1000000,4000000))


#Fond de carte département
departements_L93 <- st_read(dsn = "C:/Users/cpontet/OneDrive - terresinovia.fr/Mes Documents/Projets/Applications Shiny/Carte SRP/CarteSRP_dvp/DEPARTEMENTS", layer = "DEPARTEMENT", quiet = TRUE) %>% 
  st_transform(2154)%>% 
  full_join(ref,by=c("CODE_DEPT","NOM_DEPT","CODE_REG","NOM_REG"))


data_srp %>% 
  filter(NIVEAU %in% niveau, CULTURE %in% culture, CRITERE %in% critere, ANNEE %in% annee) %>% 
  full_join(fond,by=c("ENTITE"=JoinLevel)) %>% 
  ggplot() +
  geom_sf(aes(fill=cut(VALUE,breaks=LIMITS2)))
































library(rvest)
library(tidyverse)
# 
# hci <- read_html("http://reports.weforum.org/human-capital-report-2016/rankings/") %>%
#   html_node("table") %>% html_table() %>% 
#   rename(country = Economy, rank = `Overall Rank`, human_capital = `Overall Score`) %>% 
#   mutate(country = recode(country, 
#                           'Russian Federation' = 'Russia',
#                           'United Kingdom' = 'UK',
#                           'United States' = 'USA',
#                           'Korea, Rep.' = 'South Korea',
#                           'Kyrgyz Republic' = 'Kyrgyzstan',
#                           'Slovak Republic' = 'Slovakia',
#                           'Macedonia, FYR' = 'Macedonia',
#                           'Iran, Islamic Rep.' = 'Iran',
#                           'Trinidad and Tobago' = 'Trinidad',
#                           'Lao PDR' = 'Laos',
#                           'CÙte d’Ivoire' = 'Ivory Coast'))

# data_ue %>% 
#   mutate_if(is.character,as.factor) %>% 
#   summary


# globe <- map_data("world") %>% rename(country = region)
# 
# data <- left_join(globe, data_ue)
# head(data)
# 
# ggplot(data = data, aes(x = long, y = lat, group = group)) +
#    geom_polygon(aes(fill = VALUE)) +
#    coord_map(projection = "gilbert") +
#    theme_minimal()








library(rnaturalearth)
library(sf)




countries <- ne_countries(returnclass = "sf") %>% 
  rename(country = sovereignt) %>% 
  # mutate(country = recode(country, 
                          # "United States of America" = "USA",
                          # "United Kingdom" = "UK")) %>%
  full_join(data_ue) %>%
  filter(CRITERE=="Surface", ANNEE==2016, CULTURE=="Colza", continent=="Europe" )

ggplot(countries) + 
  geom_sf(aes(fill = VALUE), size = 0.2) +
  theme_minimal()



countries%>% 
  summary

countries$country






