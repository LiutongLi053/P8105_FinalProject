library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(scales)
library(glue)
library(htmltools) 

options(tigris_use_cache = TRUE)  

recs = read.csv("EDA/clean data/RECS_Energy_insecurity.csv")
glimpse(recs)
states_sf <- states(cb = TRUE, class = "sf") |> 
  filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI"))
map_data = states_sf |> 
  left_join(recs, by = c("NAME" = "state"))



pal_po <- colorNumeric(
  palette  = "YlOrRd",
  domain   = map_data$PO_proportion,
  na.color = "transparent"
)

pal_ei <- colorNumeric(
  palette  = "Blues",
  domain   = map_data$EI_proportion,
  na.color = "transparent"
)



labels_PO <- map_data |>
  mutate(
    lab = glue(
      "{NAME}<br>",
      "PO_number: {PO_number}<br>",
      "PO_proportion: {percent(PO_proportion, accuracy = 0.1)}"
    )
  ) |>
  pull(lab) |>
  lapply(htmltools::HTML) 

labels_EI <- map_data |>
  mutate(
    lab = glue(
      "{NAME}<br>",
      "EI_number: {EI_number}<br>",
      "EI_proportion: {percent(EI_proportion, accuracy = 0.1)}"
    )
  ) |>
  pull(lab) |>
  lapply(htmltools::HTML)

legend_PO_html <- HTML(glue("
  <div id='legendPO' style='background: white; padding: 8px; border-radius: 4px;'>
    <b>PO proportion</b><br>
    <i style='background: #ffffb2; width: 18px; height: 10px; display:inline-block;'></i> Low<br>
    <i style='background: #fecc5c; width: 18px; height: 10px; display:inline-block;'></i> Medium<br>
    <i style='background: #e31a1c; width: 18px; height: 10px; display:inline-block;'></i> High
  </div>
"))

legend_EI_html <- HTML(glue("
  <div id='legendEI' style='background: white; padding: 8px; border-radius: 4px; display:none;'>
    <b>EI proportion</b><br>
    <i style='background: #deebf7; width: 18px; height: 10px; display:inline-block;'></i> Low<br>
    <i style='background: #9ecae1; width: 18px; height: 10px; display:inline-block;'></i> Medium<br>
    <i style='background: #3182bd; width: 18px; height: 10px; display:inline-block;'></i> High
  </div>
"))

m <- leaflet(map_data) |>
  addTiles() |>
  setView(lng = -98.5, lat = 39.8, zoom = 4) |>
  
  # 图层 1：PO
  addPolygons(
    fillColor   = ~pal_po(PO_proportion),
    color       = "white",
    weight      = 1,
    opacity     = 1,
    fillOpacity = 0.7,
    label       = labels_PO,   
    labelOptions = labelOptions(
      style = list(
        "font-size" = "12px",
        "background" = "rgba(255,255,255,0.85)"
      ),
      direction = "auto"
    ),
    group = "PO proportion"
  ) |>
  
  # 图层 2：EI
  addPolygons(
    fillColor   = ~pal_ei(EI_proportion),
    color       = "white",
    weight      = 1,
    opacity     = 1,
    fillOpacity = 0.7,
    label       = labels_EI,   
    labelOptions = labelOptions(
      style = list(
        "font-size" = "12px",
        "background" = "rgba(255,255,255,0.85)"
      ),
      direction = "auto"
    ),
    group = "EI proportion"
  ) |>
  
  addLayersControl(
    baseGroups = c("PO proportion", "EI proportion"),
    options    = layersControlOptions(collapsed = FALSE)
  ) |>
  
  addControl(legend_PO_html, position = "bottomright") |>
  addControl(legend_EI_html, position = "bottomright")
  
m <- htmlwidgets::onRender(
  m,
  "
  function(el, x) {
    var map = this;

    map.on('baselayerchange', function(e) {

      if (e.name === 'PO proportion') {
        document.getElementById('legendPO').style.display = 'block';
        document.getElementById('legendEI').style.display = 'none';
      }

      if (e.name === 'EI proportion') {
        document.getElementById('legendPO').style.display = 'none';
        document.getElementById('legendEI').style.display = 'block';
      }

    });
  }
  "
)

m  
