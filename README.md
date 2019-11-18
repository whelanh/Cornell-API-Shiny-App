# Cornell-API-Shiny-App
This R Shiny code displays a list of birds you haven't seen.  You can select different areas and also different "seen" lists (e.g., ABA area, World, ABA year etc.).  This requires updating csv files in the "data" directory when you add seen birds.  To run, you will need to supply your own Cornell API 2.0 key. The output is an HTML table sorted by distance from your chosen location with clickable links to more information about the species, a map link and a link to the eBird hotspot LOCID (if it is a hotspot).
A working example can be seen at: https://whelanh.shinyapps.io/Birds/   For more information about how to create and deploy a R Shiny app see: https://shiny.rstudio.com/tutorial/ 
![Screenshot]("Bird Target List.png")
