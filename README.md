# Bate Collection Shiny Explorer

Minimal Shiny app for MSc Digital Scholarship, providing
a Shiny catalogue explorer for the Bate Collection CSV with
an additional "Data quality" tab for Digital Scholarship
discussion of missingness, unparsed dates, top values.

## Web interface

You can interact with this app on shinyapps.io using

https://l23zxj-david-de0roure.shinyapps.io/bate-collection-shiny/

## Setup in R Studio

Install required packages:

```r
install.packages(c("shiny","DT","dplyr","stringr","readr","ggplot2"))
```

Download the Bate spreadsheet bate_catalogue.csv to the same directory
as the file app.R
 
Load app.R into RStudio using then choose "Run App" and interact
through the Web interface.

NB The Excel version of the catalogue is available from

https://bate.web.ox.ac.uk/sites/default/files/bate_catalogue.xlsx

The shared CSV version was exported from Excel and stripped of
non-printing characters (e.g. ^M, ^]) using the unix command
```
tr -cd '\11\12\40-\176'
```

which only retains tabs, newlines and printable characters.
