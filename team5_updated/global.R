# the necessary packages
library(leaflet)
library(RPostgres)
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# connect to the hotel database
con <- dbConnect(
  drv = dbDriver('Postgres'),
  dbname = 'hotel23a_05',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com',
  port = 25061,
  user = 'proj23a_05',
  password = 'AVNS_tyZSMg3JwUAean3CENR',
  sslmode = 'require'
)

# rest table----
rest <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM dining;'
)

# acco table----
acco <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM accommodation;'
)

# faci table----
faci <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM facilities ORDER BY facility_id;'
)

# spa table----
spa <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM spa ORDER BY spamenu_id;'
)

# amen table----
amen <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM amenities;'
)

# attr table----
attr <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM attractions;'
)

# wedd table----
wedd <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM wedding;'
)

# pack table----
pack <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM package;'
)

# input id's of tabs
ti <- c(
  'home', 
  'acco', 
  'faci',
  'dini', 
  'attr', 
  'wedd',
  'pack',
  'abou'
)

# labels of tabs
tl <- c(
  'Home', 
  'Accommodations', 
  'Facilities',
  'Dining', 
  'Attractions', 
  'Wedding',
  'Packages',
  'About Us'
)

# team members----
mem <- c(
  'Surbhi Panwar',
  'Mayu Kikuchi',
  'Kanina Anindita',
  'Amal Aldaoud'
)

# member bios----
bio <- c(
  paste0(
    'Hotel Manager'
  ),
  paste0(
    'Reservation Manager'
  ),
  paste0(
    'Guest Relations'
  ),
  paste0(
    'Receptionist'
  )
)
