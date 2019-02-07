##############
# plot a map #
##############

#library(rgdal)
norge <- rgdal::readOGR(dsn = "tests/testthat/data/maps", layer = "eldre", encoding = "ESRI Shapefile" )

norge <- rgdal::readOGR(dsn = "tests/testthat/data/maps", layer = "eldre" )

norge4 <- raster::shapefile("tests/testthat/data/maps/eldre")


# https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")



original <- rgdal::readOGR(dsn = "../kart/Norge_Oslo", layer = "HelseAtlas_Dagkir_v2_OsloSentrum")

# reduce the resolution by the use of the Douglas-Peucker algorithm.
utah <- rgeos::gSimplify(original, tol = 500)

#utah@data$id = rownames(original@data)
utah.points = fortify(utah, region="id")
#utah.df = join(utah.points, utah@data, by="id")

ggplot(utah) + 
  aes(long,lat,group=group) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() #+
#  scale_fill_brewer("Utah Ecoregion")


# Lagre redusert størrelse, shapefile
require("rgdal") # requires sp, will use proj.4 if installed
require("rgeos") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

# Reduser kart-størrelse
original <- rgdal::readOGR(dsn = "../kart/Norge_Oslo", layer = "HelseAtlas_Dagkir_v2_OsloSentrum")
test2 <- gSimplify(original, tol = 500)
test3 <- as(test2, "SpatialPolygonsDataFrame")
# Read shapefile attributes
df = data.frame(original)
# Keep shp attributes
test3 <- SpatialPolygonsDataFrame(test2, df)
rgdal::writeOGR(test3,dsn = "../kart/Norge_Oslo", layer = "compressed", driver="ESRI Shapefile")



# Lagre redusert størrelse, shapefile
require("rgdal") # requires sp, will use proj.4 if installed
require("rgeos") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

# Reduser kart-størrelse
kols <- readOGR(dsn = "tests/testthat/data/maps", layer = "KOLS_4")
test2 <- gSimplify(kols, tol = 50)
test3 <- as(test2, "SpatialPolygonsDataFrame")
# Read shapefile attributes
df = data.frame(kols)
# Keep shp attributes
test3 <- SpatialPolygonsDataFrame(test2, df)

rgdal::writeOGR(test3,dsn = "tests/testthat/data/maps", layer = "kols2", driver="ESRI Shapefile")


# Reduser kart-størrelse
avtspes2 <- readOGR(dsn = "tests/testthat/data/maps", layer = "Helseatlas_Dagkirurgi_v2")
test2 <- gSimplify(avtspes2, tol = 50)
test3 <- as(test2, "SpatialPolygonsDataFrame")
# Read shapefile attributes
df = data.frame(avtspes2)
# Keep shp attributes
test3 <- SpatialPolygonsDataFrame(test2, df)

rgdal::writeOGR(test3,dsn = "tests/testthat/data/maps", layer = "dagkir2", driver="ESRI Shapefile")



# read geojson and convert to shp




eldre <- readOGR(dsn = "tests/testthat/data/maps", layer = "eldre")
eldre@data$id <- rownames(eldre@data)
eldre.points <- fortify(eldre, region="id")
eldre.df <- join(eldre.points, eldre@data, by="id")

test <- eldre@polygons
test2 <- rgeos::gSimplify(eldre, tol = 5000)

test3 <- fortify(test2, region = "id")

test4 <- join(test3, eldre@data, by = "id")

ggplot(test4,aes(x=long, y=lat, fill=bohf_str)) + 
  #  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group))


ggplot(eldre.df,aes(x=long, y=lat, fill=bohf_str)) + 
#  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group))



library(rgdal)

plot(norge3)

ggplot2::ggplot(data = norge3)

plot(norge)

head(norge$data)

print(sp::proj4string(norge))

plot(norge, axes=TRUE, border="gray")

ggplot2::ggplot(data = norge)

norge2 <- sf::read_sf(dsn = "tests/testthat/data/maps", layer = "eldre")

norge3 <- rgdal::readOGR(dsn = "tests/testthat/data/maps", layer = "Kommune_FLATE")

sapply(norge3, class)

norge3

plot(norge3)

ggplot2::ggplot(data = norge3)

head(norge2$geometry)

head(norge$Shape_Leng)

nrow(norge)

plot(norge)

sapply(norge$OBJECTID, class)

# Just testing

devtools::install_github("Helseatlas/shinymap", ref = "data_independence")
shinymap::submit_application(datasett = kols, HNproxy = TRUE, title = "Helseatlas kols", language = "no")

print(ls())

rm(list=ls())



myfile <- "tests/testthat/data/eldre.json"

myfile <- "tests/testthat/data/barn.json"

myfile <- "tests/testthat/data/kols.json"

myfile <- "tests/testthat/data/dagkir.json"


json_data <- jsonlite::fromJSON(myfile)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features
bo <- data.frame(tbl$features)$name


is.data.frame(bo$name)

#if (testing){
  # Convert all special characters to "normal" characters if running tests,
  # because the sorting with special characters is system dependent.

  conv_list1 <- list("æ", "ø", "å", "Æ",  "Ø", "Å", '-', "St. ")
  conv_list2 <- list("ae","o", "a", "AE", "O", "å", "_", "St ")

  test <- bo
  for (i in 1:length(conv_list1)){
#    test <- data.frame(lapply(bo, function(x) {
#      gsub(conv_list1[i], conv_list2[i], x)
#    }))
    test <- gsub(conv_list1[i], conv_list2[i], test)
  }
#}
data.frame(test)
data.frame(bo)

data.frame(test,bo)

all.equal(test, bo)

tmp <- readIAjson(json_file = myfile)
tmp2 <- readIAjson(json_file = myfile, testing = TRUE)

all.equal(tmp, tmp2)


ref <- readRDS("tests/testthat/data/dagkir.rds")



all.equal(tmp, ref)


str(tmp$bo)

str(tmp2$bo)

data.frame(tmp2 = levels(barn2$area),tmp = levels(barn$area))
data.frame(tmp = levels(tmp$bo))



test <- merge(tmp, ref, by = c("bo", "level1", "level2", "id", "rate"))

length(test$bo)
length(tmp$bo)

df.changes(tmp, ref, KEYS = c("bo"))

akershus <- dplyr::filter(tmp, bo == "Førde")
akershus2 <- dplyr::filter(tmp2, bo == "Førde")

test3 <- df.changes(akershus2, akershus, KEYS = c("bo"))


plotVariasjon(fullDataFrame = tmp, which_id = "i3")

test <- compare::compare(tmp, tmp2)

test2 <- dplyr::all_equal(tmp, tmp2)


test3 <- df.changes(tmp2, tmp, KEYS = c("level1"))

test$tMpartial

httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
devtools::install_github("helseatlas/shinymap", ref = "readIAjson")


rm(list=ls())
json_file <- "tests/testthat/data/kols.json"
testing <- FALSE

# COPY OF readJS.R


# Read the json file
# NOTE: The js-file HAS to be converted from UTF-8 BOM to UTF-8 (in notepad++) before this will work!
json_data <- jsonlite::fromJSON(json_file)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features$name
area <- data.frame(tbl$features)$name

# Name of reference areas are located in json_data$geographies$comparisonFeatures$name
ref_area <- data.frame(tbl$comparisonFeatures)$name

if (testing){
  # Convert all special characters to "normal" characters if running tests,
  # because the sorting with special characters is system dependent.

  conv_list1 <- list("\u00E5", "\u00F8", "\u00E5", "\u00C6",  "\u00D8", "\u00C5", '-', "St. ")
  conv_list2 <- list("ae","o", "aa", "AE", "O", "AA", "", "St")

  for (i in 1:length(conv_list1)){
    area <- gsub(conv_list1[i], conv_list2[i], area)
    ref_area <- gsub(conv_list1[i], conv_list2[i], ref_area)
  }
}

# The rest of the data is located in json_data$geographies$themes
themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()

# Test that number of highest level names (json_data$geographies$themes$name)
# is equal the length of the data (json_data$geographies$themes$indicators)
if (length(themes$name) != length(themes$indicators)){
  stop("Something fishy in your json file. ")
}

# Define an empty data frame, so we can add data to this frame in the loop.
all_data <- data.frame()

# Loop over level one
for (i in 1:length(themes$name)){

  # Names for first level
  level1 <- themes$name[i]

  # Evereything else is stored in next_level
  next_level <- data.frame(themes$indicators[i])

  # Rates to be plotted
  rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
  # Rates for Norway etc
  ref_rates <- data.frame(next_level$comparisonValues)  %>% tibble::as_data_frame()
  # Link to fact sheets
  href <- next_level$href

  # Extract the numeraters and denominators
  extra <- data.frame(next_level$associates)

  # Dummy definition, to later check if level3 is equal to previous level3
  prev_level3 <- "qwerty"

  k = 0

  for (j in 1:length(next_level)){
    # Level 2

    if (!is.na(next_level$id[j])){

      # Names for the second level
      level2 <- next_level$name[j]
      level3 <- NULL
      # Names for the third level, if it exists
      level3 <- try(next_level$date[j])
      # ID for level 2 (not unique with three levels)
      selection_id <- next_level$id[j]

      # numeraters and denominaters stored in extra$values.1 etc.
      k <- j - 1
      if (k == 0){
        post <- ""
      } else {
        post <- paste0(".", k)
      }
      numerater <- data.frame(extra[, paste0("values",post)][2])
      denominator <- data.frame(extra[, paste0("values",post)][1])
      name_numerater <- extra[, paste0("name",post)][2]
      name_denominator <- extra[, paste0("name",post)][1]
      ref_numerater <-  data.frame(extra[, paste0("comparisonValues",post)][2])
      ref_denominator <- data.frame(extra[, paste0("comparisonValues",post)][1])

      combined <- data.frame(area, level1, level2)
      colnames(combined) <- c("area", "level1", "level2")
      ref_combined <- data.frame(ref_area, level1, level2)
      colnames(ref_combined) <- c("area", "level1", "level2")

      # Extract metatext, if present
      properties <- NULL
      properties <- try(next_level$properties)
      metatext <- NULL
      if (!is.null(properties)){
        # Metatext can either be stored in next_level$properties[l]$value[n] or in
        # next_level$properties$value[n], next_level$properties$value.1[n] etc.
        if (length(properties) > 1){
          # If metatext is stored in next_level$properties[l]$value[n]
          for (l in 1:length(properties)){
            df_properties <- data.frame(properties[l])
            for (n in 1:length(df_properties$value)){
              if (df_properties$name[n] == "metatext"){
                metatext[l] <- try(df_properties[, "value"][n])
              }
            }
          }
        } else {
          # If metatext is stored in next_level$properties$value[n], next_level$properties$value.1[n] etc.
          df_properties <- data.frame(properties)
          for (l in 1:(length(df_properties)/2)){
            m = l - 1
            if (m == 0){
              for (n in 1:length(df_properties$value)){
                if (df_properties$name[n] == "metatext"){
                  metatext[l] <- try(df_properties[, "value"][n])
                }
              }
            } else {
              for (n in 1:length(df_properties$value)){
                if (df_properties[,paste0("name.", m)][n] == "metatext"){
                  metatext[l] <- try(df_properties[,paste0("value.", m)][n])
                }
              }
            }

          }
        }
      }
      if (is.null(level3)){

        # Only for two-level atlases
        combined["id"] <- selection_id
        ref_combined["id"] <- selection_id

      } else {
        # A new unique identifier has to be defined if it is a three-level atlas
        if (level3 != prev_level3){ # If level3 is not equal to previous level3
          k = k + 1
          id2 <- paste0(selection_id, "j", k)
        }
        combined["level3"] <- level3
        ref_combined["level3"] <- level3
        combined["id"] <- id2
        ref_combined["id"] <- id2
        prev_level3 <- level3

      }
      combined["rate"] <- rates[j]
      ref_combined["rate"] <- ref_rates[j]

      combined["name_numerater"] <- name_numerater
      ref_combined["name_numerater"] <- name_numerater

      combined["numerater"] <- numerater
      ref_combined["numerater"] <- ref_numerater

      combined["name_denominator"] <- name_denominator
      ref_combined["name_denominator"] <- name_denominator

      combined["denominator"] <- denominator
      ref_combined["denominator"] <- ref_denominator

      combined["ref"] <- 0
      ref_combined["ref"] <- 1

      combined["href"] <- href[j]
      ref_combined["href"] <- href[j]

      if (!is.null(metatext)){
        combined["metatext"] <- metatext[j]
        ref_combined["metatext"] <- metatext[j]
      }


      all_data <- rbind(all_data, combined, ref_combined)
    }
  }
}
print(all_data)

