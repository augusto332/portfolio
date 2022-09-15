rm(list=ls())

library(xgboost)
library(data.table)
library(tm)
library(Matrix)
library(dplyr)
library(ggplot2)
library(clue)
library(stringr)
library(sf)
library(purrr)


setwd('/Users/augustoalasino/Desktop/Facultad/Mineria/Clase practica 2')

#Importing functions from a different script
source("functions.R")

##~~~~~~~~~~~~~ Loading the data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA_PATH <- "/Users/augustoalasino/Desktop/Facultad/Mineria/TP/competition_data/"

#Loading 100% of the data starting on 04/2021.
ads_data <- load_competition_data(DATA_PATH, sample_ratio = 1, from_when = '2021_04')

#Removing data after september 17, since it would be too close to the period in which we are going to make predictions.
ads_data <- ads_data[!((strftime(ads_data$created_on, "%Y-%m", tz="UTC") == "2021-09") & (strftime(ads_data$created_on, "%d", tz="UTC") >= 17)),]


##~~~~~~~~~~~~~ We create a variable to identify the different sets: training, validation & testing ~~~~~~~~~~~~~
ads_data$train_val_eval <- ifelse(ads_data$created_on >= strptime("2021-10-01", format = "%Y-%m-%d", tz = "UTC"), "eval", "train")

#we take 10% of the observations as validation set, but only after august 23.
ads_data[sample(which(ads_data$train_val_eval == "train" & ads_data$created_on >= strptime("2021-08-23", format = "%Y-%m-%d", tz = "UTC")),
                round(0.1 * sum(ads_data$train_val_eval == "train"))), "train_val_eval"] <- "valid"


#~~~~~~~~~~~~~ Small samplle of the Exploratory Data Analysis performed ~~~~~~~~~~~~~

mean(is.na(ads_data$place_l6)) #97% NA
mean(is.na(ads_data$place_l5)) #87% NA
unique(ads_data$place_l6)
unique(ads_data$place_l5)

mean(is.na(ads_data$current_state)) #99% NA
unique(ads_data$current_state)
mean(is.na(ads_data$place_l4)) #66% NA
class(ads_data$place_l4)

#description & title to lower case
ads_data$description <- tolower(ads_data$description)
ads_data$title <- tolower(ads_data$title)

#Creating variables for title and description amount of characters 
ads_data$title_lenght = nchar(ads_data$title)
ads_data$desc_lenght = nchar(ads_data$description)

#correlation between title_lenght y contacts
ggplot(ads_data, aes(x = title_lenght, y = contacts)) + coord_cartesian(ylim = c(1, 400)) + geom_point()

#correlation between desc_lenght y contacts
ggplot(ads_data, aes(x = desc_lenght, y = contacts)) + coord_cartesian(xlim = c(1, 10000), ylim = c(1, 400)) +
  geom_point()

#rooms historgram
summary(ads_data$rooms)
ggplot(ads_data) + geom_histogram(aes(rooms))

#average prices for rental propoerties ('alquiler').

#postings with contacts
ads_data$has_contacts <- (ads_data$contact > 0)
data_for_plot <- ads_data %>% 
  filter(operation == "Alquiler") %>% filter(has_contacts == TRUE) %>%
  group_by(rooms, place_l1) %>% 
  summarise(mean_price = mean(price_usd, na.rm = TRUE))

ggplot(data_for_plot) + 
  geom_col(aes(x = rooms, y = mean_price), position = "dodge") + 
  facet_grid(. ~ place_l1, scales = "free_y")  +
  coord_cartesian(xlim = c(1, 20), ylim = c(1, 15000)) +
  labs(title = 'Precio promedio en USD de las propiedas en alquiler con contactos, según la cantidad de habitaciones', subtitle = "Diferenciando por país",
       y='Precio en USD', x='Rooms')

#postings in general
data_for_plot_2 <- ads_data %>% 
  filter(operation == "Alquiler") %>%
  group_by(rooms, place_l1) %>% 
  summarise(mean_price = mean(price_usd, na.rm = TRUE))

ggplot(data_for_plot_2) + 
  geom_col(aes(x = rooms, y = mean_price), position = "dodge") + 
  facet_grid(. ~ place_l1, scales = "free_y")  +
  coord_cartesian(xlim = c(1, 20), ylim = c(1, 15000)) +
  labs(title = 'Precio promedio en USD de las propiedas en alquiler, según la cantidad de habitaciones', subtitle = "Diferenciando por país",
       y='Precio en USD', x='Rooms')

#removing 'has_contacts' variable
ads_data$has_contacts <- NULL

#~~~~~~~~~~~~~ DATA CLEANSING & FEATURE ENGINEERING~~~~~~~~~~~~~

#converting to binary variables. 0 if na, 1 if any data.
ads_data$place_l6 <- ifelse(is.na(ads_data$place_l6), 0, 1)
ads_data$place_l5 <- ifelse(is.na(ads_data$place_l5), 0, 1)
ads_data$current_state <- ifelse(is.na(ads_data$current_state), 0, 1)


#unifying las currency IDs
unique(ads_data$currency_id)
ads_data$currency_id <- replace(ads_data$currency_id, ads_data$currency_id == "ars", 'ARS')
ads_data$currency_id <- replace(ads_data$currency_id, ads_data$currency_id == "usd", 'USD')
ads_data$currency_id <- replace(ads_data$currency_id, ads_data$currency_id == "cop", 'COP')
ads_data$currency_id <- replace(ads_data$currency_id, ads_data$currency_id == "pen", 'PEN')
ads_data$currency_id <- as.character(ads_data$currency_id)
unique(ads_data$currency_id)


#creating variables using the date
ads_data$day <- as.integer(strftime(ads_data$created_on, format = "%d", tz = "UTC")) #day
ads_data$month <- as.integer(strftime(ads_data$created_on, format = "%m", tz = "UTC")) #month
ads_data$year <- as.integer(strftime(ads_data$created_on, format = "%Y", tz = "UTC")) #year
ads_data$week_day <- as.integer(strftime(ads_data$created_on, format = "%w", tz = "UTC")) #day of the week
ads_data$year_week <- as.integer(strftime(ads_data$created_on, format = "%W", tz = "UTC")) #week of the year


#extracting amount of rooms using the text variables, to complete NAs following each country criteria
#frist from the title, and if not found, from the description

#looking in the title for the previous string to the word 'amb' 
#amb: abbreviation from the spaish word 'ambientes', widely used in the south american real state terminology
ads_data$ambientes <- word(sub("\\ amb.*", "", ads_data$title),-1)
#replacing words for numbers, only for values that make sense.
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "dos", 2)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "tres", 3)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "cuatro", 4)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "cinco", 5)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "seis", 6)

#converting to number and if it's not a number: NA.
ads_data$ambientes <- as.numeric(ads_data$ambientes) 

#now look in the description for those that were NA before.
ads_data$ambientes <- ifelse(is.na(ads_data$ambientes), word(sub("\\ amb.*", "", ads_data$description),-1), ads_data$ambientes)

#replacing words for numbers, only for values that make sense.
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "dos", 2)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "tres", 3)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "cuatro", 4)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "cinco", 5)
ads_data$ambientes  <- replace(ads_data$ambientes, tolower(ads_data$ambientes) == "seis", 6)

#converting to number and if it's not a number or it does not makes sense: NA.
ads_data$ambientes <- as.numeric(ads_data$ambientes)
ads_data$ambientes[ads_data$ambientes > 6] <- NA
ads_data$ambientes[ads_data$ambientes < 1] <- NA
ads_data$ambientes[ads_data$ambientes %% 1 != 0] <- NA #keeping only integers

#we use the same method to fill NAs in the bedroom ('dormitorio in spanish') variable.
ads_data$description_bed <- word(sub("\\ dormitorio.*", "", ads_data$description),-1)

#replacing words for numbers, only for values that make sense.
ads_data$description_bed <- replace(ads_data$description_bed, tolower(ads_data$description_bed) == "dos", 2)
ads_data$description_bed <- replace(ads_data$description_bed, tolower(ads_data$description_bed) == "tres", 3)
ads_data$description_bed <- replace(ads_data$description_bed, tolower(ads_data$description_bed) == "cuatro", 4)
ads_data$description_bed <- replace(ads_data$description_bed, tolower(ads_data$description_bed) == "cinco", 5)
ads_data$description_bed <- replace(ads_data$description_bed, tolower(ads_data$description_bed) == "seis", 6)

#converting to number and if it's not a number or it does not makes sense: NA.
ads_data$description_bed <- as.numeric(ads_data$description_bed)
ads_data$description_bed[ads_data$description_bed > 6] <- NA
ads_data$description_bed[ads_data$description_bed < 1] <- NA
ads_data$bedrooms[ads_data$bedrooms %% 1 != 0] <- NA  #keeping only integers

#replacing rows with NA in bedroom with the values found
es_na = is.na(ads_data$bedrooms)
ads_data$bedrooms[es_na] <- ads_data[es_na]$description_bed

#removing the created column for that purpose
ads_data$description_bed <- NULL

#repeating the process for the amount of bathrooms
ads_data$description_bath <- word(sub("\\ baño.*", "", ads_data$description),-1)

#replacing words for numbers, only for values that make sense.
ads_data$description_bath <- replace(ads_data$description_bath, tolower(ads_data$description_bath) == "un", 1)
ads_data$description_bath <- replace(ads_data$description_bath, tolower(ads_data$description_bath) == "dos", 2)
ads_data$description_bath <- replace(ads_data$description_bath, tolower(ads_data$description_bath) == "tres", 3)
ads_data$description_bath <- replace(ads_data$description_bath, tolower(ads_data$description_bath) == "cuatro", 4)
ads_data$description_bath <- replace(ads_data$description_bath, tolower(ads_data$description_bath) == "cinco", 5)

#converting to number and if it's not a number or it does not makes sense: NA.
ads_data$description_bath <- as.numeric(ads_data$description_bath)
ads_data$description_bath[ads_data$description_bath > 5] <- NA
ads_data$description_bath[ads_data$description_bath < 1] <- NA
ads_data$bathrooms[ads_data$bathrooms %% 1 != 0] <- NA

#replacing rows with NA in bedroom with the values found
es_na_bath = is.na(ads_data$bathrooms)
ads_data$bathrooms[es_na_bath] <- ads_data[es_na_bath]$description_bath

#removing the new column created.
ads_data$description_bath <- NULL

#replacing room, bedroom and rooms (ambientes) for  1, if we found the word 'monoambiente' in the description.
#monoambiente means a propoerty with only one room and one bathroom.
ads_data$monoambiente <- grepl("monoambiente", tolower(ads_data$description))
mono <- ads_data$monoambiente == TRUE
ads_data$bedrooms[mono] <- 1
ads_data$bathrooms[mono] <- 1
ads_data$ambientes[mono] <- 1
unique(ads_data$ambientes)

#calculating prices deciles for a new variable.
quantile(ads_data$price_usd, probs = seq(.1, .9, by = .1), na.rm = TRUE)
ads_data$decile_price <- ntile(ads_data$price_usd, 10)
ads_data$decile_price <- as.factor(ads_data$decile_price) #converting to factor type.

#creating multiple combinations of variables and ratios
ads_data <- ads_data %>% mutate(bed_and_bath = bathrooms + bedrooms,
                                rooms_bed = rooms/bedrooms,
                                rooms_bath = rooms/bathrooms,
                                bed_bath = bedrooms/bathrooms,
                                surfacecovered_surfacetotal = surface_covered/surface_total,
                                surfacecovered_rooms = surface_covered/rooms,
                                price_m2_covered = price_usd/surface_covered,
                                price_m2 = price_usd/surface_total,
                                price_bath = price_usd/bathrooms,
                                price_bedrooms = price_usd/bedrooms,
                                price_bed_and_bath = price_usd/bed_and_bath,
                                price_rooms = price_usd/rooms,
                                price_rooms_bed = price_usd/rooms_bed,
                                price_rooms_bath = price_usd/rooms_bath,
                                price_bed_bath = price_usd / bed_bath,
                                price_ambientes = price_usd / ambientes,
                                title_price = title_lenght /price_usd,
                                desc_price = desc_lenght / price_usd)

ratios = c("bed_bath", "bed_and_bath", "rooms_bed","surfacecovered_surfacetotal", "surfacecovered_rooms",
          "rooms_bath","price_m2_covered", "price_m2","price_bath",
           "price_bedrooms","price_bed_and_bath","price_rooms", "price_rooms_bed", "price_rooms_bath",
           "price_bed_bath",'price_ambientes', 'title_price', 'desc_price')

#fixing infinite values, since some denominators are 0
ads_data[, ratios] <- lapply(ads_data[, ..ratios], function(x) ifelse(is.infinite(x), NA, x))

dplyr::count(ads_data[(is.na(ads_data$bed_bath))], property_type, sort = TRUE)
dplyr::count(ads_data[(is.na(ads_data$ambientes))], property_type, sort = TRUE)



#Using the text to create variables. 
#Finding key words and creating binary variables showing that feature from the property.
#Finally, we only kept the one without comment. ('amoblado' means that it includes furniture)

#ads_data$pileta <- grepl(paste(c("pileta", "piscina"), collapse = "|"), tolower(ads_data$description))
#ads_data$luminoso <- grepl("luminos", tolower(ads_data$description))
#ads_data$vista <- grepl("vista", tolower(ads_data$description))
ads_data$amoblado <- grepl("amoblad", tolower(ads_data$description)) #Conservamos solo esta
#ads_data$amplio <- grepl("amplio", tolower(ads_data$description))
#ads_data$alfrente <- grepl("al frente", tolower(ads_data$description))
#ads_data$balcon <- grepl("balcon", chartr("ÁÉÍÓÚ", "AEIOU", tolower(ads_data$description)))
#ads_data$aire_acondicionado <- grepl("aire acondicionado", tolower(ads_data$description))


#converting date to numeric. it could be usefull to make predictions.
ads_data$created_on <- as.numeric(ads_data$created_on)

#-------Using external data sources-------
#finally the model worked better without this section. see the code in coments below to see the process.

#distance from each property to the metro stations
#we found a dataset with latitude and longitude for the metro stations in buenos aires
#subte_estaciones <- st_read("http://bitsandbricks.github.io/data/subte_estaciones.geojson")

#creating a spatial dataset
#ads_data_subte <- ads_data %>% filter(!is.na(lat)) %>% filter(!is.na(lon)) %>%
  #st_as_sf(coords = c("lon", "lat"), crs = 4326)
#ads_data_subte <- ads_data_subte %>%
  #mutate(DIST_SUBTE = apply(st_distance(ads_data_subte, subte_estaciones), 1, function(x) min(x)))

#transforming the dataset to a traditional one again.
#ads_data_subte <- ads_data_subte %>% mutate(lat = unlist(map(ads_data_subte$geometry,1)), lon = unlist(map(ads_data_subte$geometry,2))) %>%
  #st_set_geometry(NULL)

#combining 
#ads_data <- merge(x = ads_data, y = ads_data_subte[ , c('ad_id', 'DIST_SUBTE')], by= 'ad_id', all.x = TRUE)

#removing distances with the metro stations for cities that are not buenos aires.
#capital <- ads_data$place_l2 == 'Capital Federal'
#ads_data$DIST_SUBTE[!capital] <- NA
#rm(ads_data_subte)


#using the mobility google file as an external source, joining by country and date.
#mobility <- readRDS("mobility.RDS")
#mobility$country_region <- ifelse(mobility$country_region == "Peru", "Perú", mobility$country_region)
#mobility$country_region <- as.factor(mobility$country_region)
#mobility$date <- strftime(mobility$date, "%Y-%m-%d", tz="UTC")
#mobility <- as.data.table(mobility)
#ads_data$date <- strftime(ads_data$created_on, "%Y-%m-%d", tz="UTC")
#ads_data <- merge(ads_data,mobility, by.x = c("place_l1", "date"), by.y = c("country_region", "date"), all.x = TRUE)

#using k means to create a cluster variable. using latitude, longitude and price
#train_data <- ads_data[ads_data$train_val_eval == "train"]
#kmeans_columns_to_keep <- c('price_usd, latitud, longitud')
#train_data_clusters <- train_data[, ..kmeans_columns_to_keep]
#train_data_clusters <- train_data_clusters[complete.cases(train_data_clusters)]

#Escalamos y eliminamos outliers
#removing outliers and modifying the scale.
#train_data_clusters <- as.data.frame(sapply(train_data_clusters, function(data) (abs(data-mean(data))/sd(data))))    
#train_data_clusters <- train_data_clusters[!rowSums(train_data_clusters>4),]

#kmeans_results <- find_k_means(train_data_clusters,3,20)
#kmeans_results <- readRDS("kmeans_result.RDS")

#plot(c(1:18), kmeans_results$var, type="o", xlab="# Clusters", ylab="tot.withinss")

#training KMeans
#clusters_model <- kmeans(train_data_clusters, centers=9, iter.max=3000, nstart=10)

#saveRDS(train_data,"train_data_clusters.RDS")
#train_data <- readRDS("train_data_clusters.RDS")
#train_data$cluster <- factor(cl_predict(clusters_model, train_data[, ..kmeans_columns_to_keep]))

#train_data %>% group_by(cluster) %>% summarise(prom_contactos = mean(contacts, na.rm=TRUE),
                                               #cant_obs = n(),
                                               #cant_obs_pct = n()/nrow(.))

#predicting clusters for the entire dataframe,
#ads_data$cluster <- factor(cl_predict(clusters_model, ads_data[, ..kmeans_columns_to_keep]))

#-----------------------------------------------------------------------------


##~~~~~~~~~~~~~ one_hot_encoding y converting to sparse matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_val_eval <- ads_data$train_val_eval
cols_to_delete <- c("ad_id", "title", "description", "short_description", "development_name","train_val_eval")
columns_to_keep <- setdiff(names(ads_data), cols_to_delete)

ads_data_sparse <- one_hot_sparse(ads_data[, ..columns_to_keep])
gc()
saveRDS(ads_data_sparse,"ads_data_sparse.RDS") 
#ads_data_sparse <- readRDS("ads_data_sparse.RDS")


#using bag of words text analyisis to create more variables for words in the title and description.
gen_bow_vars <- function(char_vector, var_name, min_wordLengths, min_bound, remove_stopwords) {
  
  corpus <- VCorpus(VectorSource(char_vector))
  dt.mat <- DocumentTermMatrix(corpus,
                               control = list(stopwords = remove_stopwords,
                                              tolower=TRUE,
                                              removePunctuation = TRUE,
                                              wordLengths = c(min_wordLengths, Inf),
                                              bounds = list(global=c(min_bound, 500000))))
  
  var_names <- paste(var_name, colnames(dt.mat), sep="_")
  dt.mat <- sparseMatrix(i=dt.mat$i, j=dt.mat$j, x=dt.mat$v, dims = dim(dt.mat))
  colnames(dt.mat) <- var_names
  return(dt.mat)
}
dim(ads_data[ads_data$contacts == 0])
dim(ads_data)
#bag of words for the title
bow_comments <- gen_bow_vars(ads_data$title, "title", 2, 50, TRUE)
#Bag of words for the description
bow_comments_desc <- gen_bow_vars(ads_data$description, "descrip", 2, 50, TRUE)


#joining sparse matrices
ads_data_sparse <- cbind(ads_data_sparse, bow_comments)
#saveRDS(ads_data_sparse,"ads_data_sparse+bow_tit.RDS")
ads_data_sparse <- cbind(ads_data_sparse, bow_comments_desc)
#saveRDS(ads_data_sparse,"ads_data_sparse+bow_tit&desc.RDS")

#we will use the one with both title and description bag of words.
#ads_data_sparse <- readRDS("ads_data_sparse+bow_tit&desc.RDS")


##~~~~~~~~~~~~~ training xgboost model with hyperparameter optimization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dtrain <- xgb.DMatrix(data = ads_data_sparse[train_val_eval == "train", colnames(ads_data_sparse) != "contacts"],
                      label = ads_data_sparse[train_val_eval == "train", colnames(ads_data_sparse) == "contacts"])

dvalid <- xgb.DMatrix(data = ads_data_sparse[train_val_eval == "valid", colnames(ads_data_sparse) != "contacts"],
                      label = ads_data_sparse[train_val_eval == "valid", colnames(ads_data_sparse) == "contacts"])

rgrid <- random_grid(size = 10,
                     min_nrounds = 200, max_nrounds = 350, 
                     min_max_depth = 12, max_max_depth = 15, 
                     min_eta = 0.01, max_eta = 0.1, 
                     min_gamma = 2, max_gamma = 5, 
                     min_min_child_weight = 8, max_min_child_weight = 15,
                     min_colsample_bytree = 0.5, max_colsample_bytree = 0.8,
                     min_subsample = 0.4, max_subsample = 0.8) 

predicted_models <- train_xgboost(dtrain, dvalid, rgrid)

#saveRDS(predicted_models, "predicted_models")
#predicted_models <- readRDS("predicted_models.RDS")

#saving results in a dataframe
res_table <- result_table(predicted_models)
print(res_table)

#we keep the best model
best_model <- predicted_models[[res_table[1,"i"]]]$model

#analazyng variables with better predictive performance
importance_matrix = xgb.importance(colnames(dtrain), model = best_model)
xgb.plot.importance(importance_matrix[1:30,])
View(head(xgb.importance(model=best_model), 500))

##~~~~~~~~~~~~~ training with train + validation to predict eval~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dall <- xgb.DMatrix(data = ads_data_sparse[train_val_eval != "eval", colnames(ads_data_sparse) != "contacts"],
                    label = ads_data_sparse[train_val_eval != "eval", colnames(ads_data_sparse) == "contacts"])
final_model <- xgb.train(data = dall,
                         nrounds = res_table[1, "nrounds"],
                         params=as.list(res_table[1, c("max_depth",
                                                   "eta",
                                                   "gamma",
                                                   "colsample_bytree",
                                                   "subsample",
                                                   "min_child_weight")]),
                         watchlist = list(train = dall),
                         objective = "reg:squaredlogerror",
                         feval = rmsle,
                         print_every_n = 10)

#predicting and saving submission
preds <- data.frame(ad_id = ads_data[train_val_eval == "eval", "ad_id"],
                    contacts = predict(final_model,
                                       ads_data_sparse[train_val_eval == "eval", colnames(ads_data_sparse) != "contacts"]))
preds$contacts <- pmax(preds$contacts, 0)


write.table(preds, "submission_17.csv", sep=",", row.names=FALSE, quote=FALSE)

