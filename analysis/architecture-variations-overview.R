library(stringr)
library(anytime)
library(dplyr)
library(tidyr)
library(readr)
library(kableExtra)
library(ggplot2)
library(modelsummary)
library(gridExtra)
library(webshot2)

resultsPath = "./results"
resultFolders = c("/original/teastore-private-nlb-original"
                  ,"/serviceReplication/teastore-private-nlb-highreplication"
                  ,"/serviceReplication/teastore-private-nlb-lowreplication"
                  ,"/serviceReplication/teastore-private-nlb-mixedreplication"
                  ,"/serviceReplication/teastore-private-nlb-withfailures-noreplication"
                  ,"/serviceReplication/teastore-private-nlb-withfailures-lowreplication"
                  ,"/serviceReplication/teastore-private-nlb-withfailures-mixedreplication"
                  ,"/serviceReplication/teastore-private-nlb-withfailures-highreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-single-noreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-single-highreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-two-noreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-two-highreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-three-noreplication"
                  ,"/horizontalDataReplication/teastore-private-nlb-rds-three-highreplication"
                  ,"/verticalReplication/teastore-private-nlb-noreplication-nocaching"
                  ,"/verticalReplication/teastore-private-nlb-noreplication-withcaching"
                  ,"/verticalReplication/teastore-private-nlb-noreplication-withmorecaching"
                  ,"/verticalReplication/teastore-private-nlb-withfailures-nocaching"
                  ,"/verticalReplication/teastore-private-nlb-withfailures-withcaching"
                  ,"/verticalReplication/teastore-private-nlb-withfailures-withmorecaching"
);

getRequestTraceNameFromMeasures <- function(nameFromExperiment) {
  return(
    switch(nameFromExperiment, 
           "Add Product 2 to Cart"="Add Product To Cart",
           "Add Product to Cart"="Add Product To Cart",
           "Home"="Index Page",
           "List Products"="Show Category",
           "List Products with different page"="Show Category",
           "Login"="User Login",
           "Logout"="User Logout",             
           "Look at Product"="Product Page",
           "n/a"
    )
  )
}

architecturesOverview <- data.frame();

# load data
for (result in resultFolders) {
  fullPath <- paste(resultsPath, result, sep="");
  
  variationName <- tail(str_split_1(result, "/"), n=1);
  transformedName <- gsub("-(.)", "\\U\\1", variationName, perl = TRUE)  # Capitalize letters following hyphens
  transformedName <- gsub("-", "", transformedName)  # Remove hyphens
  
  measures <- read.csv2(paste(resultsPath, result, "-model-measures.csv", sep=""))
  systemMeasures <- measures %>% 
    mutate("variation" = variationName, "entityName" = "teaStore") %>%
    filter(entityType == "system") %>% 
    filter(measureKey %in% c("serviceReplicationLevel", "storageReplicationLevel", "ratioOfCachedDataAggregates")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  architecturesOverview <- dplyr::bind_rows(architecturesOverview, systemMeasures)
  
  serviceMeasures <- measures %>% 
    mutate("variation" = variationName) %>%
    filter(entityType == "component") %>% 
    filter(entityName %in% c("Recommender", "Imageprovider", "Webui", "Persistence", "Auth")) %>%
    mutate("entityType" = "service") %>%
    filter(measureKey %in% c("serviceReplicationLevel", "ratioOfCachedDataAggregates")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  architecturesOverview <- dplyr::bind_rows(architecturesOverview, serviceMeasures)
  
  storageBackingServiceMeasures <- measures %>% 
    mutate("variation" = variationName) %>%
    filter(entityType == "component") %>% 
    filter(entityName %in% c("DB")) %>%
    mutate("entityType" = "sbs") %>%
    filter(measureKey %in% c("storageReplicationLevel")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  architecturesOverview <- dplyr::bind_rows(architecturesOverview, storageBackingServiceMeasures)
  
  requestTraceMeasures <- measures %>% 
    mutate("variation" = variationName) %>%
    filter(entityType == "requestTrace") %>% 
    filter(entityName %in% c("Index Page", "Show Category", "Product Page", "Add Product To Cart", "User Login", "User Logout")) %>%
    filter(measureKey %in% c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "storageReplicationLevel")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  architecturesOverview <- dplyr::bind_rows(architecturesOverview, requestTraceMeasures)
}

#transform data 

architecturesOverviewTable <- architecturesOverview %>% 
  mutate(variation = str_remove(variation, "teastore-private-nlb-")) %>%
  mutate_at(c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "storageReplicationLevel","ratioOfCachedDataAggregates"), as.numeric) %>%
  mutate_if(is.numeric, round, 3) %>%
  pivot_wider(names_from = variation, values_from = c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "storageReplicationLevel","ratioOfCachedDataAggregates"))
 
write.csv(architecturesOverviewTable, "architecture-variations.csv")

summary(architecturesOverviewTable)







