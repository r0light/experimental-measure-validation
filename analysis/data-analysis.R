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


loadData <- function(path, subPath) {
  return(read.csv2(paste(path,subPath,".csv", sep=""), sep=",", check.names = FALSE))
}

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
           #"Add Product 2 to Cart - discrete"="Add Product To Cart",
           #"Add Product to Cart - discrete"="Add Product To Cart",
           #"Home - discrete"="Index Page",
           #"List Products - discrete"="Show Category",
           #"List Products with different page - discrete"="Show Category",
           #"Login - discrete"="User Login",
           #"Logout - discrete"="User Logout",             
           #"Look at Product - discrete"="Product Page",
           "n/a"
    )
  )
}


coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}


addMeasures <- function(resultSet, resultsPath, subPath) {
  measures <- read.csv2(paste(resultsPath, subPath, "-model-measures.csv", sep=""))
  systemMeasures <- measures %>% 
    filter(entityType == "system") %>% 
    filter(measureKey %in% c("serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel", "ratioOfCachedDataAggregates", "")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  requestTraceMeasures <- measures %>% 
    filter(entityType == "requestTrace") %>% 
    filter(measureKey %in% c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel")) %>% 
    select (-c(measureName, systemName, entityId)) %>% 
    pivot_wider(
      names_from = measureKey, 
      values_from = value
    )
  
  resultSet <- resultSet %>% left_join(systemMeasures, by=join_by(entityType == entityType))
  resultSet <- resultSet %>% mutate(requestTraceName = unlist(lapply(label, getRequestTraceNameFromMeasures))) %>% coalesce_join(requestTraceMeasures, by=join_by(requestTraceName == entityName), join=dplyr::left_join)
  resultSet <- resultSet %>% mutate("entityName" = coalesce(entityName, requestTraceName)) %>% relocate("entityName", .after="entityType") %>% select (-c("requestTraceName"))
  
  resultSet <- resultSet %>% mutate_at(c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel"), as.character) %>%
    mutate_at(c("dataReplicationAlongRequestTrace", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel"), as.numeric)
  return(resultSet)
}

combinedLoad <- function(path) {
  rawResultSet <- loadData(resultsPath, path)
  rawResultSet <- rawResultSet %>% select (-c("mean(latency) [ms]", "median(latency) [ms]", "90th_percentile(latency) [ms]", "mean(receivedBytes) [byte]", "median(receivedBytes) [byte]", "90th_percentile(receivedBytes) [byte]", "mean(sentBytes) [byte]")) %>%
    filter(!str_detect(label, "- discrete")) %>%
    relocate("entityType", .before="mean(elapsed) [ms]") %>%
    relocate("label", .before="entityType") %>%
    relocate("architectureVariation", .before="label" )
  
  rawResultSet["success_rate [%]"] <- as.numeric(unlist(rawResultSet["success_rate [%]"])) * 100
  
  # TODO find a better, more generic solution
  rawResultSet["mean(elapsed) [ms]"] <- as.numeric(unlist(rawResultSet["mean(elapsed) [ms]"]))
  rawResultSet["90th_percentile(elapsed) [ms]"] <- as.numeric(unlist(rawResultSet["90th_percentile(elapsed) [ms]"]))
  rawResultSet["throughput [requests / s]"] <- as.numeric(unlist(rawResultSet["throughput [requests / s]"]))
  
  combinedResultSet <- addMeasures( rawResultSet, resultsPath, path )
  
  combinedResultSet <- combinedResultSet %>%
    mutate(architectureVariation=str_remove(architectureVariation, "teastore-private-nlb-")) %>% 
    mutate(entityName = ifelse(entityType == "system", "teaStore", entityName))
  
  combinedResultSet["serviceReplicationLevel"] <- as.numeric(unlist(combinedResultSet["serviceReplicationLevel"]))
  combinedResultSet["medianServiceReplication"] <- as.numeric(unlist(combinedResultSet["medianServiceReplication"]))
  combinedResultSet["smallestReplicationValue"] <- as.numeric(unlist(combinedResultSet["smallestReplicationValue"]))
  combinedResultSet["storageReplicationLevel"] <- as.numeric(unlist(combinedResultSet["storageReplicationLevel"]))
  combinedResultSet["ratioOfCachedDataAggregates"] <- as.numeric(unlist(combinedResultSet["ratioOfCachedDataAggregates"]))
  combinedResultSet["dataReplicationAlongRequestTrace"] <- as.numeric(unlist(combinedResultSet["dataReplicationAlongRequestTrace"]))
  
  combinedResultSet <- combinedResultSet %>% mutate_if(is.numeric, ~round(., 2))
  
  return(combinedResultSet)
}


for (result in resultFolders) {
  fullPath <- paste(resultsPath, result, sep="");

  variationName <- tail(str_split_1(result, "/"), n=1);
  transformedName <- gsub("-(.)", "\\U\\1", variationName, perl = TRUE)  # Capitalize letters following hyphens
  transformedName <- gsub("-", "", transformedName)  # Remove hyphens
  
  assign(transformedName, combinedLoad(result)) # load data
}


# actual analysis

theme_set(theme_bw())  # pre-set the bw theme.
plotResult <- function(data, entityName, xVar, yVar, legendPos) {
  if (legendPos == "tr") {
    legendPosValues <- c(0.7,0.7)
  } else if (legendPos == "br") {
    legendPosValues <- c(0.7,0.25)
  } else if (legendPos == "bl") {
    legendPosValues <- c(0.25,0.25)
  } else if (legendPos == "tl") {
    legendPosValues <- c(0.25,0.7)
  } else {
    legendPosValues <- "left"
  }
  
  ggplot(data %>% filter(entityName == {{entityName}}), aes(x={{xVar}}, y={{yVar}}, colour = architectureVariation, shape = factor(loadLevel))) + 
    geom_point(size=2) +
    labs(color = "Architecture variation", shape ="Load level [req/s]") +
    theme(legend.position = legendPosValues,
          legend.spacing.y = unit(0, "mm"), 
          axis.text = element_text(colour = 1, size = 10),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
}

plotResultWithRegression <- function(data, hypoName, entityName, xVar, xLab, yVar, form, legendPos) {
  if (legendPos == "tr") {
    legendPosValues <- c(0.7,0.75)
  } else if (legendPos == "br") {
    legendPosValues <- c(0.65,0.25)
  } else if (legendPos == "bl") {
    legendPosValues <- c(0.25,0.25)
  } else if (legendPos == "tl") {
    legendPosValues <- c(0.25,0.8)
  } else {
    legendPosValues <- "left"
  }
  
  linearModel = lm(formula(form), data = data %>% filter(entityName == {{entityName}})) # linear regression
  summary(linearModel) #Review the results
  
  textSize <- 16
  legendSize <- 14
  
  ggplot(data %>% filter(entityName == {{entityName}}), aes(x={{xVar}}, y={{yVar}}, colour = architectureVariation, shape = factor(loadLevel))) + 
    geom_point(size=2) +
    labs(title= paste("Results for ", hypoName, " (entity: ", entityName, ")"), color = "Architecture variation", shape ="Load level [req/s]") +
    xlab(xLab) +
    theme(plot.title = element_text(size = 18),
          axis.title = element_text(size = textSize),
          axis.text = element_text(colour = 1, size = textSize),
          legend.position = "inside",
          legend.direction="vertical",
          legend.position.inside = legendPosValues,
          legend.spacing.y = unit(0, "mm"), 
          legend.title = element_text(size = legendSize, face = "bold"),
          legend.text = element_text(size = legendSize),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    guides(shape = guide_legend(
              direction = "horizontal")
    ) + 
    geom_abline(slope = coef(linearModel)[[2]], 
                intercept = coef(linearModel)[["(Intercept)"]],
                linetype="dashed")
  
}

reportLinearModel <- function(data, entityName, form) {
  linearModel = lm(formula(form), data = data %>% filter(entityName == {{entityName}})) # linear regression
  return(msummary(list("Model"=linearModel), output = 'kableExtra',
           stars = TRUE,
           title = paste("Linear regresssion model for the entity: ", entityName), gof_omit = 'IC|Log|RMSE'))
}

# --------------------
# Hypothesis 1
# --------------------

teastorePrivateNlbNoreplication <- teastorePrivateNlbOriginal %>% mutate("architectureVariation" = "noreplication")

investigateServiceReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "storageReplicationLevel", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(serviceReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis1 <- investigateServiceReplicationTimeBehaviour(rbind(teastorePrivateNlbHighreplication,
                                                                teastorePrivateNlbLowreplication,
                                                                teastorePrivateNlbNoreplication,
                                                                teastorePrivateNlbMixedreplication))

plotResultWithRegression(hypothesis1, "H1", "teaStore", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "teaStore", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "Index Page", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "Index Page", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "Show Category", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "Show Category", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "User Login", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "User Login", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "Product Page", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "Product Page", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "Add Product To Cart", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "Add Product To Cart", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "User Logout", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
reportLinearModel(hypothesis1, "User Logout", `90th_percentile(elapsed) [ms]`~serviceReplicationLevel)

plotResultWithRegression(hypothesis1, "H1", "teaStore", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "teaStore", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "Index Page", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "Index Page", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "Show Category", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "Show Category", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "User Login", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "User Login", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "Product Page", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "Product Page", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "Add Product To Cart", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "Add Product To Cart", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

plotResultWithRegression(hypothesis1, "H1", "User Logout", smallestReplicationValue, "Smallest replication level", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~smallestReplicationValue, "tr")
reportLinearModel(hypothesis1, "User Logout", `90th_percentile(elapsed) [ms]`~smallestReplicationValue)

# --------------------
# Hypothesis 2
# --------------------


investigateServiceReplicationAvailability <- function(data) {
  result <- data %>%
    select(-c("mean(elapsed) [ms]","median(elapsed) [ms]","90th_percentile(elapsed) [ms]","median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]","throughput [requests / s]", "storageReplicationLevel", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(serviceReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis2 <- investigateServiceReplicationAvailability(rbind(teastorePrivateNlbWithfailuresHighreplication,
                                                               teastorePrivateNlbWithfailuresLowreplication,
                                                               teastorePrivateNlbWithfailuresNoreplication,
                                                               teastorePrivateNlbWithfailuresMixedreplication))

plotResultWithRegression(hypothesis2, "H2", "teaStore", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "Index Page", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "Show Category", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "User Login", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "Product Page", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "Add Product To Cart", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
plotResultWithRegression(hypothesis2, "H2", "User Logout", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")

# --------------------
# Hypothesis 3
# --------------------

investigateHorizontalReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "ratioOfCachedDataAggregates","dataReplicationAlongRequestTrace" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(storageReplicationLevel, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis3a <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleNoreplication,
                                                                    teastorePrivateNlbRdsTwoNoreplication,
                                                                    teastorePrivateNlbRdsThreeNoreplication))
hypothesis3b <- investigateHorizontalReplicationTimeBehaviour(rbind(teastorePrivateNlbRdsSingleHighreplication,
                                                                     teastorePrivateNlbRdsTwoHighreplication,
                                                                     teastorePrivateNlbRdsThreeHighreplication))

plotResultWithRegression(hypothesis3a, "H3", "teaStore", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3a, "H3", "Index Page", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3a, "H3", "Show Category", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3a, "H3", "User Login", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3a, "H3", "Product Page", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3a, "H3", "Add Product To Cart", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
#plotResultWithRegression(hypothesis3a, "H3", "User Logout", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")

plotResultWithRegression(hypothesis3b, "H3", "teaStore", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3b, "H3", "Index Page", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3b, "H3", "Show Category", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3b, "H3", "User Login", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3b, "H3", "Product Page", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
plotResultWithRegression(hypothesis3b, "H3", "Add Product To Cart", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")
#plotResultWithRegression(hypothesis3b, "H3", "User Logout", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tr")

# --------------------
# Hypothesis 4
# --------------------


investigateVerticalReplicationTimeBehaviour <- function(data) {
  result <- data %>%
    select(-c("success_rate [%]", "median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel")) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(ratioOfCachedDataAggregates,dataReplicationAlongRequestTrace,, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis4 <- investigateVerticalReplicationTimeBehaviour(rbind(teastorePrivateNlbNoreplicationWithcaching,
                                                                 teastorePrivateNlbNoreplicationNocaching,
                                                                 teastorePrivateNlbNoreplicationWithmorecaching,
                                                                 teastorePrivateNlbNoreplication))

plotResultWithRegression(hypothesis4, "H4", "teaStore", ratioOfCachedDataAggregates, "Ratio of cached data aggregates (CD)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~ratioOfCachedDataAggregates, "tr")
plotResultWithRegression(hypothesis4, "H4", "Index Page", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
plotResultWithRegression(hypothesis4, "H4", "Show Category", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
plotResultWithRegression(hypothesis4, "H4", "User Login", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
plotResultWithRegression(hypothesis4, "H4", "Product Page", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
plotResultWithRegression(hypothesis4, "H4", "Add Product To Cart", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
plotResultWithRegression(hypothesis4, "H4", "User Logout", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")

# --------------------
# Hypothesis 5
# --------------------


investigateVerticalReplicationAvailability <- function(data) {
  result <- data %>%
    select(-c("mean(elapsed) [ms]","median(elapsed) [ms]","90th_percentile(elapsed) [ms]","median(sentBytes) [byte]", "90th_percentile(sentBytes) [byte]","throughput [requests / s]", "serviceReplicationLevel", "medianServiceReplication", "smallestReplicationValue", "storageReplicationLevel" )) %>%
    relocate("entityType", "entityName", "label", .before="architectureVariation") %>%
    group_by(entityType, entityName) %>% 
    arrange(ratioOfCachedDataAggregates,dataReplicationAlongRequestTrace,, .by_group=TRUE, .locale = "en")
  return(result)
}

hypothesis5 <- investigateVerticalReplicationAvailability(rbind(teastorePrivateNlbWithfailuresWithcaching,
                                                                teastorePrivateNlbWithfailuresNocaching,
                                                                teastorePrivateNlbWithfailuresWithmorecaching,
                                                                teastorePrivateNlbWithfailuresNoreplication))

plotResultWithRegression(hypothesis5, "H5", "teaStore", ratioOfCachedDataAggregates, "Ratio of cached data aggregates (CD)", `success_rate [%]`, `success_rate [%]`~ratioOfCachedDataAggregates, "br")
plotResultWithRegression(hypothesis5, "H5", "Index Page", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
plotResultWithRegression(hypothesis5, "H5", "Show Category", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
plotResultWithRegression(hypothesis5, "H5", "User Login", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
plotResultWithRegression(hypothesis5, "H5", "Product Page", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
plotResultWithRegression(hypothesis5, "H5", "Add Product To Cart", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
plotResultWithRegression(hypothesis5, "H5", "User Logout", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")



# ----------------------
# Output for paper
# ----------------------

figureHeight <- 6
figureWidth <- 9

# H1: teaStore and Product Page

h1teaStore <- plotResultWithRegression(hypothesis1, "H1", "teaStore", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
h1teaStore <- h1teaStore + scale_colour_discrete(breaks= c("noreplication", "lowreplication","mixedreplication","highreplication"))
h1teaStore <- h1teaStore + labs(y="90th percentile response time [ms]")
ggsave(h1teaStore, 
       filename = "h1teaStore.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

h1productPage <- plotResultWithRegression(hypothesis1, "H1", "Product Page", serviceReplicationLevel, "Service replication level (SeR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~serviceReplicationLevel, "tr")
h1productPage <- h1productPage + scale_colour_discrete(breaks= c("noreplication", "lowreplication","mixedreplication","highreplication"))
h1productPage <- h1productPage + labs(y="90th percentile response time [ms]")
ggsave(h1productPage, 
       filename = "h1productPage.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

linearModel1 = lm(`90th_percentile(elapsed) [ms]`~serviceReplicationLevel, data = hypothesis1 %>% filter(entityName == "teaStore"))
linearModel2 = lm(`90th_percentile(elapsed) [ms]`~serviceReplicationLevel, data = hypothesis1 %>% filter(entityName == "Product Page"))
msummary(list("teaStore"=linearModel1, "Product Page"=linearModel2), output = 'latex_tabular',
         stars = TRUE,
         title = "test", gof_omit = 'IC|Log|RMSE')

# H2: Show Category and Logout

h2showCategory <- plotResultWithRegression(hypothesis2, "H2", "Show Category", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "br")
h2showCategory <- h2showCategory + scale_colour_discrete(breaks= c("withfailures-noreplication", "withfailures-lowreplication","withfailures-mixedreplication","withfailures-highreplication"))
h2showCategory <- h2showCategory + labs(y="Success rate [%]")
ggsave(h2showCategory, 
       filename = "h2showCategory.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

h2login <- plotResultWithRegression(hypothesis2, "H2", "User Login", serviceReplicationLevel, "Service replication level (SeR)", `success_rate [%]`, `success_rate [%]`~serviceReplicationLevel, "tr")
h2login <- h2login + scale_colour_discrete(breaks= c("withfailures-noreplication", "withfailures-lowreplication","withfailures-mixedreplication","withfailures-highreplication"))
h2login <- h2login + labs(y="Success rate [%]")
ggsave(h2login, 
       filename = "h2login.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

linearModel1 = lm(`success_rate [%]`~serviceReplicationLevel, data = hypothesis2 %>% filter(entityName == "Show Category"))
linearModel2 = lm(`success_rate [%]`~serviceReplicationLevel, data = hypothesis2 %>% filter(entityName == "User Login"))
msummary(list("Show Category"=linearModel1, "User Login"=linearModel2), output = 'latex_tabular',
         stars = TRUE,
         title = "test", gof_omit = 'IC|Log|RMSE')

# H3: teaStore low and teaStore high

h3ateaStore <- plotResultWithRegression(hypothesis3a, "H3", "teaStore", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tl")
h3ateaStore <- h3ateaStore + scale_colour_discrete(breaks= c("rds-single-noreplication","rds-two-noreplication","rds-three-noreplication"))
h3ateaStore <- h3ateaStore + labs(y="90th percentile response time [ms]")
ggsave(h3ateaStore, 
       filename = "h3ateaStore.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

h3bteaStore <- plotResultWithRegression(hypothesis3b, "H3", "teaStore", storageReplicationLevel, "Storage replication level (StR)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~storageReplicationLevel, "tl")
h3bteaStore <- h3bteaStore + scale_colour_discrete(breaks= c("rds-single-highreplication","rds-two-highreplication","rds-three-highreplication"))
h3bteaStore <- h3bteaStore + labs(y="90th percentile response time [ms]")
ggsave(h3bteaStore, 
       filename = "h3bteaStore.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

linearModel1 = lm(`90th_percentile(elapsed) [ms]`~storageReplicationLevel, data = hypothesis3a %>% filter(entityName == "teaStore"))
linearModel2 = lm(`90th_percentile(elapsed) [ms]`~storageReplicationLevel, data = hypothesis3b %>% filter(entityName == "teaStore"))
msummary(list("teaStore"=linearModel1, "teaStore"=linearModel2), output = 'latex_tabular',
         stars = TRUE,
         title = "test", gof_omit = 'IC|Log|RMSE')


# H4: teaStore and Product Page

h4teaStore <- plotResultWithRegression(hypothesis4, "H4", "teaStore", ratioOfCachedDataAggregates, "Ratio of cached data aggregates (CD)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~ratioOfCachedDataAggregates, "tr")
h4teaStore <- h4teaStore + scale_colour_discrete(breaks= c("noreplication-nocaching", "noreplication","noreplication-withcaching","noreplication-withmorecaching"))
h4teaStore <- h4teaStore + labs(y="90th percentile response time [ms]")
ggsave(h4teaStore, 
       filename = "h4teaStore.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

h4productPage <- plotResultWithRegression(hypothesis4, "H4", "Product Page", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `90th_percentile(elapsed) [ms]`, `90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, "tr")
h4productPage <- h4productPage + scale_colour_discrete(breaks= c("noreplication-nocaching", "noreplication","noreplication-withcaching","noreplication-withmorecaching"))
h4productPage <- h4productPage + labs(y="90th percentile response time [ms]")
ggsave(h4productPage, 
       filename = "h4productPage.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

linearModel1 = lm(`90th_percentile(elapsed) [ms]`~ratioOfCachedDataAggregates, data = hypothesis4 %>% filter(entityName == "teaStore"))
linearModel2 = lm(`90th_percentile(elapsed) [ms]`~dataReplicationAlongRequestTrace, data = hypothesis4 %>% filter(entityName == "Product Page"))
msummary(list("teaStore"=linearModel1, "Product Page"=linearModel2), output = 'latex_tabular',
         stars = TRUE,
         title = "test", gof_omit = 'IC|Log|RMSE')


# H5: teaStore and Login Page

h5teaStore <- plotResultWithRegression(hypothesis5, "H5", "teaStore", ratioOfCachedDataAggregates, "Ratio of cached data aggregates (CD)", `success_rate [%]`, `success_rate [%]`~ratioOfCachedDataAggregates, "br")
h5teaStore <- h5teaStore + scale_colour_discrete(breaks= c("withfailures-nocaching", "withfailures-noreplication","withfailures-withcaching","withfailures-withmorecaching"))
h5teaStore <- h5teaStore + labs(y="Success rate [%]")
ggsave(h5teaStore, 
       filename = "h5teaStore.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")

h5login<- plotResultWithRegression(hypothesis5, "H5", "User Login", dataReplicationAlongRequestTrace, "Data replication along request trace (RA)", `success_rate [%]`, `success_rate [%]`~dataReplicationAlongRequestTrace, "br")
h5login <- h5login + scale_colour_discrete(breaks= c("withfailures-nocaching", "withfailures-noreplication","withfailures-withcaching","withfailures-withmorecaching"))
h5login <- h5login + labs(y="Success rate [%]")
ggsave(h5login, 
       filename = "h5login.pdf",
       device = "pdf",
       height = figureHeight, width = figureWidth, units = "in")


linearModel1 = lm(`success_rate [%]`~ratioOfCachedDataAggregates, data = hypothesis5 %>% filter(entityName == "teaStore"))
linearModel2 = lm(`success_rate [%]`~dataReplicationAlongRequestTrace, data = hypothesis5 %>% filter(entityName == "User Login"))
msummary(list("teaStore"=linearModel1, "User Login"=linearModel2), output = 'latex_tabular',
         stars = TRUE,
         title = "test", gof_omit = 'IC|Log|RMSE')




