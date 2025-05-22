library(stringr)
library(anytime)
library(dplyr)
library(tidyr)
library(readr)

rawResultsRootPath = "../../experiment-results";
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

analyze <- function(data) {
  data %>%
    summarise(
      "mean(elapsed) [ms]" = mean(elapsed, na.rm = T), 
      "median(elapsed) [ms]" = median(elapsed, na.rm = T), 
      "90th_percentile(elapsed) [ms]" = quantile(elapsed, probs=0.9, na.rm=T),
      "mean(latency) [ms]" = mean(Latency, na.rm = T), 
      "median(latency) [ms]" = median(Latency, na.rm = T), 
      "90th_percentile(latency) [ms]" = quantile(Latency, probs=0.9, na.rm=T),
      "mean(receivedBytes) [byte]" = mean(bytes, na.rm = T), 
      "median(receivedBytes) [byte]" = median(bytes, na.rm = T), 
      "90th_percentile(receivedBytes) [byte]" = quantile(bytes, probs=0.9, na.rm=T),
      "mean(sentBytes) [byte]" = mean(sentBytes, na.rm = T), 
      "median(sentBytes) [byte]" = median(sentBytes, na.rm = T), 
      "90th_percentile(sentBytes) [byte]" = quantile(sentBytes, probs=0.9, na.rm=T),
      "success_rate [%]" = sum(success == "true") / n(),
      "duration" = (max(timeStamp + elapsed, na.rm=T) - min(timeStamp, na.rm=T)) / 1000,
      "requests" = n(),
      "start" = min(timestamp()),
      "throughput [requests / s]" = n() / ((max(timeStamp + elapsed, na.rm=T) - min(timeStamp, na.rm=T)) / 1000),
    )
}

for (results in resultFolders) {
  parentScope = parent.frame()
  
  fullPath <- paste(rawResultsRootPath, results, sep="")
  
  variationName <- tail(str_split_1(fullPath, "/"), n=1);
  
  assign(variationName, data.frame(), envir=parentScope)
  assign(paste(variationName, "-raw"), data.frame(), envir=parentScope)
  
  runs <- list.files(path=fullPath, pattern="\\.csv$", full.names = TRUE)
  
  lapply(runs, function(run) {
    print(run)
    # use filename without file ending as identifier
    runId <- gsub('(.*)\\.(.*)', '\\1', tail(str_split_1(run, "/"), n=1));
    
    # ignore warmup data
    if (!str_detect(runId, "warmup")) {
      
      # data cleansing
      runData <- read.csv2(run, sep=",");
      runData <- runData %>% 
        filter(!str_detect(label, "-[:digit:]*$")) %>% #remove sub-spans of requests that can be identified by a -0, -1, ... suffix
        mutate(timestamp_iso = anytime(timeStamp/1000), runId = runId) %>%
        mutate(
          runId = as.factor(runId),
          label = as.factor(label),
          responseCode = as.factor(responseCode),
          success = as.factor(success),
          threadName = as.factor(threadName),
          dataType = as.factor(dataType),
        )
      
      assign(paste(variationName, "-raw", sep=""), rbind(get(paste(variationName, "-raw"), envir = parentScope), runData), envir=parentScope)
      
      
      # TODO outlier removal?
      if (FALSE) {
        Q <- quantile(runData$elapsed, probs=c(.9), na.rm = FALSE)
        runData <- runData %>% filter(elapsed < Q[1])
      }
      
      
      # data aggregation overall
      overall <- runData %>% 
        group_by(runId) %>% 
        analyze %>% 
        mutate(label = "Overall", entityType="system", architectureVariation=variationName) %>% 
        filter(requests >= 100) #filter out aggregations where there are less than 100 requests included to avoid skewed aggregations
      
      # data aggregation per request trace
      perRequestTrace <- runData %>%
        group_by(runId,label) %>%
        analyze %>% 
        mutate(entityType = "requestTrace", duration = NA, "throughput [requests / s]" = NA, architectureVariation=variationName) %>% 
        filter(requests >= 100) #filter out aggregations where there are less than 100 requests included to avoid skewed aggregations
      
      runData <- rbind(overall, perRequestTrace) %>% mutate(loadLevel = str_match(runId, "([0-9]{8}_[0-9]{6}_)([0-9]*)(req.*)")[,3])
      
      assign(variationName, rbind(get(variationName, envir = parentScope), runData), envir=parentScope)
      
    }
  })
}


outputPath = "./results"

for (results in resultFolders) {
  
  # create directories if necessary
  dirPath <- paste(outputPath, str_split_1(results, "/")[[2]], sep="/");
  ifelse(!dir.exists(file.path(dirPath)), dir.create(file.path(dirPath), recursive = TRUE), FALSE)
  
  filePath <- paste(outputPath, results, ".csv", sep="");
  variationName <- tail(str_split_1(results, "/"), n=1);
  
  write.csv(get(variationName, envir=parent.frame()), filePath, row.names = FALSE)
}
