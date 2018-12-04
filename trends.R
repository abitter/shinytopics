# Trends for range input
########################


#theta_year <- theta_mean_by_year
#theta_time <- theta_mean_by_year_time
#theta_ts <- theta_mean_by_year_ts

#saveRDS(theta_year, file = "./shinytopics/data/theta_year.rds")
#saveRDS(theta_time, file = "./shinytopics/data/theta_time.rds")
#saveRDS(theta_ts, file = "./shinytopics/data/theta_ts.rds")
#saveRDS(years, file = "./shinytopics/data/years.rds")

#von <- input$range[1]-1979
#bis <- input$range[2]-1979

# trends.ab(von, bis)[[1]] # Hot Topics
# trends.ab(von, bis)[[2]] # Cold Topics
# trends.ab(von, bis)[[3]] # hot_ts for plot
# trends.ab(von, bis)[[4]] # cold_ts for plot


trends.ab <- function(von, bis){
  #data
  theta_year <- readRDS("data/theta_year.rds")
  theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds")
  theta_mean_by_year_time <- readRDS("data/theta_time.rds")
  theta_mean_by_year_ts <- readRDS("data/theta_ts.rds")
  years <- readRDS("data/years.rds")
  
  #Linear model
  theta_mean_lm <- apply(theta_mean_by_year[von:bis,], 2, function(x) lm(x ~ theta_mean_by_year_time[von:bis])) # 2 is margin for columns
  theta_mean_lm_coef <- lapply(theta_mean_lm,function(x) coef(summary(x)))
  theta_mean_lm_coef_sign <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Pr(>|t|)")
  theta_mean_lm_coef_slope <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Estimate")
  
  # devide in positive and negative slopes
  theta_mean_lm_coef_slope_pos <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope >= 0]
  theta_mean_lm_coef_slope_neg <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope < 0]
  
  # table of topics with positive and negative trends by level of significance
  p_level <- c(0.05, 0.01, 0.001, 0.0001)
  significance_total <- sapply(p_level, 
                               function(x) (theta_mean_lm_coef_sign[theta_mean_lm_coef_sign < x]))
  significance_neg <- sapply(1:length(p_level),
                             function(x) intersect(names(theta_mean_lm_coef_slope_neg),names(significance_total[[x]])))
  significance_pos <- sapply(1:length(p_level),
                             function(x) intersect(names(theta_mean_lm_coef_slope_pos),names(significance_total[[x]])))
  
  ### hot & cold
  topics_hot <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_pos[[1]]], decreasing=TRUE)))
  topics_cold <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_neg[[1]]], decreasing=FALSE)))
  
  hot_ts <- ts(theta_mean_by_year_ts[von:bis,topics_hot[1:10]], start = as.integer(years[von]))
  cold_ts <- ts(theta_mean_by_year_ts[von:bis,topics_cold[1:10]], start = as.integer(years[von]))
  # each plot gets a fixed label, that matches the print output of terms_hot and terms_cold
  colnames(hot_ts) <- c(1:10) 
  colnames(cold_ts) <- c(1:10)
  
  terms_hot <- colnames(theta_year[,topics_hot[1:10]])
  terms_cold <- colnames(theta_year[,topics_cold[1:10]])
  
  results <- list()
  results[1] <- list(terms_hot)
  results[2] <- list(terms_cold)
  results[3] <- list(hot_ts)
  results[4] <- list(cold_ts)
  
  return(results)
  
}
