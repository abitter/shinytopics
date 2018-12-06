# Trends for range input
########################


#von <- input$range[1]-1979
#bis <- input$range[2]-1979

# trends.ab(von, bis)[[1]] # Hot Topics
# trends.ab(von, bis)[[2]] # Cold Topics
# trends.ab(von, bis)[[3]] # hot_ts for plot
# trends.ab(von, bis)[[4]] # cold_ts for plot

trends.ab <- function(von, bis, 
                      theta_year, theta_mean_by_year, 
                      theta_mean_by_year_time, 
                      theta_mean_by_year_ts, years){
  
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
  
  terms_hot <- as.data.frame(colnames(theta_year[,topics_hot[1:10]]))
  terms_hot$NR <- row.names(terms_hot)
  terms_hot[ ,c(1,2)] <- terms_hot[ ,c(2,1)]
  names(terms_hot) <- c("NR", "Wahrscheinlichste Begriffe")
  
  terms_cold <- as.data.frame(colnames(theta_year[,topics_cold[1:10]]))
  terms_cold$NR <- row.names(terms_cold)
  terms_cold[ ,c(1,2)] <- terms_cold[ ,c(2,1)]
  names(terms_cold) <- c("NR", "Wahrscheinlichste Begriffe")
  
  results <- list()
  results[1] <- list(terms_hot)
  results[2] <- list(terms_cold)
  results[3] <- list(hot_ts)
  results[4] <- list(cold_ts)
  
  return(results)
  
}
