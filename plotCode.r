w1_plottingData <- data.frame(
  period = rep("Wave 1 (Original Dataset)",4)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features")
  ,roc_auc_score = c(0.921806855,0.778974529,0.921806855,0.778974529)
)

w2_plottingData <- data.frame(
  period = rep("Wave 2 (Sep-Nov 2020)",4)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features")
  ,roc_auc_score = c(0.812652552,0.679011799,0.814039379,0.732406375)
)

w3_plottingData <- data.frame(
  period = rep("Wave 3 (Dec-Jan 2020/21)",4)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features")
  ,roc_auc_score = c(0.7994211243501196,0.676489397389172,0.7996498376252137,0.7344011525084371)
)

w4_plottingData <- data.frame(
  period = rep("Wave 4 (Aug-Sep 2021)",4)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features")
  ,roc_auc_score = c(0.6378048474735774,0.5840622672200418,0.6378518863955038,0.6010853273909755)
)

w5_plottingData <- data.frame(
  period = rep("Wave 5 (Jan-Feb 2022",4)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features")
  ,roc_auc_score = c(0.4776838523671778,0.46617360807291075,0.499940322490221,0.4723540407498081)
)

library(dplyr)
masterPlot <- rbind(
  w1_plottingData, w2_plottingData, w3_plottingData, w4_plottingData, w5_plottingData
) 
masterPlot$period
masterPlot2 <- masterPlot %>% 
  mutate(model = factor(model, levels = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"),ordered = TRUE)) %>% 
  mutate(roc_auc_score2 = round(roc_auc_score,3))

library(ggplot2)
# 
# ggplot(masterPlot,aes(y = roc_auc_score,x = period, fill = model)) + 
#   geom_col(position = "dodge") + 
#   geom_hline(yintercept = 0.5, color = "red") +
#   xlab("Data Period") +
#   ylab("Area under ROC Score") +
#   ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel")

ggplot(masterPlot2,aes(y = roc_auc_score,x = period, color = model, group = model)) + 
  geom_line() +
  geom_hline(yintercept = 0.5, color = "red") +
  theme(legend.text = element_text(size = 12),axis.text = element_text(size = 12),axis.title =  element_text(size = 14), title = element_text(size = 16)) +
  xlab("Data Period") +
  ylab("Area under ROC Score") +
  ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel")

ggplot(masterPlot2,aes(y = roc_auc_score,x = period, fill = model)) + 
  geom_col(position = "dodge") + 
  geom_hline(yintercept = 0.5, color = "red") +
  geom_text(aes(label = roc_auc_score2), nudge_y = 0.025) +
  theme(legend.text = element_text(size = 12),axis.text = element_text(size = 12),axis.title =  element_text(size = 14), title = element_text(size = 16)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  facet_wrap(vars(model)) +
  xlab("Data Period") +
  ylab("Area under ROC Score") +
  ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(masterPlot2,aes(y = roc_auc_score,x = period, color = model, group = model)) + 
#   geom_line() +
#   geom_hline(yintercept = 0.5, color = "red") +
#   geom_text(aes(label = roc_auc_score2), nudge_y = 0.025, hjust = 0, angle = 25) +
#   facet_wrap(vars(model)) +
#   xlab("Data Period") +
#   ylab("Area under ROC Score") +
#   ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Part 2 ----

w1_plottingData <- data.frame(
  period = rep("Wave 1 (Original Dataset)",8)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
             ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
             )
  ,roc_auc_score = c(0.921806855,0.778974529,0.921806855,0.778974529,0.901836799,0.874074209,0.901836799,0.874074209)
)

w2_plottingData <- data.frame(
  period = rep("Wave 2 (Sep-Nov 2020)",8)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
             ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
  )
  ,roc_auc_score = c(0.812652552,0.679011799,0.814039379,0.732406375,0.817801963,0.792381512,0.818878715,0.794219032)
)

w3_plottingData <- data.frame(
  period = rep("Wave 3 (Dec-Jan 2020/21)",8)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
             ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
  )
  ,roc_auc_score = c(0.7994211243501196,0.676489397389172,0.7996498376252137,0.7344011525084371,0.798749225,0.769252963,0.803719483,0.774206725)
)

w4_plottingData <- data.frame(
  period = rep("Wave 4 (Aug-Sep 2021)",8)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
             ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
  )
  ,roc_auc_score = c(0.6378048474735774,0.5840622672200418,0.6378518863955038,0.6010853273909755,0.798749225,0.769252963,0.691913874,0.674525258)
)

w5_plottingData <- data.frame(
  period = rep("Wave 5 (Jan-Feb 2022",8)
  ,model = c("Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
             ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
  )
  ,roc_auc_score = c(0.4776838523671778,0.46617360807291075,0.499940322490221,0.4723540407498081,0.499952732,0.497559104,0.594864806,0.590638981)
)



masterPlot <- rbind(
  w1_plottingData, w2_plottingData, w3_plottingData, w4_plottingData, w5_plottingData
) 

masterPlot2 <- masterPlot %>% 
  mutate(model = factor(model, levels = c(
    "Original Model, 8 features","Original Model, 5 features","Original Model refit, 8 features","Original Model refit, 5 features"
    ,"Random forest original, 8 features","Random forest original, 5 features","Random forest refit, 8 features","Random forest refit, 5 features"
    ),ordered = TRUE)) %>% 
  mutate(roc_auc_score2 = round(roc_auc_score,3))

library(ggplot2)
# 
# ggplot(masterPlot,aes(y = roc_auc_score,x = period, fill = model)) + 
#   geom_col(position = "dodge") + 
#   geom_hline(yintercept = 0.5, color = "red") +
#   xlab("Data Period") +
#   ylab("Area under ROC Score") +
#   ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel")

ggplot(masterPlot2,aes(y = roc_auc_score,x = period, color = model, group = model)) + 
  geom_line() +
  geom_hline(yintercept = 0.5, color = "red") +
  theme(legend.text = element_text(size = 12),axis.text = element_text(size = 12),axis.title =  element_text(size = 14), title = element_text(size = 16)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  xlab("Data Period") +
  ylab("Area under ROC Score") +
  ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel")

ggplot(masterPlot2,aes(y = roc_auc_score,x = period, fill = model)) + 
  geom_col(position = "dodge") + 
  geom_hline(yintercept = 0.5, color = "red") +
  geom_text(aes(label = roc_auc_score2), nudge_y = 0.025) +
  facet_wrap(vars(model)) +
  theme(legend.text = element_text(size = 12),axis.text = element_text(size = 12),axis.title =  element_text(size = 14), title = element_text(size = 16)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +  xlab("Data Period") +
  ylab("Area under ROC Score") +
  ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(masterPlot2,aes(y = roc_auc_score,x = period, color = model, group = model)) + 
#   geom_line() +
#   geom_hline(yintercept = 0.5, color = "red") +
#   geom_text(aes(label = roc_auc_score2), nudge_y = 0.025, hjust = 0, angle = 25) +
#   facet_wrap(vars(model)) +
#   xlab("Data Period") +
#   ylab("Area under ROC Score") +
#   ggtitle("Model Performance (roc_auc_score) over Covid Pandemic Waves in Israel") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))