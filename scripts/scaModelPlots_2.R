# Plotting dataset of shoreline change analysis confidence levels
# Using Monte Carlo simulations of Shoreline Change Analyses

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

projDir <-  'G:/.shortcut-targets-by-id/1avidn3l8UPCyW6_A-M00EH-A2Q3J-f3P/BakerLabDrive/LabProjects/2757RB_RESTORE_LivingShorelines/Data/Shoreline_Data/Shoreline Methods Analyses/SCA_Sim'

setwd(projDir)

#load input error sets
load('Outputs/ModelDF_6.rda')

r100 <- filter(ModelDF2, Replicates == 100)
r1000 <- filter(ModelDF2, Replicates == 1000)


plotDF <- ModelDF2 |>
  arrange(Method, Transects, desc(Rate), Years, desc(Interval)) |>
  mutate(Method = as.factor(Method),
         Rate = as.factor(Rate),
         Quality = ifelse(Upper >=0, "Poor", "Good")) |>
  group_by(Rate, Years, Interval, Method, Transects, Seasonal) |>
  filter(Replicates == max(Replicates)) |>
  ungroup()

#generalized plots ----

plotDF.general_2yr <- plotDF |>
  filter(Interval == 2, Quality == "Good") |>
  mutate(Rate = abs(as.numeric(levels(Rate))[Rate])) |>
  group_by(Transects, Years, Method, Seasonal) |>
  summarize(Rate = min(Rate)) |>
  mutate( Method = gsub("Aerial", "Airplane", Method)) |>
  mutate(Method = factor(Method, levels = c("Airplane Poor", "Airplane Good", "UAS Poor", "UAS Good", "RTK")))

plotDF.general_1yr <- plotDF |>
  filter(Interval == 1, Quality == "Good") |>
  mutate(Rate = abs(as.numeric(levels(Rate))[Rate])) |>
  group_by(Transects, Years, Method, Seasonal) |>
  summarize(Rate = min(Rate)) |>
  mutate( Method = gsub("Aerial", "Airplane", Method)) |>
  mutate(Method = factor(Method, levels = c("Airplane Poor", "Airplane Good", "UAS Poor", "UAS Good", "RTK")))

plotDF.general_3yr <- plotDF |>
  filter(Interval == 3, Quality == "Good") |>
  mutate(Rate = abs(as.numeric(levels(Rate))[Rate])) |>
  group_by(Transects, Years, Method, Seasonal) |>
  summarize(Rate = min(Rate)) |>
  mutate( Method = gsub("Aerial", "Airplane", Method)) |>
  mutate(Method = factor(Method, levels = c("Airplane Poor", "Airplane Good", "UAS Poor", "UAS Good", "RTK")))

plotGeneral <- function(df, t, s, title){
  DF.filt <- filter(df, Transects == t, Seasonal == s)
  minRate <- min(DF.filt$Rate)
  ggplot(DF.filt, 
         aes(x = Years, y = Rate, group = Method, color = Method)) +
    geom_line() +
    geom_jitter(size = 3, width = 0.1, height = 0.1) +
    annotate("label", x = 15, y = 8, 
             label = paste(minRate, "m/yr"), size = 10) +
    ylim(0, 11) +
    xlim(0, 25) +
    theme_bw() +
    # ggtitle(title) +
    xlab("Timespan (yr)") +
    ylab("Shoreline movement rate (m/yr)") +
    theme(plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 10))
}
arrangePlots <- function(df, seasonal = TRUE) {
  p1 <- plotGeneral(df, t = 1, s = FALSE, "1 Transect") +
    guides(color = "none")
  p2 <- plotGeneral(df, t = 30, s = FALSE, "30 Transects") +
    guides(color = "none")
  p3 <- plotGeneral(df, t = 100, s = FALSE, "100 Transects") +
    guides(color = "none")
  p4 <- plotGeneral(df, t = 1, s = TRUE, "1 Transect + Seasonal") +
    guides(color = "none")
  p5 <- plotGeneral(df, t = 30, s = TRUE, "30 Transects + Seasonal") +
    guides(color = "none")
  p6 <- plotGeneral(df, t = 100, s = TRUE, "100 Transects + Seasonal") +
    guides(color = "none")
  
  if(seasonal){
  return(grid.arrange(p1, p2, p3,
                      p4, p5, p6,
                      nrow = 2)) } else {
                        return(grid.arrange(p1, p2, p3, nrow = 1))
                      }
}

generalP.1.ind <- plotGeneral(plotDF.general_2yr, t = 1, s = FALSE, "1 Transect") +
  theme(plot.title = element_text(size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12))
generalP.1.ind

mainDir <- 'G:/.shortcut-targets-by-id/1avidn3l8UPCyW6_A-M00EH-A2Q3J-f3P/BakerLabDrive/LabProjects/2757RB_RESTORE_LivingShorelines/Data/Shoreline_Data/Shoreline Methods Analyses/Paper_Figures/'
ggsave(paste0(mainDir, "G_1_ind.png"), generalP.1.ind, width = 10, height = 6, units = "in")

general_Arranged_2 <- arrangePlots(plotDF.general_2yr, seasonal = TRUE)
general_Arranged_1 <- arrangePlots(plotDF.general_1yr, seasonal = FALSE)
general_Arranged_3 <- arrangePlots(plotDF.general_3yr, seasonal = FALSE)


mainDir <- 'G:/.shortcut-targets-by-id/1avidn3l8UPCyW6_A-M00EH-A2Q3J-f3P/BakerLabDrive/LabProjects/2757RB_RESTORE_LivingShorelines/Data/Shoreline_Data/Shoreline Methods Analyses/Paper_Figures/'
ggsave(paste0(mainDir, "interval2_arranged.png"), general_Arranged_2, width = 11, height = 7, units = "in")
ggsave(paste0(mainDir, "interval1_arranged.png"), general_Arranged_1, width = 11, height = 3.5, units = "in")
ggsave(paste0(mainDir, "interval3_arranged.png"), general_Arranged_3, width = 11, height = 3.5, units = "in")

#1 Transect ----

RTK <- ggplot(plotDF |> filter(Method == "RTK", Transects == 1), 
              aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("RTK, 1 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
RTK

UAS.good <- ggplot(plotDF |> filter(Method == "UAS Good", Transects == 1), 
              aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Good, 1 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.good

UAS.poor <- ggplot(plotDF |> filter(Method == "UAS Poor", , Transects == 1), 
                   aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Poor, 1 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.poor

Aerial.good <- ggplot(plotDF |> filter(Method == "Aerial Good", Transects == 1), 
                   aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Good, 1 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.good

Aerial.poor <- ggplot(plotDF |> filter(Method == "Aerial Poor",Transects == 1), 
                      aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Poor, 1 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.poor

#30 Transects ----

RTK.30 <- ggplot(plotDF |> filter(Method == "RTK", Transects == 30), 
              aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("RTK, 30 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
RTK.30

UAS.good.30 <- ggplot(plotDF |> filter(Method == "UAS Good", Transects == 30), 
                   aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Good, 30 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.good.30

UAS.poor.30 <- ggplot(plotDF |> filter(Method == "UAS Poor", , Transects == 30), 
                   aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Poor, 30 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.poor.30

Aerial.good.30 <- ggplot(plotDF |> filter(Method == "Aerial Good", Transects == 30), 
                      aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Good, 30 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.good.30

Aerial.poor.30 <- ggplot(plotDF |> filter(Method == "Aerial Poor", Transects == 30), 
                      aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Poor, 30 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.poor.30

#100 Transects ----

RTK.100 <- ggplot(plotDF |> filter(Method == "RTK", Transects == 100), 
                 aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("RTK, 100 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
RTK.100

UAS.good.100 <- ggplot(plotDF |> filter(Method == "UAS Good", Transects == 100), 
                      aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Good, 100 Transect") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.good.100

UAS.poor.100 <- ggplot(plotDF |> filter(Method == "UAS Poor", , Transects == 100), 
                      aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("UAS Poor, 100 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
UAS.poor.100

Aerial.good.100 <- ggplot(plotDF |> filter(Method == "Aerial Good", Transects == 100), 
                         aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Good, 100 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.good.100

Aerial.poor.100 <- ggplot(plotDF |> filter(Method == "Aerial Poor", Transects == 100), 
                         aes(x = Interval, y = Rate, fill = Quality)) +
  geom_raster() +
  scale_fill_manual(values = c("Poor" = "red", "Good" = "blue")) +
  facet_grid(~Years) +
  ggtitle("Aerial Poor, 100 Transects") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Years", breaks = NULL, labels = NULL))
Aerial.poor.100

#generalized plots----

plotDF.general <- plotDF |>
  filter(Interval == 2, Quality == "Good") |>
  mutate(Rate = abs(as.numeric(levels(Rate))[Rate])) |>
  group_by(Transects, Years, Method) |>
  summarize(Rate = min(Rate))

# Method = case_match(Method,
#                     "RTK" ~ "RTK",
#                     c("Aerial Good", "Aerial Poor") ~ "Aerial",
#                     c("UAS Good", "UAS Poor") ~ "UAS")

generalP.1 <- ggplot(plotDF.general |> filter(Transects == 1), 
                   aes(x = Years, y = Rate, group = Method, color = Method)) +
  geom_line() +
  geom_jitter(size = 3, width = 0.1, height = 0.1)
generalP.1 

generalP.30 <- ggplot(plotDF.general |> filter(Transects == 30), 
                     aes(x = Years, y = Rate, group = Method, color = Method)) +
  geom_line() +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  ylim(0, 10)
generalP.30 

generalP.100 <- ggplot(plotDF.general |> filter(Transects == 100), 
                      aes(x = Years, y = Rate, group = Method, color = Method)) +
  geom_line() +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  ylim(0, 10)
generalP.100 

#save plots----

# ggsave("Figures/rtk_1.jpg", RTK, width = 5, height = 4, units = "in")
# ggsave("Figures/UASgood_1.jpg", UAS.good, width = 5, height = 4, units = "in")
# ggsave("Figures/UASpoor_1.jpg", UAS.poor, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialGood_1.jpg", Aerial.good, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialPoor_1.jpg", Aerial.poor, width = 5, height = 4, units = "in")
# 
# ggsave("Figures/rtk_30.jpg", RTK.30, width = 5, height = 4, units = "in")
# ggsave("Figures/UASgood_30.jpg", UAS.good.30, width = 5, height = 4, units = "in")
# ggsave("Figures/UASpoor_30.jpg", UAS.poor.30, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialGood_30.jpg", Aerial.good.30, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialPoor_30.jpg", Aerial.poor.30, width = 5, height = 4, units = "in")
# 
# ggsave("Figures/rtk_100.jpg", RTK.100, width = 5, height = 4, units = "in")
# ggsave("Figures/UASgood_100.jpg", UAS.good.100, width = 5, height = 4, units = "in")
# ggsave("Figures/UASpoor_100.jpg", UAS.poor.100, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialGood_100.jpg", Aerial.good.100, width = 5, height = 4, units = "in")
# ggsave("Figures/AerialPoor_100.jpg", Aerial.poor.100, width = 5, height = 4, units = "in")
