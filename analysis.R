library(tidyverse)
library(viridis)
library(sf)

all.files <- list.files('./Output_Forecasted_by_Issue_Date', recursive=T, full.names=T)

all_df <- lapply(all.files,read_csv) %>%
  bind_rows() %>%
  mutate(fcode = gsub('ED_','', fcode))

district_characters <- readRDS('../Dengue_District_HPC/Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  mutate(fcode0 = paste(province, district),
         fcode0 = gsub(' ','_',fcode0),
         inc = m_DHF_cases/pop*100000
         ) %>%
  group_by(fcode0) %>%
  summarize(inc = mean(inc),
            m_DHF_cases = mean(m_DHF_cases),
            Population_density = mean(Population_density, na.rm=T),
            NetImmigration_Rate = mean(NetImmigration_Rate, na.rm=T),
            Hygienic_Water_Access = mean(Hygienic_Water_Access, na.rm=T),
            Monthly_Average_Income_Percapita = mean(Monthly_Average_Income_Percapita, na.rm=T)
            )


overall_check <- all_df %>%
  filter(Forecast_horizon == '3 months') %>%
  group_by(Forecast_horizon,pred_date) %>%
  summarize(pred_cases=sum(pred_cases),
            obs_dengue_cases = sum(obs_dengue_cases)
            )

ggplot(overall_check)+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA)

district_names = unique(all_df$fcode)

cor.obs.pred <- all_df %>%
  filter(Forecast_horizon == '3 months') %>%
  group_by(fcode) %>%
  summarize( cor.obs.pred = cor(obs_dengue_cases,pred_cases, use="pairwise.complete.obs"),
             cor.obs.pred.lag1 = cor(lag(obs_dengue_cases,1),pred_cases, use="pairwise.complete.obs"),
             cor.obs.pred.lag2 = cor(lag(obs_dengue_cases,2),pred_cases, use="pairwise.complete.obs"),
             unlagged_better = if_else(cor.obs.pred>cor.obs.pred.lag1 & cor.obs.pred>cor.obs.pred.lag2 ,1,0)
             ) %>%
  arrange(-cor.obs.pred) %>%
  mutate(rank1= row_number() ,
         fcode = gsub('CA_MAU', 'CAM_MAU', fcode),
         fcode0 = gsub('_DISTRICT', '', fcode),
         fcode0 = gsub('_CITY', '', fcode0),
         fcode0 = gsub('_URBAN', '', fcode0),
         fcode0 = gsub('_TOWN', '', fcode0)
         ) %>%
  left_join(district_characters, by='fcode0')


all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & rank1<=9) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')

all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & rank1>9) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode,scales='free_y')


all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & cor.obs.pred >=0.78) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')


all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & cor.obs.pred >=0.6164 & unlagged_better==1) %>%
ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')


all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & cor.obs.pred <0.235) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')


View(cor.obs.pred)


  MDR_NEW <- sf::st_read(dsn = "./shape/MDR_NEW_Boundaries_Final.shp") 
  MDR_NEW$fcode = paste(MDR_NEW$NAME_En, MDR_NEW$VARNAME, MDR_NEW$ENGTYPE, sep='_')
  MDR_NEW$fcode = toupper(MDR_NEW$fcode )
  MDR_NEW$fcode  = gsub(' ', '_', MDR_NEW$fcode )
  MDR_NEW$fcode  = gsub('CITY_CITY', 'CITY', MDR_NEW$fcode )

  p3 <- MDR_NEW %>%
    full_join(cor.obs.pred,by=c('fcode')) %>%
    ggplot() +
    geom_sf(aes(fill = cor.obs.pred)) +
    scale_fill_viridis()+
    theme_minimal()
p3

#are there factors associated with better correlation between observed and predicted
cor(cor.obs.pred[,c('cor.obs.pred','cor.obs.pred.lag2','inc','m_DHF_cases','Population_density','NetImmigration_Rate','Hygienic_Water_Access','Monthly_Average_Income_Percapita')], use='pairwise.complete.obs')

    