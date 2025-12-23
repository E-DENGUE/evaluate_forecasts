library(tidyverse)
library(viridis)
library(sf)

all.files <- list.files('./Output_Forecasted_by_Issue_Date', recursive=T, full.names=T)

all_df <- lapply(all.files,read_csv) %>%
  bind_rows() %>%
  mutate(fcode = gsub('ED_','', fcode))

#claude generated cluster based on 20024-2022
clusters_new <- read_csv('./raw/claude_province_cluster_assignments.csv') %>%
  rename(fcode0 = District) %>%
  mutate(
    fcode0 = gsub('CA_MAU', 'CAM_MAU', fcode0),
    fcode0 = gsub('_DISTRICT', '', fcode0),
    fcode0 = gsub('_CITY', '', fcode0),
    fcode0 = gsub('_URBAN', '', fcode0),
    fcode0 = gsub('_TOWN', '', fcode0),
    fcode0 = gsub('THANH_BEN_TRE', 'THANH', fcode0),
    fcode0 = gsub('TIEN_GIANG_CHAU_THANH_TIEN_GIANG','TIEN_GIANG_CHAU_THANH', fcode0),
    fcode0 = gsub('HAU_GIANG_CHAU_THANH_HAU_GIANG','HAU_GIANG_CHAU_THANH', fcode0),
    fcode0 = gsub('TRA_VINH_CHAU_THANH_TRA_VINH','TRA_VINH_CHAU_THANH', fcode0),
    fcode0 = gsub('LONG_AN_CHAU_THANH_LONG_AN', 'LONG_AN_CHAU_THANH', fcode0),
    
    fcode0 = gsub('AN_GIANG_CHAU_THANH_AN_GIANG','AN_GIANG_CHAU_THANH', fcode0),
    fcode0 = gsub('AN_GIANG_PHU_TAN_AN_GIANG', 'AN_GIANG_PHU_TAN', fcode0),
    fcode0 = gsub('DONG_THAP_CHAU_THANH_DONG_THAP','DONG_THAP_CHAU_THANH_DONG_THAP', fcode0),
    fcode0 = gsub('CAM_MAU_PHU_TAN_CAM_MAU','CAM_MAU_PHU_TAN', fcode0),
    fcode0 = gsub('DONG_THAP_CHAU_THANH_DONG_THAP','DONG_THAP_CHAU_THANH', fcode0)
    
  ) 

district_characteristics <- readRDS('../Dengue_District_HPC/Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
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
  mutate(
         fcode = gsub('CA_MAU', 'CAM_MAU', fcode),
         fcode0 = gsub('_DISTRICT', '', fcode),
         fcode0 = gsub('_CITY', '', fcode0),
         fcode0 = gsub('_URBAN', '', fcode0),
         fcode0 = gsub('_TOWN', '', fcode0)
         ) %>%
  left_join(district_characteristics, by='fcode0') %>%
  left_join(clusters_new, by='fcode0')


cor.obs.pred.top9 <- cor.obs.pred %>%
  filter(unlagged_better==1) %>%
  mutate(rank1= row_number() ) 
  
all_df %>%
    left_join(cor.obs.pred.top9, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & rank1<=9) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')+
  ggtitle('Top districts with no lag')


cor.obs.pred.top9.v2 <- cor.obs.pred %>%
  mutate(rank2= row_number() ) 

all_df %>%
  left_join(cor.obs.pred.top9.v2, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & rank1<=9) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')+
  ggtitle('Top models with or without lagged prediction (not as good)')



all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & cor.obs.pred <0.06) %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')+
  ggtitle('Worst models')

#Vinh Long province
all_df %>%
  filter(Forecast_horizon == '3 months' & grepl('VINH_LONG', fcode) & pred_date>='2023-01-01') %>%
  ggplot()+
  geom_point(aes(x=pred_date,y=obs_dengue_cases)) +
  geom_line(aes(x=pred_date, y=pred_cases))+
  ggtitle('Observed vs predicted cases 2023-2025')+
  ylim(0,NA) +
  facet_wrap(~fcode, nrow=3,ncol=3, scales='free_y')+
  ggtitle('Top districts with no lag')


cor.obs.pred %>%
  mutate(Cluster=as.factor(Cluster)) %>%
ggplot()+
  geom_violin(aes(x=Cluster, y=cor.obs.pred, color=unlagged_better))


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



ts_mat <- district_characteristics <- readRDS('../Dengue_District_HPC/Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  mutate(fcode0 = paste(province, district),
         fcode0 = gsub(' ','_',fcode0),
         inc = m_DHF_cases/pop*100000
  ) %>%
  pivot_wider(id_cols=c(date), values_from=inc, names_from=fcode0)
    
write_csv(ts_mat, './raw/ts_mat.csv')



## Try some alternative models for poor districts
worst_districts <- all_df %>%
  left_join(cor.obs.pred, by='fcode') %>%
  filter(Forecast_horizon == '3 months' & cor.obs.pred <0.06) %>%
  pull(fcode) %>%
  unique()

unique(cor.obs.pred.top9$fcode)

  in.ds3$pred <- exp(fit_inla$summary.linear.predictor$mean)
  in.ds3$log_resid <- log((in.ds3$obs_dengue_cases+1) / (in.ds3$pred1+1) )
  in.ds3$cumresid <- cumsum(in.ds3$log_resid)
  
  
single_district_test <- function(select.district){  
    in.ds <- arrow::read_parquet('../baseline_evaluation/data/raw.parquet') %>%
      mutate(fcode = gsub('ED_', '', fcode)) %>%
      filter( fcode == select.district) %>%
      mutate(date = as.Date(paste(year, month, '01', sep='-')) ) %>%
      arrange(fcode, date) %>%
      group_by(fcode) %>%
      mutate(index = row_number(),
             sin12 = sin(2*pi*index/12),
             cos12 = cos(2*pi*index/12)
      ) %>%
      ungroup()
    
    p2 <- ggplot(in.ds) +
      geom_line(aes(x=date, y=obs_dengue_cases)) 
    p2
    
    
    in.ds2 <- in.ds %>%
      filter(date>='2004-01-01')
    
    mod1 <- glm(obs_dengue_cases~ sin12+cos12 , family='poisson', data=in.ds2 )
    
    in.ds2$pred1 <- predict(mod1, type='response')
    in.ds2$log_resid <- log((in.ds2$obs_dengue_cases+1) / (in.ds2$pred1+1) )
    in.ds2$cumresid <- cumsum(in.ds2$log_resid)
    
     p2 +
       geom_line(data=in.ds2, aes(x=date, y=pred1, color='red'))
     
    
     
     
     in.ds3 <- in.ds2 %>%
      # filter(date<='2015-01-01' & date<='2025-01-01') %>%
       arrange(date) %>%
       mutate(t=row_number(),
              t2=2,
              max_temp_c = t2m_max -273.15,
              min_temp_c = t2m_min - 273.15,
              optimal_temp = if_else(max_temp_c<=32 & min_temp_c>=24,1,0),
      
              f_min = exp(-((pmax(0, 26 - min_temp_c))^2) / (2 * 2^2)),
              f_max = exp(-((pmax(0, max_temp_c - 30))^2) / (2 * 2^2)),
              
              thermal_suitability  = f_min * f_max,  
              yearN = year - min(year, na.rm=T) + 1,
              
              climate_scale = scale(thermal_suitability),
              climate_scale_lag3 = lag(climate_scale,3),
              )
     
     form1 <- as.formula( 'obs_dengue_cases    ~ 1 +
         climate_scale   + 
         sin12 + cos12+                       # fixed effects (optional)
         f(t, model = "ar1", constr=T)+
         f(t2, model = "iid", constr=T)+
    
              f(yearN, model="rw2", constr=T)            
                          '
                          )
     
     fit_inla <- INLA::inla(form1,
       data = in.ds3,
          family = "poisson" ,
       control.compute = list(dic = TRUE, waic = TRUE)
       
     )
     
     mod.summary <-summary(fit_inla)
     return(mod.summary)
}
 

districts_test <- unique(all_df$fcode)
all.res <- lapply(districts_test,single_district_test)

beta1 <- sapply(all.res, function(x) x$fixed['climate_scale','mean'] )

beta.climate <- cbind.data.frame('fcode'=districts_test, 'beta1'=beta1)

MDR_NEW <- sf::st_read(dsn = "./shape/MDR_NEW_Boundaries_Final.shp") 
MDR_NEW$fcode = paste(MDR_NEW$NAME_En, MDR_NEW$VARNAME, MDR_NEW$ENGTYPE, sep='_')
MDR_NEW$fcode = toupper(MDR_NEW$fcode )
MDR_NEW$fcode  = gsub(' ', '_', MDR_NEW$fcode )
MDR_NEW$fcode  = gsub('CITY_CITY', 'CITY', MDR_NEW$fcode )
MDR_NEW$fcode  = gsub('CAM_', 'CA_', MDR_NEW$fcode )


covars <-    in.ds <- arrow::read_parquet('../baseline_evaluation/data/raw.parquet') %>%
  mutate(fcode = gsub('ED_', '', fcode),
         max_temp_c = t2m_max -273.15,
         min_temp_c = t2m_min - 273.15,
         optimal_temp = if_else(max_temp_c<=32 & min_temp_c>=24,1,0),
         
         f_min = exp(-((pmax(0, 26 - min_temp_c))^2) / (2 * 2^2)),
         f_max = exp(-((pmax(0, max_temp_c - 30))^2) / (2 * 2^2)),
         
         thermal_suitability  = f_min * f_max  ) %>%
  group_by(fcode) %>%
  summarize(pop_density = mean(pop_density),
            min_temp = mean(t2m_min),
            max_temp = mean(t2m_max),
            thermal_suitability= mean(thermal_suitability)
            )

comp_df <- MDR_NEW %>%
  full_join(beta.climate,by=c('fcode')) %>%
  left_join(covars, by=c('fcode')) 

comp_df%>%
  ggplot() +
  geom_sf(aes(fill = beta1)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  )+
  theme_minimal()
p3

ggplot(comp_df)+
  geom_point(aes(y=beta1, x=thermal_suitability))
  # 
 # p3 <- ggplot(in.ds3) +
 #   geom_line(aes(x=date, y=obs_dengue_cases)) +
 #   geom_line(aes(x=date, y=pred, col='red')) 
 # p3
 # 