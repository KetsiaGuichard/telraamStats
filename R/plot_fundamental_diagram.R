#' @description
#' A short description...
#' Plot three fundamentals diagrams 
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday_choice weekday Character vector. Weekday choosen. Default to the all week.
#' @param hour_choice Integer vector. Hours choosen, default to the all day.
#' @param vacation_choice Character vector. Selected vacation. Full period by default (NULL).
#' @param holiday_choise Boolean. Selected holiday.  Full period by default (NULL).
#' @param direction_choice Character vector. Selected direction ('rgt','lft'). Both by default (NULL)
#'  
#' @return enriched_data 
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' plot_fundamental_diagram(traffic)
#' plot_fundamental_diagram(traffic,
#'   date_range = c('2022-07-01','2022-09-01'),
#'   weekday_choice= c('monday','friday','sunday),
#'   hour_choice= c(1,5,10,14,21),
#'   vacation_choice=NULL,
#'   holiday_choice=TRUE,
#'   segments = 'RteVitre-06',
#'   direction_choice='rgt')

plot_fundamental_diagram <-function(enriched_data,
                                      date_range = NULL,
                                      weekday_choice = NULL,
                                      hour_choice = NULL,
                                      vacation_choice=NULL,
                                      holiday_choice=NULL,
                                      segments = NULL,
                                      direction_choice=NULL)
        
{
  #Filter the user's demand
  enriched_data<-filter_demand_user(enriched_data,segments,date_range,weekday_choice,hour_choice,
                                    vacation_choice,holiday_choice)
  
  id_seg<-unique(enriched_data$segment_id)

  #Plot the fundamental diagram for each sensor
  for (id in id_seg)
  {
    df_seg<-enriched_data[enriched_data$segment_id==id,]
    if("speed_hist_car_lft" %in% colnames(df_seg)){df_seg<-reconstitute_v85(df_seg)}
    df_seg<-retrieve_missing_data(df_seg)
    
    df_seg<-calculate_axes(df_seg)
    plot_diagram(df_seg,direction_choice)
  }

}

#' @description
#' A short description...
#' Filter the dataframe from user's demand
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday_choice weekday Character vector. Weekday choosen. Default to the all week.
#' @param hour_choice Integer vector. Hours choosen, default to the all day.
#' @param vacation_choice Character vector. Selected vacation. Full period by default (NULL).
#' @param holiday_choise Boolean. Selected holiday.  Full period by default (NULL).
#'  
#' @return enriched_data 
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' filter_demand_user(traffic)
#' filter_demand_user(traffic,
#'  date_range = c('2022-07-01','2022-09-01'),
#'  weekday_choice= c('monday','friday','sunday),
#'  hour_choice= c(1,5,10,14,21),
#'  vacation_choice=NULL,
#'  holiday_choice=TRUE,
#'  segments = 'RteVitre-06')

filter_demand_user<-function (enriched_data,
                              segments,
                              date_range,
                              weekday_choice,
                              hour_choice,
                              vacation_choice,
                              holiday_choice)
{
  if(!is.null(segments))
  {enriched_data<-enriched_data %>% filter(segment_name %in% segments)}
  
  if(!is.null(date_range))
  {enriched_data<-enriched_data[enriched_data$day>=date[1] & enriched_data$day<= date[2],]}
  
  enriched_data$weekday<-tolower(enriched_data$weekday)
  tolower(weekday_choice)
  
  if(!is.null(weekday_choice))
  { enriched_data<-enriched_data %>% filter(weekday %in% weekday_choice)}
  
  if(!is.null(hour_choice))
  {enriched_data<-enriched_data %>% filter(hour %in% hour_choice)}
  
  if(!is.null(vacation_choice))
  {enriched_data<-enriched_data %>% filter(vacation %in% vacation_choice)}
  
  if(!is.null(holiday_choice))
  {enriched_data<-enriched_data %>% filter(holiday %in% holiday_choice)}
  
  enriched_data$weekend<-ifelse(enriched_data$weekday %in% c('saturday','sunday'), "Weekend", "Week")
  return(enriched_data)
}

#' @description
#' A short description...
#' Calculate necessary data to trace the fundamental diagram
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'  
#' @return enriched_data 
#' @export
#'
#'
#' @examples
#' reconstitute_v85(traffic)
#' calculate_axes(traffic)

calculate_axes<-function(enriched_data)
{
  enriched_data$veh_h<-enriched_data$car
  enriched_data$km_h<-enriched_data$v85
  enriched_data$veh_km<-0
  for(i in 1:length(enriched_data$veh_km))
    {enriched_data$veh_km[i]<-enriched_data$veh_h[i]/enriched_data$km_h[i]}
      
   
  if("speed_hist_car_lft" %in% colnames(enriched_data))   
  {
      enriched_data$veh_h_lft<-enriched_data$car_lft*4
      enriched_data$km_h_lft<-enriched_data$v85_lft
      enriched_data$veh_km_lft<-0
      
      enriched_data$veh_h_rgt<-enriched_data$car_rgt*4
      enriched_data$km_h_rgt<-enriched_data$v85_rgt
      enriched_data$veh_km_rgt<-0
      
      for(i in 1:length(enriched_data$car))
      {
        enriched_data$veh_km_lft[i]<-enriched_data$veh_h_lft[i]/enriched_data$km_h_lft[i]
        enriched_data$veh_km_rgt[i]<-enriched_data$veh_h_rgt[i]/enriched_data$km_h_rgt[i]
      }
  }
  return(enriched_data)
}

#' @description
#' A short description...
#' Plot three fundamentals diagrams 
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param direction_choice Character vector. Selected direction ('rgt','lft'). Both by default (NULL)
#'  
#' @return enriched_data 
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' plot_diagram(traffic)
#' plot_diagram(traffic,
#'  direction_choice='rgt') 


plot_diagram<-function(enriched_data,
                       direction_choice=NULL)
{
  if(!("speed_hist_car_lft" %in% colnames(df_seg)))
  {
    plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_km, y = .data$veh_h, color = .data$weekend)) +
         geom_point(pch = 20) +
         labs(x = 'Density', y = 'Flow', title = paste('Segment :', enriched_data$segment_fullname[1])))
    plot(ggplot(data = enriched_data, mapping = aes(x = veh_km, y = km_h, color = weekend)) +
         geom_point(pch = 20) +
         labs(x = 'Density', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1])))
    plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_h, y = .data$km_h, color = .data$weekend)) +
         geom_point(pch = 20) +
         labs(x = 'Flow', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1])))
  }
  
  else
  {
    
    if(direction_choice== 'lft' || is.null(direction_choice))
    {
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_km_lft, y = .data$veh_h_lft, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Density', y = 'Flow', title = paste('Segment :', enriched_data$segment_fullname[1], ' Left')))
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_km_lft, y = .data$km_h_lft, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Density', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1], ' Left')))
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_h_lft, y = .data$km_h_lft, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Flow', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1],' Left')))
    }
    
    if(direction_choice== 'rgt' || is.null(direction_choice))
    {
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_km_rgt, y = .data$veh_h_rgt, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Density', y = 'Flow', title = paste('Segment :', enriched_data$segment_fullname[1], ' Right')))
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_km_rgt, y = .data$km_h_rgt, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Density', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1], ' Right')))
      plot(ggplot(data = enriched_data, mapping = aes(x = .data$veh_h_rgt, y = .data$km_h_rgt, color = .data$weekend)) +
             geom_point(pch = 20) +
             labs(x = 'Flow', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1],' Right')))
    }
  }

}


#' Reconstitute the v85 by direction from speed_hist_car
#'
#' Add two colums in the dataframe : v85_lft and v85_rgt
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data 
#' @export
#'
#' @examples
#' reconstitute_v85(traffic)


reconstitute_v85<-function(enriched_data)
{
  
  speed<-seq(5,125,by=5)
  
  #Left
  enriched_data$v85_lft<-0

  for (i in 1:length(enriched_data$car))
    {
      vec<-enriched_data$speed_hist_car_lft[i]
      elements <- strsplit(vec, ",")[[1]]
      elements[1]<- gsub("\\[", "", elements[1])
      elements[25]<- gsub("\\]", "", elements[25])
      vecteur <- as.numeric(elements)
      per_vec <- sum(vecteur)
      per_85 <- 0.85*per_vec
      j<-1
      som<-0

      while (som<per_85 & j<length(vecteur))
        {
          som<-som+vecteur[j]
          j<-j+1
        }
  
      enriched_data$v85_lft[i]<-speed[j]
    }
  
  #Right
  enriched_data$v85_rgt<-0

  for (i in 1:length(enriched_data$car))
    {
      vec<-enriched_data$speed_hist_car_rgt[i]
      elements <- strsplit(vec, ",")[[1]]
      elements[1]<- gsub("\\[", "", elements[1])
      elements[25]<- gsub("\\]", "", elements[25])
      vecteur <- as.numeric(elements)
      per_vec <- sum(vecteur)
      per_85 <- 0.85*per_vec    
      j<-1
      som<-0
  
      while (som<per_85 & j<length(vecteur))
        {
          som<-som+vecteur[j]
          j<-j+1
        }
  
      enriched_data$v85_rgt[i]<-speed[j]
  
    }

  return(enriched_data)
}


