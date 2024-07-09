#' @description
#' A short description...
#' Calculate necessary data to trace the axes of the fundamental diagram
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#'
#' @examples
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
#' Calculate characteristics values of the fundamental diagram.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param direction_choice Character. Direction choosen. Default to NULL.
#'
#' @return list(flow,speed,density)
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_characteristic(traffic)
#' calculate_characteristic(traffic,
#'   direction_choice='lft')

calculate_characteristic<- function (enriched_data,
                                     direction_choice=NULL)
{
  if(is.null(direction_choice) || !("speed_hist_car_lft" %in% colnames(enriched_data)))
  {
    density <- sum(enriched_data$veh_km,na.rm = TRUE)/length(enriched_data$veh_km)
    speed<- sum(enriched_data$km_h,na.rm = TRUE)/length(enriched_data$km_h)
    flow<- sum(enriched_data$veh_h,na.rm = TRUE)/length(enriched_data$veh_h)
  }


  else if (direction_choice=='lft')
  {
    density<-sum(enriched_data$veh_km_lft,na.rm = TRUE)/length(enriched_data$veh_km_lft)
    speed<-sum(enriched_data$km_h_lft,na.rm = TRUE)/length(enriched_data$km_h_lft)
    flow<- sum(enriched_data$veh_h_lft,na.rm = TRUE)/length(enriched_data$veh_h_lft)
  }

  else
  {
    density<-sum(enriched_data$veh_km_rgt,na.rm = TRUE)/length(enriched_data$veh_km_rgt)
    speed<-sum(enriched_data$km_h_rgt,na.rm = TRUE)/length(enriched_data$km_h_rgt)
    flow<- sum(enriched_data$veh_h_rgt,na.rm = TRUE)/length(enriched_data$veh_h_rgt)
  }


  return(list(flow=flow,speed=speed,density=density))

}

#' @description
#' A short description...
#' Create news columns to have dimensionless data.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param direction_choice Character Direction choosen. Default to NULL.
#' @param list_charac List of Reals. Characteristics values.
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_data_dimensionless(traffic,
#'   direction_choice='lft,
#'   list_charac=calculate_characteristic(traffic))


calculate_data_dimensionless<- function (enriched_data,
                                         direction_choice=NULL,
                                         list_charac)
{
  if(!("speed_hist_car_lft" %in% colnames(enriched_data)) || is.null(direction_choice))
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_adim=veh_km/list_charac$density,
        km_h_adim=km_h/list_charac$speed,
        veh_h_adim= veh_h/list_charac$flow)
  }

  else if (direction_choice=='lft')
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_lft_adim=veh_km_lft/list_charac$density,
        km_h_lft_adim=km_h_lft/list_charac$speed,
        veh_h_lft_adim=veh_h_lft/list_charac$flow)
  }

  else
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_rgt_adim=veh_km_rgt/list_charac$density,
        km_h_rgt_adim=km_h_rgt/list_charac$speed,
        veh_h_rgt_adim=veh_h_rgt/list_charac$flow)
  }

  return(enriched_data)

}


#' @description
#' A short description...
#' Calculate the slope of the interpolation line for each slope tested.
#'
#'
#' @param ordinate_line Real vector. Ordinate test.Length vector = NumberOfOrdinate.
#' @param mat_inter Real matrix. Percent of point under the tested line. Matrix result of the function 'count_point'
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#'
#' @return alpha
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_interpolation_line(ordinate_line,
#'   mat_inter,
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65)

calculate_interpolation_line<-function (ordinate_line,
                                        mat_inter,
                                        NumberOfSlope=50,
                                        NumberOfOrdinate=45)
{
  alpha<-rep(0,NumberOfSlope)

  for (i in 1:NumberOfSlope)
  {
    if(mat_inter[i,NumberOfOrdinate+1] > 1)
    {
      xout <- seq(ordinate_line[mat_inter[i,NumberOfOrdinate+1]-1],
                  ordinate_line[mat_inter[i,NumberOfOrdinate+1]],
                  by = ordinate_line[mat_inter[i,NumberOfOrdinate+1]]-ordinate_line[mat_inter[i,NumberOfOrdinate+1]-1])
      interpolated <- approx(ordinate_line,mat_inter[i,1:NumberOfOrdinate], xout = xout,method = 'linear')
      x_interp <- interpolated$x
      y_interp <- interpolated$y
      alpha[i]<- (y_interp[2]-y_interp[1])/(x_interp[2]-x_interp[1])
    }
  }

  return(alpha)
}


#' @description
#' A short description...
#' Calculate a and b from the equation y=ax+b
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#' @param slope Real vector. vector of 2 reals.
#' @param ordinate Real vector. vector of 2 reals.
#' @param direction_choice Character. Direction choosen.default to NULL.
#'
#' @return list(a,b)
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_necessary_data (traffic,
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65,
#'   slope=c(-2,-0.1),
#'   ordinate=c(1,1.5),
#'   direction_choice=NULL)

calculate_necessary_data <- function (enriched_data,
                                      NumberOfSlope=50,
                                      NumberOfOrdinate=45,
                                      slope,
                                      ordinate,
                                      direction_choice=NULL,
                                      percent1=95,
                                      percent2=98)
{
  percent<-percent1

  for(i in 1:2)
  {
    # Create vectors to have the values we want to test
    slope_line<-seq(from=slope[1],to=slope[2],length.out=NumberOfSlope)
    ordinate_line<-seq(from=ordinate[1],to=ordinate[2],length.out=NumberOfOrdinate)

    # Percent of the point under the line
    mat_inter<-count_point(enriched_data,slope_line,ordinate_line,direction_choice,
                           NumberOfSlope,NumberOfOrdinate,percent)

    # Calculate the alpha slope
    alpha<-calculate_interpolation_line(ordinate_line,mat_inter,
                                        NumberOfSlope,NumberOfOrdinate)

    # Calculate the final line
    final_slope<-slope_line[which(alpha==max(alpha))]
    final_ordinate<-ordinate_line[mat_inter[which(alpha==max(alpha)),NumberOfOrdinate+1]]

    # Update data for the following loop
    percent<- percent2
    slope<-c(final_slope,final_slope)
    ordinate<-c(final_ordinate,4*final_ordinate)

  }
  final_slope<-final_slope[1]
  final_ordinate<-final_ordinate[1]

  return(list(a=final_slope,b=final_ordinate))
}


#' @description
#' A short description...
#' Calculate the percent of point under the line tested.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param slope_line Real vector. Slope tested.Length vector = NumberOfSlope.
#' @param ordinate_line Real vector. Ordinate tested.Length vector = NumberOfOrdinate.
#' @param direction_choice Character. Direction choosen.
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#' @param percent Real. Percent of point under which we want the number of points.
#'
#' @return mat_inter
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' count_point(traffic,
#'   slope_line=seq(from=-2,to=-0.1,length.out=50),
#'   ordinate_line=seq(from=1,to=1.5,length.out=45),
#'   direction_choice='lft',
#'   NumberOfSlope=50,
#'   NumberofOrdinate=45,
#'   percent=95)


count_point<-function(enriched_data,
                      slope_line,
                      ordinate_line,
                      direction_choice=NULL,
                      NumberOfSlope=50,
                      NumberOfordiante=45,
                      percent)

{
  if(is.null(direction_choice) || !("speed_hist_car_lft" %in% colnames(enriched_data)) )
  {
    abcissa_point<-enriched_data$veh_km_adim
    ordinate_point<-enriched_data$km_h_adim
  }

  else if (direction_choice=='lft')
  {
    abcissa_point<-enriched_data$veh_km_lft_adim
    ordinate_point<-enriched_data$km_h_lft_adim
  }

  else
  {
    abcissa_point<-enriched_data$veh_km_rgt_adim
    ordinate_point<-enriched_data$km_h_rgt_adim
  }


  mat_inter<-matrix(0,nrow=NumberOfSlope,ncol=NumberOfordiante+1)

  for(a in 1:NumberOfSlope)
  {
    test_slope<-slope_line[a]

    for(b in 1:NumberOfordiante)
    {
      test_ordinate<-ordinate_line[b]
      length_vector<-0

      NumberOfPoint<-0
      for(i in 1:length(abcissa_point))
      {
        if(!is.na(abcissa_point[i]))
        {
          length_vector<-length_vector+1
          test_equation<-test_slope*abcissa_point[i]+test_ordinate
          if(ordinate_point[i]<test_equation){NumberOfPoint<-NumberOfPoint+1}
        }

      }
      mat_inter[a,b]<-100*NumberOfPoint/length_vector
      if( mat_inter[a,b]>=percent & mat_inter[a,NumberOfordiante+1]==0)
      {mat_inter[a,NumberOfordiante+1]=b}
    }
    if(mat_inter[a,NumberOfordiante+1]==0)
    {mat_inter[a,NumberOfordiante+1]=1}
  }

  return(mat_inter)
}


#' @description
#' A short description...
#' Create the necessary columns to have the final envelope of the fundamental diagram.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' create_necessary_column(traffic)

create_necessary_column <- function (enriched_data)
{
  if("speed_hist_car_lft" %in% colnames(enriched_data))
  {enriched_data<-restore_v85(enriched_data)}

  enriched_data<-retrieve_missing_data(enriched_data)

  enriched_data<-calculate_axes(enriched_data)

  return(enriched_data)
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
                              segments=NULL,
                              date_range=NULL,
                              weekday_choice=NULL,
                              hour_choice=NULL,
                              vacation_choice=NULL,
                              holiday_choice=NULL)
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
  {
    if(vacation_choice==FALSE)
    {enriched_data<-enriched_data[enriched_data$vacation!='no vacation',]}

    else
    {enriched_data<-enriched_data[enriched_data$vacation=='no vacation',]}
  }

  if(!is.null(holiday_choice))
  {enriched_data<-enriched_data %>% filter(holiday %in% holiday_choice)}

  enriched_data$weekend<-ifelse(enriched_data$weekday %in% c('saturday','sunday'), "Weekend", "Week")
  return(enriched_data)
}


#' @description
#' A short description...
#' Plot the fundamental diagram and its envelope
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday_choice weekday Character vector. Weekday choosen. Default to the all week.
#' @param hour_choice Integer vector. Hours choosen, default to the all day.
#' @param vacation_choice Character vector. Selected vacation. Full period by default (NULL).
#' @param holiday_choise Boolean. Selected holiday.  Full period by default (NULL).
#' @param direction_choice Character Direction choosen. Default to NULL.
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#'
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' plot_diagram_envelope(traffic)
#' plot_diagram_envelope(traffic,
#'   date_range = c('2022-07-01','2022-09-01'),
#'   weekday_choice= c('monday','friday','sunday),
#'   hour_choice= c(1,5,10,14,21),
#'   vacation_choice=NULL,
#'   holiday_choice=TRUE,
#'   segments = 'RteVitre-06',
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65,
#'   direction_choice='lft)


plot_diagram_envelope <- function (enriched_data,
                                   date_range = NULL,
                                   weekday_choice = NULL,
                                   hour_choice = NULL,
                                   vacation_choice=NULL,
                                   holiday_choice=NULL,
                                   segments = NULL,
                                   direction_choice=NULL,
                                   NumberOfSlope=50,
                                   NumberOfOrdinate=45,
                                   percent1=c(95,99),
                                   percent2=c(95,99))
{
  enriched_data<-filter_demand_user(enriched_data,
                                    segments,
                                    date_range,
                                    weekday_choice,
                                    hour_choice,
                                    vacation_choice,
                                    holiday_choice)

  id_seg<-unique(enriched_data$segment_id)

  #Plot the fundamental diagram for each sensor
  for (id in id_seg)
  {
    df<-enriched_data[enriched_data$segment_id==id,]

    df<-create_necessary_column(df)
    list_charac<-calculate_characteristic(df ,
                                          direction_choice)
    df<-calculate_data_dimensionless(df,
                                     direction_choice,
                                     list_charac)

    list_final_1<-calculate_necessary_data(df,
                                           NumberOfSlope,
                                           NumberOfOrdinate,
                                           slope=c(-2,-0.1),
                                           ordinate=c(1,7),
                                           direction_choice,
                                           percent1=percent1[1],
                                           percent2=percent1[2])

    list_final_2<-calculate_necessary_data(df,
                                           NumberOfSlope,
                                           NumberOfOrdinate,
                                           slope=c(-6,list_final_1$a),
                                           ordinate=c(list_final_1$b,8*list_final_1$b),
                                           direction_choice,
                                           percent1=percent2[1],
                                           percent2=percent2[2])

    # Repasser en données dimensionnées
    list_final_1$a<-list_charac$speed*list_final_1$a/list_charac$density
    list_final_1$b<-list_charac$speed*list_final_1$b

    list_final_2$a<-list_charac$speed*list_final_2$a/list_charac$density
    list_final_2$b<-list_charac$speed*list_final_2$b

    # Affiche le diagramme et les droites
    plot(plot_lines(df,
                    list_final_1,
                    list_final_2,
                    direction_choice))
  }
}



#' @description
#' A short description...
#' Plot the fundamental diagram and its envelope.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param list_final_1 List of reals. Values of the line's equation.
#' @param list_final_2 List of reals. Values of the line's equation.
#' @param direction_choice Character Direction choosen. Default to NULL.
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' plot_lines(traffic,
#'   list_final_1=c(-0.1,70),
#'   list_final_2=c(-5,80),
#'   direction_choice=NULL)

plot_lines <- function (enriched_data,
                        list_final_1,
                        list_final_2,
                        direction_choice=NULL)
{
  #Define the axes of the diagram
  if(!("speed_hist_car_lft" %in% colnames(enriched_data)) || is.null(direction_choice))
  {
    abscissa<-enriched_data$veh_km
    ordinate1<-enriched_data$km_h
    ordinate2<-enriched_data$veh_h
  }

  else if(direction_choice=='lft')
  {
    abscissa<-enriched_data$veh_km_lft
    ordinate1<-enriched_data$km_h_lft
    ordinate2<-enriched_data$veh_h_lft
  }

  else
  {
    abscissa<-enriched_data$veh_km_rgt
    ordinate1<-enriched_data$km_h_rgt
    ordinate2<-enriched_data$veh_h_rgt
  }

  # Calculate the intersection between the lines
  x_inter<-(list_final_2$b - list_final_1$b)/(list_final_1$a - list_final_2$a)
  y_inter_1 <- list_final_1$a * x_inter + list_final_1$b
  y_inter_2 <- list_final_1$a * x_inter*x_inter + list_final_1$b*x_inter

  x_lim_1<--list_final_2$b/list_final_2$a
  x_lim_2<-x_lim_1/2
  y_lim_2<- list_final_2$a * x_lim_2*x_lim_2+ list_final_2$b*x_lim_2

  #Plot the fundamental diagram and its envelope
  graph1<-ggplot(data = enriched_data, mapping = aes(x = abscissa, y = ordinate1, color = 'red')) +
    geom_point(pch = 20) +
    labs(x = 'Density', y = 'Speed', title = paste('Segment :', enriched_data$segment_fullname[1]))


  graph1<-graph1+
    geom_line(mapping=aes(x=abscissa,y=list_final_1$a*abscissa+list_final_1$b),color='black')+
    geom_line(mapping=aes(x=abscissa,y=list_final_2$a*abscissa+list_final_2$b),color='black')+
    geom_point(aes(x = x_inter, y = y_inter_1), color = "green", size = 2) +
    geom_text(aes(x = x_inter, y = y_inter_1,hjust=-1, label = paste("x =", sprintf("%.2f", x_inter))))+
    geom_point(aes(x = x_lim_1, y = 0), color = "green", size = 2) +
    geom_text(aes(x = x_lim_1, y = 0,hjust=1, label = paste("x =", sprintf("%.2f", x_lim_1))))+
    coord_cartesian(xlim =c(0, x_lim_1), ylim = c(0, max(ordinate1)))
  plot(graph1)

  #Plot the other one
  graph2<-ggplot(data = enriched_data, mapping = aes(x = abscissa, y = ordinate2, color = 'red')) +
    geom_point(pch = 20) +
    labs(x = 'Density', y = 'Flow', title = paste('Segment :', enriched_data$segment_fullname[1]))


  graph2<-graph2+
    geom_line(mapping=aes(x=abscissa,y=list_final_1$a*abscissa*abscissa+list_final_1$b*abscissa),color='black')+
    geom_line(mapping=aes(x=abscissa,y=list_final_2$a*abscissa*abscissa+list_final_2$b*abscissa),color='black')+
    geom_point(aes(x = x_inter, y = y_inter_2), color = "green", size = 2) +
    geom_text(aes(x = x_inter, y = y_inter_2,hjust=2, label = paste("y =", sprintf("%.2f", y_inter_2))))+
    geom_point(aes(x = x_lim_2, y = y_lim_2), color = "green", size = 2) +
    geom_text(aes(x = x_lim_2, y = y_lim_2,vjust=-2, label = paste("y =", sprintf("%.2f", y_lim_2))))+
    coord_cartesian(xlim =c(0, max(abscissa)), ylim = c(0, max(ordinate2)))

}


#' Restore the v85 by direction from speed_hist_car
#'
#' Add two colums in the dataframe : v85_lft and v85_rgt
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#' @examples
#' restore_v85(enriched_data)


restore_v85<-function(enriched_data)
{

  speed<-seq(5,125,by=5)


  enriched_data$v85_lft<-0
  enriched_data$v85_rgt<-0

  for (i in 1:length(enriched_data$car))
  {
    #Left
    vec<-enriched_data$speed_hist_car_lft[i]
    elements <- strsplit(vec, ",")[[1]]
    elements[1]<- gsub("\\[", "", elements[1])
    elements[25]<- gsub("\\]", "", elements[25])
    vector <- as.numeric(elements)
    per_vec <- sum(vector)
    per_85 <- 0.85*per_vec
    j<-1
    sum<-0

    while (sum<per_85 & j<length(vector))
    {
      sum<-sum+vector[j]
      j<-j+1
    }
    enriched_data$v85_lft[i]<-speed[j]

    #Right
    vec<-enriched_data$speed_hist_car_rgt[i]
    elements <- strsplit(vec, ",")[[1]]
    elements[1]<- gsub("\\[", "", elements[1])
    elements[25]<- gsub("\\]", "", elements[25])
    vector <- as.numeric(elements)
    per_vec <- sum(vector)
    per_85 <- 0.85*per_vec
    j<-1
    sum<-0

    while (sum<per_85 & j<length(vector))
    {
      sum<-sum+vector[j]
      j<-j+1
    }
    enriched_data$v85_rgt[i]<-speed[j]
  }
  return(enriched_data)
}




#' @description
#' A short description...
#' Retrieve hours with no data and replace incomplete data with NA,
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param successive_day Integer. Number of day choosen. Default to 2
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' retrieve_missing_data(traffic)
#' retrieve_missing_data(traffic,
#'  date_range = c('2022-07-01','2022-09-01'),
#'  segment = 'RteVitre-06',
#'  uptime_choice=0.3,
#'  successive_day=1)

retrieve_missing_data<- function(enriched_data,
                                 date_range = NULL,
                                 segments = NULL,
                                 uptime_choice=0.5,
                                 successive_day=2,
                                 retrieve_data=TRUE)
{

  if(!is.null(segments))
  {enriched_data<-enriched_data[enriched_data$segment_name==segments,]}

  if(!is.null(date_range))
  {enriched_data<-enriched_data[enriched_data$day>=date_range[1] & enriched_data$day<= date_range[2],]}

  if(length(enriched_data$car)==0){stop("No data in the selectionned period")}

  else
  {
    #Hours with no data
    enriched_data<-retrieve_missing_hours(enriched_data,uptime_choice)

    #Inactivity Period
    enriched_data<-replace_inactivity_period(enriched_data,successive_day,uptime_choice,retrieve_data)

  }
  return(enriched_data)
}

#' @description
#' A short description...
#' Retrieve hours with no data
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' retrieve_missing_hours(traffic)
#' retrieve_missing_hours(traffic,
#'  uptime_choice=0.3)

retrieve_missing_hours<-function(enriched_data,
                                 uptime_choice=0.5)
{
  enriched_data$date <- ymd_hms(enriched_data$date)
  enriched_data$season <- ifelse(month(enriched_data$date) %in% c(3,4,5), "Spring",
                                 ifelse(month(enriched_data$date) %in% c(6,7,8), "Summer",
                                        ifelse(month(enriched_data$date) %in% c(9,10,11), "Autumn", "Winter")))

  df_season<-enriched_data %>% group_by(segment_id,season,hour) %>% summarise(condition=any(car!=0 & uptime>uptime_choice))

  enriched_data <- enriched_data %>% semi_join(df_season %>% filter(condition), by = c("segment_id","season", "hour"))

  return(enriched_data)
}

#' @description
#' A short description...
#' Replace incomplete data with NA,
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param successive_day Integer. Number of day choosen. Default to 2
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#' @param retrieve_data Boolean. Choice to replace data or not. Default to TRUE
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' replace_inactivity_period(traffic)
#' replace_inactivity_period(traffic,
#'  uptime_choice=0.3,
#'  successive_day=1,
#'  retrieve_data=FALSE)

replace_inactivity_period<-function (enriched_data,
                                     successive_day=2,
                                     uptime_choice=0.5,
                                     retrieve_data=TRUE)
{
  if(retrieve_data==FALSE)
  {
    enriched_data <- enriched_data %>%
      mutate(
        heavy_NA = heavy,
        car_NA = car,
        bike_NA = bike,
        pedestrian_NA = pedestrian
      )
  }


  list_clear_data <- list()
  seg_id<-unique(enriched_data$segment_id)

  for(id in 1:length(seg_id))
  {
    df_segment<-enriched_data[enriched_data$segment_id==seg_id[id],]
    for(i in 1:length(df_segment$car))
    {
      j=i

      while ((df_segment$car[i]==0 | df_segment$uptime[i]<uptime_choice | is.na(df_segment$car[i]) )& i<length(df_segment$car))
      {i<-i+1}

      diff_days<-abs(as.numeric(difftime(df_segment$day[i], df_segment$day[j], units = "days")))

      if(diff_days>successive_day)
      {
        if(retrieve_data==TRUE)
        {
          df_segment <- df_segment %>%
            mutate_at(vars(heavy, car, bike,pedestrian,heavy_lft,heavy_rgt,car_lft,
                           car_rgt,bike_lft,bike_rgt,pedestrian_lft,pedestrian_rgt),
                      ~ ifelse(row_number() %in% min(i,j):max(i,j), NA,.))
        }
        else
        {

          df_segment <- df_segment %>%
            mutate_at(vars(heavy_NA, car_NA, bike_NA,pedestrian_NA),
                      ~ ifelse(row_number() %in% min(i,j):max(i,j), NA,.))
        }

      }
    }
    list_clear_data[[id]]<-df_segment
  }
  enriched_data<-list_clear_data[[1]]

  if(length(seg_id)>1)
  {
    for(i in 2:length(seg_id))
    {enriched_data<-rbind(enriched_data,list_clear_data[[i]])}
  }

  return(enriched_data)
}
