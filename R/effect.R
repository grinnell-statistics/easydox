#' Main Effect Plots
#'
#' This function gives the main effect plots for one or more treatments in an experiment.
#' If more than 1 treatment is given and ylim is not specified, plots will share the same y-axis range.
#' @param dataset the dataset that contains the experiment information
#' @param response the value of interest
#' @param x1 the first factor
#' @param x2 the second factor (optional)
#' @param x3 the third factor (optional)
#' @param ylim a vector of the range of the y-axis
#' @return main effect plots
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point coord_cartesian element_text
#' @importFrom dplyr group_by summarise %>%
#' @importFrom gridExtra grid.arrange
#' @export
dox_main = function(dataset, response, x1, x2, x3, ylim){
  library(dplyr)
  library(ggplot2)

  # ylim if not specified
  y_min = 0
  y_max = 0

  ## mean for x1
  df1 <- dataset %>%
    group_by({{x1}}) %>%
    summarise(Mean_Response = mean({{response}}))

  y_min = min(df1$Mean_Response)
  y_max = max(df1$Mean_Response)

  ## mean for x2
  if(!missing(x2)){
    df2 <- dataset %>%
      group_by({{x2}}) %>%
      summarise(Mean_Response = mean({{response}}))
    y_min = min(df2$Mean_Response,y_min)
    y_max = max(df2$Mean_Response,y_max)
  }

  ## mean for x3
  if(!missing(x3)){
    df3 <- dataset %>%
      group_by({{x3}}) %>%
      summarise(Mean_Response = mean({{response}}))
    y_min = min(df3$Mean_Response,y_min)
    y_max = max(df3$Mean_Response,y_max) }

  ## main plot for x1
  if(missing(ylim)){
    p1 <- ggplot(df1, aes({{x1}}, Mean_Response)) +
      geom_line(aes(group = 1)) +
      theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))+
      geom_point() + coord_cartesian(ylim = c(y_min, y_max))
  }
  else{
    p1 <- ggplot(df1, aes({{x1}}, Mean_Response)) +
      geom_line(aes(group = 1)) +
      theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))+
      geom_point() +
      coord_cartesian(ylim = ylim)
  }



  ## main plot for x2
  if(!missing(x2)){
    if(missing(ylim)){
      p2 <- ggplot(df2, aes({{x2}}, Mean_Response)) +
        theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = 12, angle = 45))+
        geom_line(aes(group = 1)) +
        geom_point() + coord_cartesian(ylim = c(y_min, y_max))
    }
    else{
      p2 <- ggplot(df2, aes({{x2}}, Mean_Response)) +
        theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = 12, angle = 45))+
        geom_line(aes(group = 1)) +
        geom_point() +
        coord_cartesian(ylim = ylim)
    }
  }


  ## main plot for x3
  if(!missing(x3)){
    if(missing(ylim)){
      p3 <- ggplot(df3, aes({{x3}}, Mean_Response)) +
        theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = 12, angle = 45))+
        geom_line(aes(group = 1)) +
        geom_point() + coord_cartesian(ylim = c(y_min, y_max))
    }
    else{
      p3 <- ggplot(df3, aes({{x3}}, Mean_Response)) +
        theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = 12, angle = 45))+
        geom_line(aes(group = 1)) +
        geom_point() +
        coord_cartesian(ylim = ylim)
    }
  }
  ## Plot
  if(!missing(x2) && !missing(x3)){
    grid.arrange(p1, p2, p3,nrow = 1)
  }
  else if (!missing(x2)){
    grid.arrange(p1, p2,nrow = 1)
  }
  else{
    p1
  }
}


#' Interaction Effect Plot
#'
#' This function gives the interaction effect plots for two treatments in an experiment.
#' @param dataset the dataset that contains the experiment information
#' @param response the value of interest
#' @param x1 the first factor
#' @param x2 the second factor
#' @return interaction effect plot
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point element_text labs
#' @importFrom dplyr group_by summarise %>%
#' @export
dox_inter = function(dataset, response, x1, x2){
  library(dplyr)
  library(ggplot2)
  df <- dataset %>%
    group_by({{x1}}, {{x2}}) %>%
    summarise(Mean_Response = mean({{response}}),.groups = 'drop')

  ggplot(df, aes({{x1}}, Mean_Response, color = {{x2}})) +
    theme(axis.title=element_text(size=14,face="bold"),
          axis.text.x = element_text(size = 12, angle = 45))+
    geom_line(aes(group = {{x2}})) +
    geom_point() +
    labs(title="Interaction Plot")
}
