#' Main Effect Plots
#'
#' This function gives the main effect plots for one or more treatments in an experiment.
#' If more than 1 treatment is given and ylim is not specified, plots will share the same y-axis range.
#' @param formula y~x1+x2(optional)+x3(optional)
#' @param dataset the dataset that contains the experiment information
#' @param label Label "Mean" or "Effect"
#' @param text_size text size of the x-axis
#' @param ylim a vector of the range of the y-axis
#' @return main effect plots
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point coord_cartesian element_text element_blank
#' @importFrom dplyr group_by summarise %>%
#' @importFrom gridExtra grid.arrange
#' @export
#' @examples
#' dox_main(LogStrength ~ Brand + Water, Towels2, label="Mean")
#' # If you want the label to be effect and have a larger size for the x-axis
#' dox_main(LogStrength ~ Brand + Water, Towels2, label="Effect", text_size = 14)
dox_main = function(formula, dataset, label="Mean", text_size=12, ylim){
  formula=as.formula(formula)
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  x2 = all.vars(formula)[3]
  x3 = all.vars(formula)[4]
  x4 = all.vars(formula)[5]
  u = mean(dataset[[response]])
  if(is.numeric(dataset[[x1]])){
    error_message = paste("Variable \"", x1, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if(is.numeric(dataset[[x2]])){
    error_message = paste("Variable \"", x2, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if(is.numeric(dataset[[x3]])){
    error_message = paste("Variable \"", x3, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if(is.numeric(dataset[[x4]])){
    error_message = paste("Variable \"", x4, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  # ylim if not specified
  y_min = 0
  y_max = 0

  ## mean for x1
  df1 <- dataset %>%
    group_by(.data[[x1]]) %>%
    summarise(Mean_Response = mean(.data[[response]]))

  y_min = min(df1$Mean_Response)
  y_max = max(df1$Mean_Response)

  ## mean for x2
  if(!is.na(x2)){
    df2 <- dataset %>%
      group_by(.data[[x2]]) %>%
      summarise(Mean_Response = mean(.data[[response]]))
    y_min = min(df2$Mean_Response,y_min)
    y_max = max(df2$Mean_Response,y_max)
  }

  ## mean for x3
  if(!is.na(x3)){
    df3 <- dataset %>%
      group_by(.data[[x3]]) %>%
      summarise(Mean_Response = mean(.data[[response]]))
    y_min = min(df3$Mean_Response,y_min)
    y_max = max(df3$Mean_Response,y_max) }

  ## mean for x4
  if(!is.na(x4)){
    df4 <- dataset %>%
      group_by(.data[[x4]]) %>%
      summarise(Mean_Response = mean(.data[[response]]))
    y_min = min(df4$Mean_Response,y_min)
    y_max = max(df4$Mean_Response,y_max) }

  if (label=="Mean" | label =="Effect"){
    y_max=y_max+(y_max-y_min)*0.15
  }


  ## main plot for x1
  p1 <- ggplot(df1, aes(.data[[x1]], Mean_Response)) +
    geom_line(aes(group = 1)) +
    theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = text_size))+
    geom_point()

  if(missing(ylim)){
    p1 <- p1+coord_cartesian(ylim = c(y_min, y_max))
  }
  else{
    p1 <- p1+coord_cartesian(ylim = ylim)
  }

  if(label=="Mean"){
    p1 <- p1+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.3e', Mean_Response), round(Mean_Response,4))),vjust = -1)
  }
  else if(label=="Effect"){
    p1 <- p1+geom_text(aes(label=ifelse(((abs(Mean_Response-u) > 1e4) | (abs(Mean_Response-u) < 0.01)), sprintf('%.3e', Mean_Response-u), round(Mean_Response-u,4))),vjust = -1)
  }




  ## main plot for x2
  if(!is.na(x2)){
    p2 <- ggplot(df2, aes(.data[[x2]], Mean_Response)) +
      theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = text_size))+
      geom_line(aes(group = 1)) +
      geom_point()


    if(missing(ylim)){
      p2 <- p2 + coord_cartesian(ylim = c(y_min, y_max))
    }
    else{
      p2 <- p2 + coord_cartesian(ylim = ylim)
    }

    if(label=="Mean"){
      p2 <- p2+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.3e', Mean_Response), round(Mean_Response,4))),vjust = -1)
    }
    else if(label=="Effect"){
      p2 <- p2+geom_text(aes(label=ifelse(((abs(Mean_Response-u) > 1e4) | (abs(Mean_Response-u) < 0.01)), sprintf('%.3e', Mean_Response-u), round(Mean_Response-u,4))),vjust = -1)
    }
  }




  ## main plot for x3
  if(!is.na(x3)){
    p3 <- ggplot(df3, aes(.data[[x3]], Mean_Response)) +
      theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = text_size))+
      geom_line(aes(group = 1)) +
      geom_point()

    if(missing(ylim)){
      p3 <- p3+ coord_cartesian(ylim = c(y_min, y_max))
    }
    else{
      p3 <- p3+coord_cartesian(ylim = ylim)
    }

    if(label=="Mean"){
      p3 <- p3+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.3e', Mean_Response), round(Mean_Response,4))),vjust = -1)
    }
    else if(label=="Effect"){
      p3 <- p3+geom_text(aes(label=ifelse(((abs(Mean_Response-u) > 1e4) | (abs(Mean_Response-u) < 0.01)), sprintf('%.3e', Mean_Response-u), round(Mean_Response-u,4))),vjust = -1)
    }
  }

  ## main plot for x4
  if(!is.na(x4)){
    p4 <- ggplot(df4, aes(.data[[x4]], Mean_Response)) +
      theme(axis.title=element_text(size=14,face="bold"),axis.title.y = element_blank(),axis.text.x = element_text(size = text_size))+
      geom_line(aes(group = 1)) +
      geom_point()

    if(missing(ylim)){
      p4 <- p4+ coord_cartesian(ylim = c(y_min, y_max))
    }
    else{
      p4 <- p4+coord_cartesian(ylim = ylim)
    }

    if(label=="Mean"){
      p4 <- p4+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.3e', Mean_Response), round(Mean_Response,4))),vjust = -1)
    }
    else if(label=="Effect"){
      p4 <- p4+geom_text(aes(label=ifelse(((abs(Mean_Response-u) > 1e4) | (abs(Mean_Response-u) < 0.01)), sprintf('%.3e', Mean_Response-u), round(Mean_Response-u,4))),vjust = -1)
    }
  }


  ## Plot
  if(!is.na(x2) && !is.na(x3) && !is.na(x4)){
    grid.arrange(p1, p2, p3, p4, nrow = 1)
  }
  else if(!is.na(x2) && !is.na(x3)){
    grid.arrange(p1, p2, p3,nrow = 1)
  }
  else if (!is.na(x2)){
    grid.arrange(p1, p2,nrow = 1)
  }
  else{
    p1
  }
}

#' Interaction Effect Plot
#'
#' This function gives the interaction effect plots for two treatments in an experiment.
#' @param formula y~x1+x2
#' @param dataset the dataset that contains the experiment information
#' @param label Label "Mean" or "Effect"
#' @param text_size text size of the x-axis
#' @return interaction effect plot
#' @importFrom ggplot2 ggplot aes geom_line theme geom_point element_text labs facet_grid vars
#' @importFrom dplyr group_by summarise %>%
#' @export
#' @examples
#' dox_inter(LogStrength ~ Brand + Water, Towels2, label="Mean", text_size = 14)
#' # If you want the label to be effect and have a larger size for the x-axis
#' dox_inter(LogStrength ~ Brand + Water, Towels2, label="Effect", text_size = 14)
dox_intera = function(formula, dataset, label="Mean", text_size = 12){
  #formula=as.formula(formula)
  #response = all.vars(formula)[1]
  #x1 = all.vars(formula)[2]
  #x2 = all.vars(formula)[3]
  vars<- all.vars(formula)
  response<-vars[1]
  term_labels<- terms(formula,keep.order=TRUE)
  c<-attr(term_labels,"term.labels")
  int <- list()
  err<- list()
  main<-vector()
  for (item in c){
    if (grepl(":",item)){
      int[[item]]<- strsplit(item,":")[[1]]
    } else if (grepl("Error\\(",item)){
      t<-gsub("^Error\\(|\\)$", "", item)
      parts<- strsplit(t,"/")[[1]]
      before <- unlist(strsplit(parts[1], "\\*"))
      after <- gsub("as.factor\\((.*)\\)", "\\1", parts[2])
      err[[item]] <- list(
        before = before,
        after  = after
      )
      
    } else{
      main<- append(main,item)
    }
  }
  p<-vector()
  pairs <- t(combn(main, 2))
  for (index in 1:dim(pairs)[1]){
    x1<-pairs[index,][1]
    x2<-pairs[index,][2]
    if(is.numeric(dataset[[x1]])){
      error_message = paste("Variable \"", x1, "\" needs to be a factor. Currently numeric.")
      stop(error_message)
    }
    
    if(is.numeric(dataset[[x2]])){
      error_message = paste("Variable \"", x2, "\" needs to be a factor. Currently numeric.")
      stop(error_message)
    }
    
    
    df <- dataset %>%
      group_by(.data[[x1]], .data[[x2]]) %>%
      summarise(Mean_Response = mean(.data[[response]]),.groups = 'drop')
    
    y_min = min(df$Mean_Response)
    y_max = max(df$Mean_Response)
    if (label=="Mean" | label =="Effect"){
      y_max=y_max+(y_max-y_min)*0.15
    }
    # calculate means
    x1_mean <- dataset %>%
      group_by(.data[[x1]]) %>%
      summarise(x1_mean = mean(.data[[response]]))
    
    x2_mean <- dataset %>%
      group_by(.data[[x2]]) %>%
      summarise(x2_mean = mean(.data[[response]]))
    
    inter_mean <- dataset %>%
      group_by(.data[[x1]], .data[[x2]]) %>%
      summarise(inter_mean = mean(.data[[response]]))
    
    u <- mean(dataset[[response]])
    
    # merge table
    inter_mean = merge(inter_mean, x1_mean, by = x1, all.x = TRUE)
    inter_mean = merge(inter_mean, x2_mean, by = x2, all.x = TRUE)
    inter_mean$effect = inter_mean$inter_mean-inter_mean$x1_mean-inter_mean$x2_mean+u
    interaction_effect = inter_mean$effect
    
    p1 = ggplot(df, aes(.data[[x1]], Mean_Response, color = .data[[x2]])) +
      theme(axis.title=element_text(size=14,face="bold"),
            axis.text.x = element_text(size = text_size))+
      geom_line(aes(group = .data[[x2]])) +
      geom_point() +
      coord_cartesian(ylim = c(y_min, y_max))
    
    if(label=="Mean"){
      p1 <- p1+geom_text(aes(label=ifelse(((abs(Mean_Response) > 1e4) | (abs(Mean_Response) < 0.01)), sprintf('%.4e', Mean_Response), round(Mean_Response,2))),vjust = -1)
    }
    
    else if(label=="Effect"){
      p1 <- p1+geom_text(aes(label=ifelse(((abs(interaction_effect) > 1e4) | (abs(interaction_effect) < 0.01)), sprintf('%.3e', interaction_effect), round(interaction_effect,4))),vjust = -1)
    }
    p<-append(p,p1)
  }
  p  
  
}
