#' A box plot to check the equal variances assumption in ANOVA.
#'
#' This function gives a box plot to check the within groups equal variances assumptions.
#' It will color by x2 is x2 exists in the formula and color= is not specified.
#' @param formula y~x
#' @param dataset the dataset that contains the experiment information
#' @param color colored by this factor (optional)
#' @param facet faceted by this factor (optional)
#' @return a box plot
#' @importFrom ggplot2 ggplot aes geom_boxplot theme element_text facet_grid vars
#' @importFrom rlang is_formula
#' @export
#' @examples
#' dox_boxplot(LogStrength ~ Brand, Towels2, color = Water)
#' # this is equivalent to
#' dox_boxplot(LogStrength ~ Brand + Water, Towels2)
#'
#' # if you want to use facet
#' dox_boxplot(LogStrength ~ Brand, Towels2, facet = Water)
# # if you want to use two-dimensional facet
#' dox_boxplot(LogStrength ~ Brand, Towels2, facet = Brand~Water)

dox_boxplot = function(formula, dataset, color=NULL, facet = NULL){
  formula=as.formula(formula)
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  x2 = all.vars(formula)[3]

  if(is.numeric(dataset[[x1]])){
    error_message = paste("Variable \"", x1, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }
  color_str = deparse(substitute(color))
  if(color_str!="NULL" && (is.numeric(dataset[,color_str]))){
    error_message = paste("Variable \"", color_str, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if(color_str=="NULL" && !is.na(x2)){
    color_str=x2
  }

  p1=ggplot(data = dataset, aes(x = .data[[x1]], y = .data[[response]])) +
    geom_boxplot() +
    theme(axis.title=element_text(size=14,face="bold"), axis.text.x = element_text(size = 12, angle = 45))


  facet_str = deparse(substitute(facet))
  if (grepl("~", facet_str)){
    p1 = p1+facet_grid(facet_str)
  }
  else if (facet_str != "NULL"){
    if(facet_str != "NULL" && is.numeric(dataset[,facet_str])){
      error_message = paste("Variable \"", facet_str, "\" needs to be a factor. Currently numeric.")
      stop(error_message)
    }
    facet_formula = as.formula(paste(". ~ ",facet_str))
    p1=p1+facet_grid(facet_formula)
  }

  if(color_str!="NULL"){
    p1=p1+aes(colour = .data[[color_str]])

    # p1=p1+stat_summary(aes(group = interaction(.data[[x1]], .data[[color_str]])),
    #                    fun = mean,
    #                    geom = "point",
    #                    shape = 20,
    #                    size = 4,
    #                    color = "red",
    #                    fill = "red",
    #                    show.legend = FALSE)
  }

  p1
  }


#' A scatterplot to check the equal variances assumption in ANOVA
#'
#' This function gives a scatterplot to check the within groups equal variances assumptions.
#' @param formula y~x
#' @param dataset the dataset that contains the experiment information
#' @param color colored by this factor (optional)
#' @param facet faceted by this factor (optional)
#' @param jitter the width of jitter or FALSE (not to use jitter)
#' @return a scatterplot
#' @importFrom ggplot2 ggplot aes geom_point theme stat_summary element_text facet_grid vars geom_jitter
#' @importFrom rlang is_formula
#' @export
#' @examples
#' dox_scatterplot(LogStrength ~ Brand, Towels2, color = Water)
#' # if you want to use jitter
#' dox_scatterplot(LogStrength ~ Brand, Towels2, color = Water, jitter = 0.05)
#'
#' # if you want to use facet
#' dox_scatterplot(LogStrength ~ Brand, Towels2, facet = Water, jitter = 0.15)
#  # if you want to use two-dimensional facet
#' dox_scatterplot(LogStrength ~ Brand, Towels2, facet = Brand~Water, jitter = 0.15)

dox_scatterplot = function(formula, dataset, color=NULL, facet = NULL, jitter = FALSE){
  formula=as.formula(formula)
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  x2 = all.vars(formula)[3]


  if(is.numeric(dataset[[x1]])){
    error_message = paste("Variable \"", x1, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }
  color_str = deparse(substitute(color))
  if(color_str!="NULL" && (is.numeric(dataset[,color_str]))){
    error_message = paste("Variable \"", color_str, "\" needs to be a factor. Currently numeric.")
    stop(error_message)
  }

  if (!(jitter)){
    p1=ggplot(dataset,  aes(x = .data[[x1]], y = .data[[response]])) +
      geom_point() +theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))
  }

  else{
    p1=ggplot(dataset,  aes(x = .data[[x1]], y = .data[[response]])) +
      geom_jitter(width = {{jitter}})+theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))
  }

  facet_str = deparse(substitute(facet))
  if (grepl("~", facet_str)){
    p1 = p1+facet_grid(facet_str)
  }
  else if (facet_str != "NULL"){
    if(facet_str != "NULL" && is.numeric(dataset[,facet_str])){
      error_message = paste("Variable \"", facet_str, "\" needs to be a factor. Currently numeric.")
      stop(error_message)
    }
    facet_formula = as.formula(paste(". ~ ",facet_str))
    p1=p1+facet_grid(facet_formula)
  }

  if(color_str=="NULL" && !is.na(x2)){
    color_str=x2
  }

  if(color_str!="NULL"){
    p1=p1+aes(colour = .data[[color_str]])
  }
  p1

}



#' An interactive table to show standard deviations of different groups
#'
#' This function gives an interactive table to show standard deviations of different groups.
#' It can help check the within groups equal variances assumptions. You can click column names to sort.
#' @param formula y~x1+x2(optional)+x3(optional)
#' @param dataset the dataset that contains the experiment information
#' @return an interactive table of standard deviations
#' @importFrom dplyr group_by summarise %>% mutate_if n
#' @importFrom reactable reactable colDef
#' @importFrom reactablefmtr data_bars
#' @export
#' @examples
#' # Note that "Run examples" won't work because
#' # of calling the reactable package
#' dox_table(LogStrength ~ Brand + Water, Towels2)
dox_table = function(formula, dataset){
  formula=as.formula(formula)
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  x2 = all.vars(formula)[3]
  x3 = all.vars(formula)[4]
  x4 = all.vars(formula)[5]

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

  if (is.na(x2)){
    data_groupby = group_by(dataset, .data[[x1]])
  }
  else if (is.na(x3)){
    data_groupby = group_by(dataset, .data[[x1]], .data[[x2]])
  }
  else if(is.na(x4)){
    data_groupby = group_by(dataset, .data[[x1]], .data[[x2]],.data[[x3]])
  }
  else{
    data_groupby = group_by(dataset, .data[[x1]], .data[[x2]],.data[[x3]],.data[[x4]])
  }

  summary_table = data_groupby %>%
    summarise(StandardDeviation=sd(.data[[response]]),
              SampleSize=n(), .groups = 'drop')
  summary_df = as.data.frame(summary_table)
  summary_df = summary_df %>%
    mutate_if(is.numeric, round, digits = 2)


  reactable(summary_df,defaultColDef = colDef(cell = data_bars(summary_df, box_shadow = TRUE, round_edges = TRUE,
                                                               text_position = "outside-base",
                                                               fill_color = c("#40c9ff"),
                                                               background = "#e5e5e5",fill_gradient = FALSE)))

  #as.datatable(formattable(summary_df, list(SampleSize = color_bar("#80ed99"),GroupVariance = color_bar("#f28482"))))

}


#' Residual vs fit/order plots
#'
#' This function gives four plots to check the "independent and identically distributed observations"
#' and "normality" assumption in ANOVA. 1.a qqplot for residuals; 2.a histogram for residuals;
#' 3.residual versus fit plot; 4.residual versus order plot.
#' Also, this function invisibly returns the dataset with residuals and fitted values.
#' @param formula formula used in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @param plot which of the four plots to show. Default is to show all four
#' @param bins the number of bins in histogram
#' @param model_name model name used in the column names for residuals and fitted values
#' @return A qqplot and a histogram for residuals; a residual versus fit plot and a residual versus order plot
#' @importFrom ggplot2 ggplot aes geom_point geom_hline xlab ylab stat_qq stat_qq_line labs geom_histogram
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr group_by summarise %>% n
#' @export
#' @examples
#' dox_resid(LogStrength~Brand*Water, Towels2)
#' # If you want to get the dataset with residuals and fitted values
#' Towels_copy = dox_resid(LogStrength~Brand*Water, Towels2, model_name="model1")
#' # If you want to check a specific plot, use plot =
#' dox_resid(LogStrength~Brand*Water, Towels2, plot = 2, bins = 40)
dox_resida = function(formula, dataset, plot = "All", bins = 10){
  formula=as.formula(formula)
  # give warnings if the experiment is not balanced
  counts_table <- dataset %>%
    group_by(across(all.vars(formula)[-1])) %>%
    summarise(n = n())
  
  if(!all(counts_table$n[1] == counts_table$n)){
    warning("Your experiment is not balanced and the result can be misleading. The aov() function used here conducts Type I ANOVA, which only works for balanced design. We recommend using Anova() in the 'car' package to conduct Type II/III ANOVA.")
    print(counts_table)
  }
  
  anova_model=aov(formula, dataset)
  
  # not split-plot design
  if(!is.null(anova_model$residuals)){
    print("Non SPLIT PLOT")
    fits  = anova_model$fitted.values
    resids  = anova_model$residuals
    
    # res vs fit
    residual_fitted = ggplot(anova_model, aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("fitted value")+ylab("residual")+ labs(title="Residual vs Fit")
    
    # res vs order
    dataset$rownum = 1:dim(dataset)[1]
    residual_order = ggplot(anova_model, aes(x=dataset$rownum, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("row number")+ylab("residual")+ labs(title="Residual vs Order")
    
    # res qqplot
    residual_df = as.data.frame(anova_model$residuals)
    colnames(residual_df) = c("residual")
    qqplot <- ggplot(residual_df, aes(sample = residual))
    qqplot = qqplot + stat_qq() + stat_qq_line() +
      labs(title = "QQ Plot for Error Terms", x = "Theoretical", y = "Sample")
    
    
    # res histogram
    hist = ggplot(residual_df, aes(x=residual)) + geom_histogram(bins=bins, fill="lightblue")+ labs(title="Histogram for Error Terms")
  }
  # split-plot design
  else{
    print("SPLIT PLOT")
    #This part calculating the residuals and fitted values for split plot designs is taken from the split_aov()
    vars<- all.vars(formula)
    y<-vars[1]
    x<-vars[-1]
    term_labels<- terms(formula,keep.order=TRUE)
    c<-attr(term_labels,"term.labels")
    
    
    int <- list()
    err<- list()
    main<-list()
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
        main[[item]]<-item
      }
    }
    
    
    main<-unlist(main)
    int<-unlist(int)
    gm <- mean(dataset[[y]])
    err[[1]]$after<- gsub("\\s+", "", err[[1]]$after)
    err[[1]]$before<- gsub("\\s+", "", err[[1]]$before)
    
    
    
    for (index in seq_along(main)){
      fmla<- as.formula(paste(y,"~",main[[index]]))
      means<-  aggregate(fmla, data = dataset, FUN = mean)
      dataset[[paste0("avg_",main[[index]])]] <- means[[y]][
        match(dataset[[main[[index]]]], means[[main[[index]]]])
      ]
      dataset[[paste0("effect_",main[[index]])]] <- dataset[[paste0("avg_",main[[index]])]] -gm
      
    }
    
    
    
    for (i in seq(1,length(int),by=2)){
      pair<- int[i:(i+1)]
      fmla_int<- as.formula(paste(y,"~",pair[[1]],"*",pair[[2]]))
      means<-  aggregate(fmla_int, data = dataset, FUN = mean)
      dataset[[paste0("avg_inter_",pair[[1]],"_",pair[[2]])]] <- means[[y]][
        match(
          interaction(dataset[[pair[[1]]]], dataset[[pair[[2]]]]),
          interaction(means[[pair[[1]]]], means[[pair[[2]]]])
        )
      ]
      dataset[[paste0("effect_",pair[[1]],"_",pair[[2]])]] <- dataset[[paste0("avg_inter_",pair[[1]],"_",pair[[2]])]]-
        dataset[[paste0("avg_",pair[[1]])]] - dataset[[paste0("avg_",pair[[2]])]] +gm
    }
    
    
    
    fmla_err<- as.formula(paste(y,"~",err[[1]]$after))
    means_err<-  aggregate(fmla_err, data = dataset, FUN = mean)
    dataset[[paste0("avg_wp_err_", err[[1]]$after)]] <- means_err[[y]][
      match(dataset[[err[[1]]$after]], means_err[[err[[1]]$after]])
    ]
    
    if (length(err[[1]]$before) == 1){
      dataset[[paste0("effect_",err[[1]]$after)]] <- dataset[[paste0("avg_wp_err_",err[[1]]$after)]]  - dataset[[paste0("avg_",err[[1]]$before)]]
    } else if (length(err[[1]]$before) ==2){
      dataset[[paste0("effect_",err[[1]]$after)]] <- dataset[[paste0("avg_wp_err_",err[[1]]$after)]] - dataset[[paste0("avg_inter_",err[[1]]$before[1],"_",err[[1]]$before[2])]]
    }
    
    
    
    cols<- grep("^effect", names(dataset))
    
    fits <-  rowSums(dataset[,cols])+gm
    resids<- dataset[[y]]- fits
    #creating a dataframe with the residuals and fitted values
    aov_data = data.frame(fits,resids)
    print(aov_data)
    # res vs fit
    residual_fitted = ggplot(aov_data,aes(x = fits, y = resids)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("fitted value")+ylab("residual")+ labs(title="Residual vs Fit")
    # res vs order
    aov_data$rownum = 1:dim(aov_data)[1]
    residual_order = ggplot(aov_data, aes(x=rownum, y = resids)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("row number")+ylab("residual")+ labs(title="Residual vs Order")
    
    # res qqplot
    residual_df = as.data.frame(resids)
    colnames(residual_df) = c("residual")
    qqplot <- ggplot(residual_df, aes(sample = residual))
    qqplot = qqplot + stat_qq() + stat_qq_line() +
      labs(title = "QQ Plot for Error Terms", x = "Theoretical", y = "Sample")
    
    # res histogram
    hist = ggplot(residual_df, aes(x=residual)) + geom_histogram(bins=bins, fill="lightblue")+ labs(title="Histogram for Error Terms")
    
  }
  
  if ({{plot}} == "All")
  {grid.arrange(qqplot, hist, residual_fitted, residual_order, ncol=2)}
  else if({{plot}} == 1) {qqplot}
  else if({{plot}} == 2) {hist}
  else if({{plot}} == 3) {residual_fitted}
  else {residual_order}
  
}

  



#' Summary Statistics
#'
#' This function applies the favstats function in mosaic package and gives summary statistics.
#' @param formula y~x1+x2
#' @param dataset the dataset that contains the experiment information
#' @return A table that gives summary statistics about y partitioned by treatments
#' @importFrom mosaic favstats
#' @importFrom stringr str_replace
#' @export
#' @examples
#' dox_sumstats(LogStrength ~ Brand + Water, Towels2)
dox_sumstats = function(formula, dataset){
  formula=as.formula(formula)
  formula_str = deparse(substitute(formula))
  formula_str=str_replace(formula_str, "\\*", "+")
  favstats(as.formula(formula_str), data=dataset)
}
