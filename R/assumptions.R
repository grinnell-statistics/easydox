#' A box plot to check the equal variances assumption in ANOVA
#'
#' This function gives a box plot to check the within groups equal variances assumptions.
#' The first factor x1 is the x-axis, and if the second factor x2 exists,
#' the box plot will be colored by x2.
#' @param formula the value of interest ~ the first factor
#' @param dataset the dataset that contains the experiment information
#' @param color colored by this factor (optional)
#' @param facet faceted by this factor (optional)
#' @return a box plot
#' @importFrom ggplot2 ggplot aes geom_boxplot theme element_text
#' @export
dox_boxplot = function(formula, dataset, color=NULL, facet = NULL){
  response = all.vars(formula)[1]
  x1 = all.vars(formula)[2]
  ggplot(data = dataset, aes(x = .data[[x1]], y = .data[[response]])) +
    geom_boxplot() +
    aes(colour = {{color}}) +
    theme(axis.title=element_text(size=14,face="bold"), axis.text.x = element_text(size = 12, angle = 45))+facet_grid(vars({{facet}}))
}


#' A scatterplot to check the equal variances assumption in ANOVA
#'
#' This function gives a scatterplot to check the within groups equal variances assumptions.
#' The first factor x1 is the x-axis, and if the second factor x2 exists,
#' the scatterplot will be colored by x2.

#' @param formula the value of interest ~ the first factor
#' @param dataset the dataset that contains the experiment information
#' @param color colored by this factor (optional)
#' @param facet faceted by this factor (optional)
#' @param jitter whether to use geom_jitter
#' @return a scatterplot
#' @importFrom ggplot2 ggplot aes geom_point theme stat_summary element_text
#' @export

dox_scatterplot = function(formula, dataset, color=NULL, facet = NULL, jitter = FALSE){
  if (!(jitter)){
    response = all.vars(formula)[1]
    x1 = all.vars(formula)[2]
    ggplot(dataset,  aes(x = .data[[x1]], y = .data[[response]], color={{color}})) +
      geom_point() +
      stat_summary(
        fun = "mean",
        geom = "point",
        col = "black",
        size = 2,
        shape = 24,
        fill = "red"
      )+theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))+facet_grid(vars({{facet}}))
  }

  else{
    response = all.vars(formula)[1]
    x1 = all.vars(formula)[2]
    ggplot(dataset,  aes(x = .data[[x1]], y = .data[[response]], color={{color}})) +
      geom_jitter() +
      stat_summary(
        fun = "mean",
        geom = "point",
        col = "black",
        size = 2,
        shape = 24,
        fill = "red"
      )+theme(axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12, angle = 45))+facet_grid(vars({{facet}}))
  }
}


#' An interactive table to show variances of different groups
#'
#' This function gives an interactive table to show variances of different groups.
#' It can help check the within groups equal variances assumptions. You can click column names to sort.
#' @param dataset the dataset that contains the experiment information
#' @param response the value of interest
#' @param x1 the first factor
#' @param x2 the second factor (optional)
#' @param x3 the third factor (optional)
#' @return an interactive variance table
#' @importFrom dplyr group_by summarise %>% mutate_if n
#' @importFrom reactable reactable colDef
#' @importFrom reactablefmtr data_bars
#' @export
dox_table = function(dataset, response, x1, x2=NULL, x3=NULL){
  summary_table = group_by(dataset, {{x1}},{{x2}},{{x3}}) %>%
    summarise(GroupVariance=var({{response}}),
              SampleSize=n(), .groups = 'drop')
  summary_df = as.data.frame(summary_table)
  summary_df = summary_df %>%
    mutate_if(is.numeric, round, digits = 2)


    reactable(summary_df,defaultColDef = colDef(cell = data_bars(summary_df, box_shadow = TRUE, round_edges = TRUE,
                                                                 text_position = "outside-base",
                                                                 fill_color = c("#e81cff", "#40c9ff"),
                                                                 background = "#e5e5e5",fill_gradient = TRUE)))

    #as.datatable(formattable(summary_df, list(SampleSize = color_bar("#80ed99"),GroupVariance = color_bar("#f28482"))))

}



#' A qqplot and a histogram for residuals
#'
#' This function gives a qqplot and a histogram for residuals to check the normality assumption of ANOVA.
#' You can either use the residuals as the parameters: normal_err(residuals, bins = 30) or you can use a similar pattern
#' as other functions normal_err(dataset, response, ...).
#' @param dataset_residual the dataset that contains the experiment information or the residuals
#' @param response the value of interest
#' @param x1 the first factor
#' @param x2 the second factor (optional)
#' @param interaction whether interaction x1*x2 is considered
#' @param bins the number of bins
#' @return A qqplot and a histogram for residuals
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line labs geom_histogram
#' @importFrom gridExtra grid.arrange
#' @import rlang
#' @export
normal_err = function(dataset_residual, response, x1, x2=NULL, interaction = TRUE, bins = 30){
  if (nargs()>2){

    library(rlang)
    response <- parse_expr(quo_name(enquo(response)))
    x1 <- parse_expr(quo_name(enquo(x1)))
    x2 <- parse_expr(quo_name(enquo(x2)))
    if (interaction == TRUE){
      aov1=eval_tidy(expr(aov(!!response ~ !!x1*!!x2, data = dataset_residual)))
    }
    else{
      aov1=eval_tidy(expr(aov(!!response ~ !!x1+!!x2, data = dataset_residual)))
    }

    residual_df = as.data.frame(aov1$residuals)
    colnames(residual_df) = c("residual")

  }
  else{
    residual_df = as.data.frame(dataset_residual)
    colnames(residual_df) = c("residual")
  }

  qqplot <- ggplot(residual_df, aes(sample = residual))
  qqplot = qqplot + stat_qq() + stat_qq_line() + labs(title="QQPLOT for Error Terms")

  hist = ggplot(residual_df, aes(x=residual)) + geom_histogram(bins=bins, fill="lightblue")+ labs(title="Histogram for Error Terms")
  grid.arrange(qqplot, hist, ncol=1)

}




#' Residual vs fit/order plots
#'
#' This function gives a residual versus fit plot and a residual versus order plot to check the
#' "independent and identically distributed observations" assumption in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @param anova_model the anova model
#' @return A residual versus fit plot and a residual versus order plot
#' @importFrom ggplot2 ggplot aes geom_point geom_hline xlab ylab
#' @importFrom gridExtra grid.arrange
#' @export
iid = function(dataset, anova_model){
  if(!is.null(anova_model$residuals)){
    residual_fitted = ggplot(anova_model, aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("fitted value")+ylab("residual")

    dataset$rownum = 1:dim(dataset)[1]
    residual_order = ggplot(anova_model, aes(x=dataset$rownum, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("row number")+ylab("residual")
  }
  else{
    fits  = anova_model$Within$fitted.values
    resids  = anova_model$Within$residuals
    aov_data = data.frame(fits,resids)

    residual_fitted = ggplot(aov_data,aes(x = fits, y = resids)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("fitted value")+ylab("residual")

    aov_data$rownum = 1:dim(aov_data)[1]
    residual_order = ggplot(aov_data, aes(x=rownum, y = resids)) +
      geom_point() +
      geom_hline(yintercept = 0)+xlab("row number")+ylab("residual")
  }


  grid.arrange(residual_fitted, residual_order, ncol=1)
}
