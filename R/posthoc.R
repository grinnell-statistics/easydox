#' An ANOVA summary table specifically for SPlit Plot designs
#'
#' This function gives an ANOVA summary table with total degrees of freedom and sum of squares
#' @param formula formula used in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @return a html table and residuals,fitted values
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable kable_styling
#' @importFrom dplyr group_by summarise %>% n
#' @export
dox_split_aov=function(formula, dataset){
  formula=as.formula(formula)
  model_1=aov(formula, dataset)
  if(length(summary(model_1)) == 1){
    warning("This function only works for a Split-Plot design. Your formula does not have any Error() term. Use dox_aov() instead")
  }
  
  # give warnings if the experiment is not balanced
  counts_table <- dataset %>%
    group_by(across(all.vars(formula)[-1])) %>%
    summarise(n = n(),.groups = "drop_last")
  
  if(!all(counts_table$n[1] == counts_table$n)){
    warning("Your experiment is not balanced and the result can be misleading. The aov() function used here conducts Type I ANOVA, which only works for balanced design. We recommend using Anova() in the 'car' package to conduct Type II/III ANOVA.")
    #print(counts_table)
  }
  tab_len<- length(summary(model_1))
  res_df<- do.call(data.frame,summary(model_1)[[tab_len]])
  DF_res<-res_df[["Residuals","Df"]]
  MS_res<- res_df[["Residuals","Mean.Sq"]]
  
  #Finding out the F-statistic and p-value for the Whole plot error term
  nested_list<- summary(model_1)[[tab_len-1]]
  nested_df<- do.call(data.frame,nested_list)
  F_nested_df<- nested_df$Mean.Sq/MS_res
  DF_nested_df<- nested_df$Df
  p_nested_df<- pf(F_nested_df,DF_nested_df,DF_res,lower.tail = FALSE)
  #nested_df[["Residuals","F.value"]]<- F_nested_df
  #nested_df[["Residuals","Pr..F."]]<- p_nested_df
  nested_df$F.value<- F_nested_df
  nested_df$Pr..F. <- p_nested_df
  rownames(nested_df)<- "Whole Plot Error"
  
  
  stack_df<-rbind(nested_df,res_df)
  
  # Finding out the F_statistic and p-value for the other whole plot and split plot variables
  for (index in rev(seq_len(tab_len-2))){
    row_list<-summary(model_1)[[index]]
    row_df<- do.call(data.frame,row_list)
    F_df<- row_df$Mean.Sq/nested_df$Mean.Sq
    #print(names(row_df))
    p_df<- pf(F_df,row_df$Df,DF_res,lower.tail = FALSE)
    rname<-rownames(row_df)
    row_df$F.value<- F_df
    row_df$Pr..F.<- p_df
    stack_df<- rbind(row_df,stack_df)
  }
  
  lastest_rownames = c(rownames(stack_df),"Total")
  extra_row <- c(sum(stack_df$Df), sum(stack_df[["Sum.Sq"]]),NA,NA,NA)
  stack_df<-rbind(stack_df, extra_row)
  rownames(stack_df) <- lastest_rownames
  
  stack_df[] <- lapply(stack_df, function(x) if(is.numeric(x)) round(x, 4) else x)
  
  formatted <- format(stack_df, digits = 4)
  formatted[is.na(stack_df)] <- ""
  
  
  tbl<-kable(formatted, align = 'r',
        caption = "ANOVA Summary", escape = F, format.args = list(big.mark = ","))  %>%
    kable_styling() %>%
    row_spec(nrow(stack_df) - 1, hline_after = TRUE)
  
  
  #printing the ANOVA summary table from the stacked dataframe
  # print(kable(format(stack_df, digits = 4), align = 'r',
  #       caption = "ANOVA Summary", escape = F, format.args = list(big.mark = ","))  %>% kable_styling())
  
  
  #This part onwards deals with the calculation of the residuals and fitted values of any split plot model
  
  #differentiating between the response and explanatory variables
  vars<- all.vars(formula)
  y<-vars[1] #response variable
  x<-vars[-1] #independent variables
  term_labels<- terms(formula,keep.order=TRUE)
  c<-attr(term_labels,"term.labels")
  
  #defining three lists as placeholders for interaction terms, error terms and main effect terms respectively
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
  
  #defining the grand mean from the mean of the response variable
  gm <- mean(dataset[[y]])
  
  
  #separating/parsing the error term into before and after parts
  err[[1]]$after<- gsub("\\s+", "", err[[1]]$after)
  err[[1]]$before<- gsub("\\s+", "", err[[1]]$before)
  
  
  #calculating the averages and effect sizes of the main plot variables
  for (index in seq_along(main)){
    fmla<- as.formula(paste(y,"~",main[[index]]))
    means<-  aggregate(fmla, data = dataset, FUN = mean)
    dataset[[paste0("avg_",main[[index]])]] <- means[[y]][
      match(dataset[[main[[index]]]], means[[main[[index]]]])
    ]
    dataset[[paste0("effect_",main[[index]])]] <- dataset[[paste0("avg_",main[[index]])]] -gm
    
  }
  
  
  #calculating the averages and effect sizes of the interaction variables
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
  
  
  #calculating the averages and effect sizes of the error term variables
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
  
  
  #aggregating all the effect sizes columsn for final calculation
  cols<- grep("^effect", names(dataset))
  tbl
  
  #computing the fitted values and the residuals of the model
  #dataset$fits <-  rowSums(dataset[,cols])+gm
  #dataset$residuals<- dataset[[y]]- dataset$fits
  #list(res=dataset$residuals,
   #    fits=dataset$fits)
  
  
  
  
}


#' An ANOVA summary table with total DF & SS
#'
#' This function gives an ANOVA summary table with total degrees of freedom and sum of squares
#' @param formula formula used in ANOVA
#' @param dataset the dataset that contains the experiment information
#' @return a html table
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable kable_styling
#' @importFrom dplyr group_by summarise %>% n
#' @export
#' @examples
#' dox_aov(LogStrength ~ Brand + Water, Towels2)
dox_aov=function(formula, dataset){
  formula=as.formula(formula)
  anova_model=aov(formula, dataset)
  if(length(summary(anova_model)) > 1){
    stop("This function only works for ANOVA with one summary table. Designs like split-plot do not work. For spli-plot designs use dox_split_aov()")
  }
  
  # give warnings if the experiment is not balanced
  counts_table <- dataset %>%
    group_by(across(all.vars(formula)[-1])) %>%
    summarise(n = n(),.groups = "drop_last")
  
  if(!all(counts_table$n[1] == counts_table$n)){
    warning("Your experiment is not balanced and the result can be misleading. The aov() function used here conducts Type I ANOVA, which only works for balanced design. We recommend using Anova() in the 'car' package to conduct Type II/III ANOVA.")
    print(counts_table)
  }
  
  
  table <- summary(anova_model)[[1]]
  lastest_rownames = c(rownames(table),"Total")
  
  
  # Add extra row for sum of DF and SS
  extra_row <- c(sum(table$Df), sum(table[["Sum Sq"]]),NA,NA,NA)
  anova_results <- rbind(table, extra_row)
  rownames(anova_results) <- lastest_rownames
  # Create ANOVA summary table with kable
  # knitr::opts_chunk$set(
  #   out.width = "50%",
  #   out.height = "400px"
  # )
  formatted <- format(anova_results, digits = 4)
  formatted[is.na(anova_results)] <- ""

  kable(formatted, align = 'r',
        caption = "ANOVA Summary", escape = F, format.args = list(big.mark = ","))  %>%
    kable_styling() %>%
    row_spec(nrow(anova_results) - 1, hline_after = TRUE)
  
  # output_list <- list(resid = anova_model$residuals)
  #
  # # Return the list
  # return(output_list)
}



#' Confidence intervals of pairwise comparisons
#'
#' This function plots the confidence intervals of pairwise comparisons using Fisher least significant difference (LSD),
#' Bonferroni significant difference (BSD), and Tukey honest significant difference (HSD) methods.
#' @param dataset dataset of experimental results
#' @param formula target~treatment
#' @param alpha alpha level (default 0.05)
#' @param method LSD, BSD, or HSD (default is ALL)
#' @return confidence interval plots
#' @importFrom dplyr %>% pull filter bind_rows
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang enquo quo_name parse_expr eval_tidy
#' @import ggplot2
#' @export
#' @examples
#' dox_pairs(LogStrength~Water, Towels2)
#' # If you want to adjust the alpha level
#' dox_pairs(LogStrength~Water, Towels2, alpha = 0.01)
#' # If you are only interested in LSD
#' dox_pairs(LogStrength~Water, Towels2, method = "LSD")
dox_pairs <- function(formula,dataset, alpha = 0.05, method = "All") {
  formula=as.formula(formula)
  # Get the string version
  target_str = all.vars(formula)[1]
  treatment_str = all.vars(formula)[2]
  
  alpha_str = deparse(substitute(alpha))
  legend_str = paste("p-value < ", alpha_str)
  
  # Compute ANOVA to obtain MSE
  # response <- parse_expr(quo_name(enquo(target)))
  # x <- parse_expr(quo_name(enquo(treatment)))
  # anova_res=eval_tidy(expr(aov(!!response ~ !!x, data = dataset)))
  
  # str aov
  formula_str <- paste(target_str, "~", treatment_str)
  formula_obj <- as.formula(formula_str)
  anova_res <- aov(formula_obj, data = dataset)
  
  mse <- summary(anova_res)[[1]]["Mean Sq"][[1]][2]
  
  # Get the levels of the treatment variable
  treatment_levels <- unique(dataset[[treatment_str]])
  treatment_levels=as.character(treatment_levels)
  
  
  # Obtain all pairs
  pairs <- as.data.frame(t(combn(treatment_levels, 2)))
  colnames(pairs) = c("treatment1","treatment2")
  
  results <- list()
  for (i in 1:nrow(pairs)) {
    treatment1 <- pairs[i, "treatment1"]
    treatment2 <- pairs[i, "treatment2"]
    
    # Extract two treatment groups
    data1 <- dataset[dataset[[treatment_str]] == treatment1,target_str,drop=TRUE]
    data2 <- dataset[dataset[[treatment_str]] == treatment2,target_str,drop=TRUE]
    
    # Compute the sample sizes and means for the two treatment groups
    n1 <- length(data1)
    n2 <- length(data2)
    mean1 <- mean(data1)
    mean2 <- mean(data2)
    
    # Compute the margin of error and confidence interval
    # LSD
    LSD_me <- qt(alpha/2, nrow(dataset) - length(treatment_levels), lower.tail = FALSE) * sqrt(mse * (1/n1 + 1/n2))
    LSD_ci <- c(mean1 - mean2 - LSD_me, mean1 - mean2 + LSD_me)
    
    # BSD
    BSD_me <- qt(alpha/2/nrow(pairs), nrow(dataset) - length(treatment_levels), lower.tail = FALSE) * sqrt(mse * (1/n1 + 1/n2))
    BSD_ci <- c(mean1 - mean2 - BSD_me, mean1 - mean2 + BSD_me)
    
    # Tukey HSD
    HSD_me <- qtukey(alpha,length(treatment_levels),nrow(dataset)-length(treatment_levels), lower.tail = FALSE)*sqrt(mse*(1/n1+1/n2))/sqrt(2)
    HSD_ci <- c(mean1 - mean2 - HSD_me, mean1 - mean2 + HSD_me)
    
    # Store the results
    results[[i]] <- data.frame(
      treatment1 = treatment1,
      treatment2 = treatment2,
      diff = mean1 - mean2,
      LSD_ci_low = LSD_ci[1],
      LSD_ci_high = LSD_ci[2],
      BSD_ci_low = BSD_ci[1],
      BSD_ci_high = BSD_ci[2],
      HSD_ci_low = HSD_ci[1],
      HSD_ci_high = HSD_ci[2]
    )
  }
  
  # Combine the results into a data frame
  results <- bind_rows(results)
  x_min = min(min(results$LSD_ci_low),min(results$BSD_ci_low),min(results$HSD_ci_low))
  x_max = max(max(results$LSD_ci_high),max(results$BSD_ci_high),max(results$HSD_ci_low))
  
  # Add an indicator for statistical significance
  results$LSD_reject_H0 <- ifelse(results$LSD_ci_low > 0 | results$LSD_ci_high < 0, "yes", "no")
  results$BSD_reject_H0 <- ifelse(results$BSD_ci_low > 0 | results$BSD_ci_high < 0, "yes", "no")
  results$HSD_reject_H0 <- ifelse(results$HSD_ci_low > 0 | results$HSD_ci_high < 0, "yes", "no")
  
  # Plot the confidence intervals
  LSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = LSD_reject_H0)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = LSD_ci_low, xmax = LSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "LSD", color = legend_str) +
    xlim(x_min, x_max)
  
  
  BSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = BSD_reject_H0)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = BSD_ci_low, xmax = BSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "BSD", color = legend_str) +
    xlim(x_min, x_max)
  
  HSD_plot = ggplot(results, aes(x = diff, y = paste(treatment1, treatment2), color = HSD_reject_H0)) +
    scale_color_manual(values = c("yes" = "red", "no" = "black")) +
    geom_errorbarh(aes(xmin = HSD_ci_low, xmax = HSD_ci_high), height = 0.3) +
    geom_point(size = 1) +
    labs(x = "Confidence Interval", y = "Tukey HSD", color = legend_str) +
    xlim(x_min, x_max)
  
  results_display = results %>%
    mutate_if(is.numeric, round, digits = 2)
  
  results_LSD=results_display[,c("treatment1","treatment2","diff","LSD_ci_low","LSD_ci_high","LSD_reject_H0")]
  results_BSD=results_display[,c("treatment1","treatment2","diff","BSD_ci_low","BSD_ci_high","BSD_reject_H0")]
  results_HSD=results_display[,c("treatment1","treatment2","diff","HSD_ci_low","HSD_ci_high","HSD_reject_H0")]
  
  if (method == "LSD")
  {
    print(results_LSD)
    LSD_plot
  }
  else if (method == "BSD")
  {
    print(results_BSD)
    BSD_plot
  }
  else if (method == "HSD"){
    print(results_HSD)
    HSD_plot
  }
  else{
    grid.arrange(LSD_plot, BSD_plot, HSD_plot, ncol=1)
    print(results_LSD)
    print(results_BSD)
    print(results_HSD)
  }
}


#' Checking for contrasts
#'
#' @param dataset the dataset that contains the experiment information
#' @return a html table and residuals,fitted values
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable kable_styling
#' @importFrom mosaic
#' @export                   
dox_contrast <- function(formula,dataset,coeff, alpha = 0.05, method = "All") {
  mat<-NULL
  size= length(coeff[[1]])
  sums<- sapply(coeff, sum)
  bad<- which(sums !=0)
  build_anova_tab <- function(contrastmatrix, split_names) {
    contrasts(dataset[[treatment_str]]) <- contrastmatrix
    split_coeff <- setNames(as.list(seq_along(split_names)), split_names)
    split_arg <- setNames(list(split_coeff), treatment_str)
    model1 <- aov(formula, data = dataset)
    
    tab <- as.data.frame(summary.aov(model1, split = split_arg)[[1]])
    tab$Sig <- symnum(tab$`Pr(>F)`, corr = FALSE, na = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                      symbols = c("***", "**", "*", ".", " "))
    
    total_row <- data.frame(
      Df       = sum(tab$Df, na.rm = TRUE),
      `Sum Sq` = sum(tab$`Sum Sq`, na.rm = TRUE),
      `Mean Sq` = NA, `F value` = NA, `Pr(>F)` = NA, Sig = NA,
      check.names = FALSE
    )
    rownames(total_row) <- "Total"
    
    tab <- rbind(tab, total_row)
    cols_to_round <- c("Sum Sq", "Mean Sq", "F value")
    tab[cols_to_round] <- lapply(tab[cols_to_round], round, digits = 3)
    
    format_pval <- function(x, limit) {
      ifelse(x < limit, formatC(x, format='e',digits=3), format(round(x, 4), scientific = FALSE))
    }
    tab$`Pr(>F)` <- format_pval(tab$`Pr(>F)`, limit = 0.0001)
    tab[is.na(tab)] <- ""
    tab
  }
  
  
  
  build_contrast_tab <- function(contrastvector,treatment_str) {
    for (ind in contrastvector){
      anova_res <- aov(formula, data = dataset)
      x<-favstats(formula ,data=dataset)
      mse <- round(summary(anova_res)[[1]]["Mean Sq"][[1]][2],3)
      mean_data<-x[["mean"]]
      n<-x[["n"]]
      cdot=ind%*%mean_data
      #cat(paste0("Going ahead with t-statistic and p-value calculation\n"))
      #Sys.sleep(1)
      coeff_sq=ind**2
      inv_n=1/n
      cross_term<- coeff_sq%*%inv_n
      t = cdot/(sqrt(mse*cross_term))
      F_v=t**2
      p<- 2*pt(abs(t),sum(n-1),lower.tail = FALSE)
      format_pval<- function(x, limit) {
        ifelse(x < limit,
               formatC(x, format='e',digits=3),
               format(round(x,4), scientific = FALSE))}
      p<-format_pval(p,limit=0.0001)
      low_c<-cdot + qt(alpha/2,sum(n-1))*sqrt(mse*cross_term)
      upp_c<-cdot + qt(1-alpha/2,sum(n-1))*sqrt(mse*cross_term)
      col_entry<-cbind(matrix(ind,nrow = 1),round(t,2),round(F_v,2),p,round(low_c,2),round(upp_c,2))
      #print(ncol(col_entry))
      if (is.null(mat)){
        mat<- col_entry
      } else {
        mat<- rbind(mat,col_entry)
      }
    }
    df<- as.data.frame(mat)
    
    rownames(df)<- c(paste0("C", 1:length(coeff)))
    colnames(df)<-c(substr(levels(dataset[[treatment_str]]),1,8),"t","F","p-value","CI_L","CI_U")
    #kable(df)#%>% add_header_above(c("Contrast"=3,""=5))
    cat("\n\n")
    df
    
    
  }
  
  
  if (length(bad) > 0){
    stop("One or more of the contrasts do not add to zero. So, the hypothesis test would not run\n")
  } else{
    if (length(coeff)==1){
      cat("There is only one set of contrasts \n")
      coeffs<- coeff[[1]]
      formula=as.formula(formula)
      treatment_str = all.vars(formula)[2]
      if (is.character(dataset[[treatment_str]])) {
        message("The treatment variable is a character. Converting to a factor.\n")
        dataset[[treatment_str]] <- as.factor(dataset[[treatment_str]])
      } else if (!is.factor(dataset[[treatment_str]])) {
        message("The treatment variable is of ",
                class(dataset[[treatment_str]])[1], " type. We are changing it to factor type")
        dataset[[treatment_str]] <- as.factor(dataset[[treatment_str]])
      }
      treatment_levels <- unique(dataset[[treatment_str]])
      treatment_levels=as.character(treatment_levels)
      cat("\nThe treatment levels are:\n")
      print(treatment_levels)
      tab<-build_anova_tab(cbind(coeffs),"C1")
      ctab<- build_contrast_tab(c(coeff[1]),treatment_str)
      
      
      
      
      
    } else{
      pairs= combn(coeff, 2, simplify = FALSE)
      for (p in pairs) {
        #v1 <- p[[1]]
        #v2 <- p[[2]]
        #print(p)
        x <- unlist(p)
        res<-x[1:(length(x)/2)]%*%x[(length(x)/2+1):length(x)]
        if (res!=0){
          stop(paste0("The contrasts ",p[1]," and ",p[2]," are not orthogonal, so the hypothesis tests will not be conducted.\n\n"))
        } else {
          cat(paste0("The contrasts ",p[1]," and ",p[2]," are orthogonal\n"))
          formula=as.formula(formula)
          treatment_str = all.vars(formula)[2]
          if (is.character(dataset[[treatment_str]])) {
            message("The treatment variable is a character. Converting to a factor.\n")
            dataset[[treatment_str]] <- as.factor(dataset[[treatment_str]])
          } else if (!is.factor(dataset[[treatment_str]])) {
            message("The treatment variable is of ",
                    class(dataset[[treatment_str]])[1], " type. We are changing it to factor type")
            dataset[[treatment_str]] <- as.factor(dataset[[treatment_str]])
          }
          
          
          cmatrix<-cbind(p[[1]],p[[2]]) 
          tab<-build_anova_tab(cmatrix,c("C1","C2") )
          
          
          
          treatment_levels <- unique(dataset[[treatment_str]])
          treatment_levels=as.character(treatment_levels)
          cat("\nThe treatment levels are:\n")
          print(treatment_levels)
          contr<-c(p[1],p[2])
          ctab<- build_contrast_tab(contr,treatment_str)
          
        }
      }
    }
  }
  kab_anova <- kable(tab, row.names = TRUE, align = "c",caption="ANOVA Summary") %>%
    kable_styling(full_width = FALSE) %>%
    row_spec(nrow(tab) - 1, hline_after = TRUE)
  
  ctab <- kable(ctab, row.names = TRUE, align = "c",caption="Contrast Summary") %>%
    kable_styling(full_width = FALSE)
  
  
  ##Do the following for RMD as this would knit to pdf
  # combine both tables into ONE returned object, at the very end of the function
  knitr::asis_output(
    paste(as.character(kab_anova), as.character(ctab), sep = "\n\n")
  )

}
 
