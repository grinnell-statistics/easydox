#' An ANOVA summary table with total DF & SS
#'
#' This function gives an ANOVA summary table with total degrees of freedom and sum of squares
#' @param aov the anova model
#' @return a html table
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable kable_styling
#' @export
dox_aov=function(aov){
  if(length(summary(aov)) > 1){
    stop("This function only works for ANOVA with one summary table. Designs like split-plot do not work.")
  }


  table <- summary(aov)[[1]]
  lastest_rownames = c(rownames(table),"Total")


  # Add extra row for sum of DF and SS
  extra_row <- c(sum(table$Df), sum(table[["Sum Sq"]]),NA,NA,NA)
  anova_results <- rbind(table, extra_row)
  rownames(anova_results) <- lastest_rownames
  # options(knitr.kable.NA = '')
  # Create ANOVA summary table with kable
  knitr::opts_chunk$set(
    out.width = "50%",
    out.height = "400px"
  )
  kable(format(anova_results, digits = 4), align = 'r',
        caption = "ANOVA Summary", escape = F, format.args = list(big.mark = ","))  %>% kable_styling()
}
