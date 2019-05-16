
#' Prepares Data for 3-6-12 Reports
#'
#' This function takes a data.table with the columns:
#' col {'prior', 'current'}
#' product
#' region
#' category
#' value
#' util
#' mm
#' If the columns product, region, or category are missing they are replaced with TRUE
#' Ensure there are no missing values, and the data has already been aggregated
#'
#' @param data A data.frame or data.table
#'
#' @export
#'


convert_to_3612 <- function(data){

  if(is.data.table(data)){
    d <- copy(data)
  } else {
    d <- as.data.table(data)
      }

  if(sum(is.na(d)) != 0) {
    stop('Address missing values before proceeding!')
    }

  if(prod(c('col', 'value', 'util', 'mm') %in% names(d))!=1) {
    stop('Error: Missing one of col, value, util, or mm columns!')
    }

  if(!('product' %in% names(d))){d[, product := T]}
  if(!('region' %in% names(d))){d[, region := T]}
  if(!('category' %in% names(d))){d[, category := T]}

  if(nrow(unique(data[, .(col, product, region, category)])) != nrow(data[, .(col, product, region, category)])) {
    stop('Address duplicates before proceeding!')
  }

  d[,`:=`(cost_pmpm = ifelse(members == 0, 0, value/mm),
          claims_per_1000 = ifelse(members == 0, 0, util/mm*12000),
          cost_per_claim = ifelse(volume == 0, 0, value/util))]

  d <- d %>%
    dcast(product + category + region ~ col,
          value.var = c('cost_pmpm', 'claims_per_1000', 'cost_per_claim', 'value', 'util'),
          fill = 0)

  d[,`:=`(cost_pmpm_prior = formattable::currency(cost_pmpm_prior),
          cost_pmpm_current = formattable::currency(cost_pmpm_current),

          claims_per_1000_current = formattable::accounting(claims_per_1000_current),
          claims_per_1000_prior = formattable::accounting(claims_per_1000_prior),

          cost_per_claim_current = formattable::currency(cost_per_claim_current),
          cost_per_claim_prior = formattable::currency(cost_per_claim_prior),

          spend_prior = formattable::currency(spend_prior, digits = 0),
          spend_current = formattable::currency(spend_current, digits = 0),

          volume_prior = formattable::accounting(volume_prior, digits = 0),
          volume_current = formattable::accounting(volume_current, digits = 0),

          cost_pmpm_var = formattable::currency(cost_pmpm_current - cost_pmpm_prior),
          claims_per_1000_var = formattable::accounting(claims_per_1000_current - claims_per_1000_prior),
          cost_per_claim_var = formattable::accounting(cost_per_claim_current - cost_per_claim_prior),
          value_var = formattable::accounting(value_current - value_prior, digits = 0),
          util_var = formattable::accounting(util_current - util_prior, digits = 0),

          cost_pmpm_var_pct = fidelis::pct_change(cost_pmpm_prior, cost_pmpm_current),
          claims_per_1000_var_pct = fidelis::pct_changepct(claims_per_1000_prior, claims_per_1000_current),
          cost_per_claim_var_pct = fidelis::pct_changepct(cost_per_claim_prior, cost_per_claim_current),
          value_var_pct = fidelis::pct_changepct(value_prior, value_current),
          util_var_pct = fidelis::pct_changepct(util_prior, util_current))]

  setcolorder(d, c('product', 'region', 'category',
                   'cost_pmpm_prior', 'cost_pmpm_current', 'cost_pmpm_var', 'cost_pmpm_var_pct',
                   'claims_per_1000_prior', 'claims_per_1000_current', 'claims_per_1000_var', 'claims_per_1000_var_pct',
                   'cost_per_claim_prior', 'cost_per_claim_current', 'cost_per_claim_var', 'cost_per_claim_var_pct',
                   'value_prior', 'value_current', 'value_var', 'value_var_pct',
                   'util_prior', 'util_current', 'util_var', 'util_var_pct'))

  setnames(d, names(d), gsub('value', 'paid', names(d)))

  return(d)

}
