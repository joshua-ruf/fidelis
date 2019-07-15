
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fidelis <img src='man/figures/logo_green.png' align="right" height="150" />

Automate data reporting with this R package\!

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshua-ruf/fidelis")
```

#### New to R?

Follow these steps to get R and the fidelis package installed on
Windows:

**First**, download and install the most recent version of R from
[CRAN](https://cran.r-project.org). As of 7/12/2019 that is 3.6.1 – if R
is already installed run `R.version.string` to check your version.

**Second**, install
[RStudio](https://www.rstudio.com/products/rstudio/download/#download).
This is not strictly necessary, but the fidelis package makes use of the
rstudioapi package for handling passwords, so it’s required to use some
functions.

**Third**, Windows users need to install Rtools before installing the
fidelis package. The installr package will handle the Rtools
installation, simply run the code below and accept the default options.

``` r
install.packages('devtools')
# install devtools

if(Sys.info()['sysname'] == 'Windows' && !devtools::find_rtools()){
  install.packages('installr');
  installr::install.URL('https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe')
  }
# install Rtools if not already installed; as of 7/12/2019 3.5 is the recommended version
```

**Fourth**, install fidelis from Github.

``` r
devtools::install_github('joshua-ruf/fidelis', dependencies = T)
```

**Fifth**, install [tidyverse](https://www.tidyverse.org/) and
[openxlsx](https://www.rdocumentation.org/packages/openxlsx/versions/4.1.0.1).
This is optional, but both are highly useful packages.

``` r
install.packages(c('tidyverse', 'openxlsx'))
```

## Example

At the heart of the fidelis package is really just an efficient
integration of R with Greenplum/PostgreSQL. This section will serve as
the `Hello World!` to just some of these functions. Throughout this
tutorial I frequently make use of `%>%` from the magrittr package. This
*pipe* allows us to split code onto multiple lines, making it easier to
read (and also write). For example, `sum(x, y)` returns the same value
as `x %>% sum(y)`.

We’ll begin by creating a data.frame and uploading it to the database.

``` r
library(data.table) #data manipulation
library(magrittr) #pipes
library(dplyr) #data manipulation
library(formattable) #number formatting/coloring
library(knitr) #basic table creation
library(kableExtra) #table formatting
library(fidelis)
```

``` r
df <- expand.grid(stringsAsFactors = F,
  
  date = seq.Date(lubridate::round_date(Sys.Date(), 'month') - months(23),
                  lubridate::round_date(Sys.Date(), 'month'),
                  by = 'month'),
  product = LETTERS[1:6],
  region = letters[1:4]
) # 24 months of data for 6 products in 4 regions

n <- nrow(df) # number of rows in df

df <- df %>%
  data.table::as.data.table() %>%
  .[, `:=`(spend = runif(n = n)*1000,
           members = sample(100:200, size = n, replace = T))]
# add 2 new columns of random data, spend and members

fidelis::greenplum_connect(ask=F)
# create a greenplum connection object, note the result does not need to be assigned to an object
# the option ask=F tells the function not to ask for input but instead to look for a secret password
# see fidelis::create_secret() for more information.

fidelis::send_to_database(df, name = 'temptable')
#> <PostgreSQLResult>
# send df to the greenplum database as a temp table
# note: if name began with "sandbox." then the table would be permenant instead of temporary

fidelis::query("select * from temptable limit 5;")
#>         date product region    spend members
#> 1 2018-12-01       B      a 294.2245     174
#> 2 2018-04-01       E      a 537.6242     115
#> 3 2017-08-01       B      b 559.9943     133
#> 4 2018-12-01       D      b 724.0961     141
#> 5 2019-05-01       B      c 780.2080     116
# to run a SQL query pass sql code as a string through fidelis::query()
# assign to R object to save results
```

The `query()` function also has the ability to execute dynamic SQL
queries. Suppose we want to run an analysis on only a couple of products
in a certain region, we can create R vectors with that information and
add those as options to the `query()` function.

``` r
products <- c('A', 'B')
regions <- 'd'

df_limited <- fidelis::query(
  
  "
  select *
  from temptable
  where product in %product%
    and region = %region%
  ;
  ",
  product = products,
  region = regions

) # note how dynamic inputs are added, they must be named after the SQL text and surrounded by "%" in the SQL code

unique(df_limited[, c('product', 'region')])
#>   product region
#> 1       B      d
#> 2       A      d
# test to make sure the dynamic where clause was successful!
```

The dynamic queries are especially useful if we are interested in
working with dates. Suppose now that we are interested in comparing the
last 3 months of 2017 to the last three months of 2018. The fidelis
functions `set_dates()` and `usdate()` will help us prepare these
inputs.

``` r
final_month <- as.Date('2018-12-01') # specify the final month we're interested in
rolling_window <- 3 # number of months to compare across years

dates_df <- final_month %>%
  fidelis::set_dates()

dates_df
#>         effper     col    col6    col9   col12
#>  1: 2018-12-01 current current current current
#>  2: 2018-11-01 current current current current
#>  3: 2018-10-01 current current current current
#>  4: 2018-09-01    <NA> current current current
#>  5: 2018-08-01    <NA> current current current
#>  6: 2018-07-01    <NA> current current current
#>  7: 2018-06-01    <NA>    <NA> current current
#>  8: 2018-05-01    <NA>    <NA> current current
#>  9: 2018-04-01    <NA>    <NA> current current
#> 10: 2018-03-01    <NA>    <NA>    <NA> current
#> 11: 2018-02-01    <NA>    <NA>    <NA> current
#> 12: 2018-01-01    <NA>    <NA>    <NA> current
#> 13: 2017-12-01   prior   prior   prior   prior
#> 14: 2017-11-01   prior   prior   prior   prior
#> 15: 2017-10-01   prior   prior   prior   prior
#> 16: 2017-09-01    <NA>   prior   prior   prior
#> 17: 2017-08-01    <NA>   prior   prior   prior
#> 18: 2017-07-01    <NA>   prior   prior   prior
#> 19: 2017-06-01    <NA>    <NA>   prior   prior
#> 20: 2017-05-01    <NA>    <NA>   prior   prior
#> 21: 2017-04-01    <NA>    <NA>   prior   prior
#> 22: 2017-03-01    <NA>    <NA>    <NA>   prior
#> 23: 2017-02-01    <NA>    <NA>    <NA>   prior
#> 24: 2017-01-01    <NA>    <NA>    <NA>   prior
#>         effper     col    col6    col9   col12
```

`dates_df` is a data.table that assigns each month for the last 24
months to one of three categories: ‘current’ period, ‘prior’ period, and
`NA` (neither). The four `col` columns differentiate between the number
of months to include in the rolling window.

``` r
dates_df <- dates_df[, c(1, 1 + rolling_window/3), with = F] %>%
  data.table::setnames(names(.), c('effper', 'col'))
# get the number of months rolling window based on the rolling_window object defined above.

current <- dates_df[col == 'current', effper] %>%
  fidelis::usdate()

prior <- dates_df[col == 'prior', effper] %>%
  fidelis::usdate()
# get only months that belong to the current and prior periods respectively
# also convert them to SQL friendly format with usdate()
```

Finally, we’ll run the query, calculating the sum of spend and members,
by period, product, and region.

``` r
df_aggregated <- fidelis::query(
  
  "
  select case when date in %current% then 'current'
    else 'prior'
    end as col
    ,product
    ,region
    ,sum(spend) as spend
    ,sum(members) as members
  from temptable
  where (date in %current% or date in %prior%)
  group by 1,2,3
  ;
  ",
  current = current,
  prior = prior
  
)

head(df_aggregated, 10)
#>        col product region     spend members
#> 1  current       F      b 2033.1587     504
#> 2  current       F      c  949.7999     493
#> 3    prior       F      d 1606.4501     462
#> 4    prior       A      a 1011.8051     463
#> 5  current       C      a 1428.0296     393
#> 6    prior       E      d 1799.3940     378
#> 7    prior       A      b 1561.8828     449
#> 8  current       F      a 1582.0105     428
#> 9    prior       A      c 2263.9995     547
#> 10   prior       F      b 2273.8818     518
```

Great\! One major limitation of this approach is that error handling can
be quite difficult; a query will return an error but often the output
does not fully display the error message. This problem lead to
`madlib()`, a function that prepares the dynamic query but instead of
sending it to the Greenplum database, it prints it to the console.
Easier to view, and potentially copy the query into another program for
more thorough debugging.

``` r
fidelis::madlib(
  
  "
  select case when date in %current% then 'current'
    else 'prior'
    end as col
    ,product
    ,region
    ,sum(spend) as spend
    ,sum(members) as members
  from temptable
  where (date in %current% or date in %prior%)
  group by 1,2,3
  ;
  ",
  current = current,
  prior = prior
  
)
#> 
#>   select case when date in ('12/01/2018','11/01/2018','10/01/2018') then 'current'
#>     else 'prior'
#>     end as col
#>     ,product
#>     ,region
#>     ,sum(spend) as spend
#>     ,sum(members) as members
#>   from temptable
#>   where (date in ('12/01/2018','11/01/2018','10/01/2018') or date in ('12/01/2017','11/01/2017','10/01/2017'))
#>   group by 1,2,3
#>   ;
#> 
```

Fortunately, this query runs with no errors\! Now let’s create a summary
table using formattable, knitr, kableExtra, and fidelis. A [great
reference](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
was made by Hao Zhu.

``` r
data.table::setDT(df_aggregated)
# set as data.table

df_aggregated[, spend_per_member := spend/members]
# create new spend_per_member column

df_wide <- fidelis::add_var_cols2(df_aggregated, vars = c('spend_per_member'))
# reshape long to wide and add var/var_pct column for specified variable

for(n in fidelis::names_with(names(df_wide), with = 'spend', without = 'pct')){
  data.table::set(df_wide, j = n, value = formattable::currency(df_wide[[n]]))
} #format spend variables (except spend_per_member_var_pct) as currency
```

Be sure to specify `results='asis'` as an option in the following
Rmarkdown chunk for best
results.

``` r
left_cols <- 2 #specify how many of the left most columns are left aligned (the rest are right algined)

df_wide %>%
  dplyr::mutate(
      spend_per_member_prior = formattable::color_bar('lightblue')(spend_per_member_prior),
      spend_per_member_current = formattable::color_bar('lightblue')(spend_per_member_current),
      spend_per_member_var = fidelis::red_green_tiles.r(spend_per_member_var),
      spend_per_member_var_pct = fidelis::red_green_text.r(spend_per_member_var_pct)
      ) %>% #apply colouring
  knitr::kable(
      escape = F, #necessary for html formatting to be read as html
      format = 'html',
      table.attr = "style = \"color: black;\"", #change font from grey to black
      col.names = c('Product', 'Region',
                    'Prior', 'Current', 'Var', 'Var (%)'),
      align = c(rep('l', left_cols),  rep('r', ncol(.) - left_cols)) #align columns left or right
      ) %>% 
  kableExtra::kable_styling(
      full_width = F,
      protect_latex = F, # useful for coloring formatting as well
      position = "left",
      bootstrap_options = c('condensed', 'hover')
      ) %>%
  kableExtra::column_spec(1, bold = T) %>%
  kableExtra::add_header_above(c(' ' = 2, 'Spend/ Members' = 4)) %>%
  kableExtra::collapse_rows(columns = 1, valign = 'top') %>%
  kableExtra::footnote(general = "Cool eh?") %>%
  htmltools::knit_print.html() #this is necessary for use with with results='asis'
```

![final\_table](./man/figures/final_table.png)

To view all `fidelis` functions, simply run `help(package = 'fidelis')`.
To learn more about a specific function run `?` followed by the function
name (i.e. `?currency.auto`).
