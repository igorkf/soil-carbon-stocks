# all helper function from https://idahoagstats.github.io/AgHackathon/soc-stock-SDA.html#helper-functions

#' @title Compute Weighted Mean SOC Stock for a Single Map Unit
#'
#' @param i `data.frame`
#' @param id character, column name containing map unit ID
#' @param s character, column name SOC stock by component
#' @param w character, column name containing component percentage
#'
#' @return `data.frame`
#' 
wtMeanStock <- function(i, id = 'mukey', s = 'SOC.stock', w = 'comppct_r') {
  # remove rows: 
  #  - NA SOC stocks
  #  - NA component percentages
  #  - 0-value SOC stocks
  .idx <- which(
    i[[s]] > 0.00001 & !is.na(i[[s]]) & !is.na(i[[w]])
  )
  
  # if no rows remaining, stock is NA
  if(length(.idx) < 1) {
    .stock <- NA
    .pct <- NA
  } else {
    # filter records with missing data
    i <- i[.idx, ]
    
    # weighted mean
    .stock <- sum(i[[s]] * i[[w]]) / sum(i[[w]])
    
    # contributing fraction
    .pct <- sum(i[[w]])
  }
  
  # compose result
  .res <- data.frame(
    .id = i[[id]][1],
    stock = .stock,
    fraction = .pct
  )
  
  # save id name
  names(.res)[1] <- id
  
  return(.res)
}


#' @title Estimate Wt. Mean SOC Stocks by Map Unit
#'
#' @param x `SoilProfileCollection` object
#' @param id character, column name containing map unit ID
#'
#' @return `data.frame`
#' 
estimateStock <- function(x, id = 'mukey') {
  
  # iterate over map unit keys,
  # computing wt. mean SOC stocks
  g <- split(site(x), x[[id]])
  g <- lapply(g, wtMeanStock, id = 'mukey', s = 'SOC.stock', w = 'comppct_r')
  
  # flatten to data.frame
  g <- do.call('rbind', g)
  row.names(g) <- NULL
  
  return(g)
}


#' @title Extract SSURGO Data for a Vector of Map Unit Keys
#' @description SSURGO data are queried (via SDA), formatted, and converted into a `SoilProfileCollection` for later use. The `.top` and `.bottom` arguments truncate profiles for the calculation of SOC stocks within a specific depth interval.
#'
#' @param m integer or character vector of map unit keys
#' @param .top integer, top depth (cm) of SOC stock estimate
#' @param .bottom integer, bottom depth (cm) of SOC stock estimate
#'
#' @return `SoilProfileCollection` object
#' 
processSSURGO <- function(m, .top = 0, .bottom = 100) {
  
  # compile unique set of map unit keys
  .is <- format_SQL_in_statement(
    unique(
      as.character(m)
    )
  )
  
  # create SQL statements for SDA
  .sql <- sprintf("
--
-- required IDs and component percentage
--
SELECT mukey, component.cokey, compname, comppct_r, chorizon.chkey, hzname,
--
-- horizon top, bottom, and computed thickness
--
hzdept_r AS hztop, hzdepb_r AS hzbottom, (hzdepb_r - hzdept_r) as thick,
--
-- using 1/3 bar bulk density (g/cm^3)
--
dbthirdbar_r as db,
--
-- variables with a logical 0, possibly encoded as NULL
-- convert NULL 'quantities' into '0'
-- SOM is ROUGHLY 58 pct SOC, future work will incorporate local estimators
--
(COALESCE(om_r, 0) / 100.0) * 0.58 AS oc, 
(COALESCE(om_l, 0) / 100.0) * 0.58 AS oc_low, 
(COALESCE(om_h, 0) / 100.0) * 0.58 AS oc_high, 
--
-- compute <2mm soil fraction, by subtracting >2mm fraction
-- limit min value to 10 pct (90pct total fragment volume), in case of data errors
-- 
CASE WHEN COALESCE(soil_fraction, 1) < 0.1 THEN 0.1 ELSE COALESCE(soil_fraction, 1) END as soil_fraction
FROM component
INNER JOIN chorizon ON component.cokey = chorizon.cokey
LEFT JOIN
    (
  --
    -- total rock fragment volumetric fraction by horizon
    -- 
    SELECT chkey, (100.0 - sum(COALESCE(fragvol_r, 0))) / 100.0 as soil_fraction
    FROM chfrags
    GROUP BY chkey
    ) as frag_data
ON chorizon.chkey = frag_data.chkey
--
-- apply map unit key filter
--
WHERE component.mukey IN %s
--
-- convenient ordering for use later in the init. of SoilProfileCollection
--
ORDER BY mukey, comppct_r DESC, hztop ASC;
", .is
  )
  
  # run query
  x <- SDA_query(.sql)
  
  # init SoilProfileCollection object
  depths(x) <- cokey ~ hztop + hzbottom
  hzdesgnname(x) <- 'hzname'
  site(x) <- ~ mukey + compname + comppct_r
  
  # combine component name + comppct for a more informative label
  x$label <- sprintf("%s\n%s%%", x$compname, x$comppct_r)
  
  # SOC stock by horizon = 
  # thick (cm) * Db 1/3 bar (g/cm^3) * (soil fraction) * SOC (%) * conversion factor (10)
  x$soc_kg_sq.m <- x$thick * x$db * x$soil_fraction * x$oc * 10
  
  # truncate profile to specified top and bottom limits
  x <- trunc(x, .top, .bottom)
  
  # cumulative SOC stock, by horizon
  x$stock.csum <- profileApply(x, function(i) {
    cumsum(i$soc_kg_sq.m)
  })
  
  # total SOC stock
  x$SOC.stock <- profileApply(x, function(i) {
    sum(i$soc_kg_sq.m, na.rm = TRUE)
  })
  
  return(x)
}