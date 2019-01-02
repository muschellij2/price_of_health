library(readr)
library(rvest)
library(httr)
library(xml2)
library(lubridate)
library(dplyr)
library(purrr)
library(pbapply)

source("html_extract_table_rows.R")
x = read_html("MD/all_payers_2016.html")

right_div = xml2::xml_find_all(x, '//div[ @id = "result"]')
stopifnot(length(right_div) == 1)
right_div = right_div[[1]]
period = xml2::xml_find_all(right_div, '//h3[ @id = "periodLabel"]')
period = html_text(period)
period = strsplit(period, "-")[[1]]
period = trimws(period)
period = paste(period, c("/01", "/31"))
period = myd(period)
stopifnot(length(period) == 2)

##############################
# get the one table
##############################
grid_div = xml2::xml_find_all(x, '//div[ @id = "gridResult"]//table [ @role = "treegrid"]')
stopifnot(length(grid_div) == 1)
grid_div = grid_div[[1]]

hdr = xml2::xml_find_all(grid_div, "./thead/tr")
stopifnot(length(hdr) == 1)
hdr = xml2::xml_find_all(hdr, "./th")
hdr = html_text(hdr)
hdr = gsub("Open APR-DRG Definition", "", hdr)
hdr = gsub("Open APR-DRG ID Definition", "", hdr)


##############################
# get the rows so get the types
##############################
rg = xml2::xml_find_all(grid_div, './tbody[ @role = "rowgroup"]')
stopifnot(length(rg) == 1)
rg = rg[[1]]

tr = html_nodes(rg, xpath = "./tr")
classes = html_attr(tr, "class")

masters = grepl("k-master-row", classes)
dets = grepl("k-detail-row", classes)
stopifnot(all(masters | dets))

cat(which(diff(which(masters)) != 2), sep = "\n")
stopifnot(all(diff(which(masters)) == 2))
mnode = tr[masters]
mtab = html_extract_table_rows(mnode)
colnames(mtab) = hdr
mtab = as_data_frame(mtab)
mtab = mtab %>% 
  select(-"Click arrow to show details")
mtab = mtab %>% 
  rename(med_cond =  "Medical Conditions (APR-DRG)",
         apr_drg_id = "APR-DRG ID", 
         num_cases = "Number of Cases", 
         avg_chg_per_case = "Average Charge Per Case ($)", 
         avg_los_days = "Average Length of Stay (Days)")
mtab$row_index = which(masters)
cn = colnames(mtab)


dnode = tr[dets]
dnode_tabs = pblapply(dnode, function(x) {
  run_nodes = html_nodes(x, xpath = ".//table")
  res =  html_table(run_nodes)
  res
})


# one before
names(dnode_tabs) = mtab$med_cond[ mtab$row_index == which(dets) - 1 ]

Smain_tab = lapply(dnode_tabs, dplyr::first)
main_tab = purrr::map_df(main_tab, function(x) { 
  x$"Number of Cases" = gsub(",", "", x$"Number of Cases")
  x$"Number of Cases" = as.numeric(x$"Number of Cases")
  x
  }, .id = "med_cond")
main_tab = as_tibble(main_tab)


other_tab = lapply(dnode_tabs, dplyr::nth, 2)
other_tab = purrr::map_df(other_tab, function(x) { 
  x$"Number of Cases" = gsub(",", "", x$"Number of Cases")
  x$"Number of Cases" = as.numeric(x$"Number of Cases")
  x
}, .id = "med_cond")
other_tab = as_tibble(other_tab)

main_tab$period_start = period[1]
main_tab$period_end = period[2]


