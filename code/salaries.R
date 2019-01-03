library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(purrr)
library(pbapply)
library(here)
library(tidyr)

parse_number_no_na = function(x) {
  x[ x %in% c("-", "-", "N/A")] = NA
  na_x = is.na(x)
  x = readr::parse_number(x, na = "")
  bad = is.na(x) & !na_x 
  stopifnot(!any(bad))
  x
}

# data from http://www.hscrc.state.md.us/Pages/hsp_Data2.aspx
sal_dir = here("salary")
fnames = list.files(sal_dir, pattern = "[.]xls", full.names = TRUE)
# temp files
fnames = fnames[ !grepl("^~", basename(fnames)) ]
fname = fnames[1]
variable_info = read_excel(here("salary", "2016.xlsx"), sheet = "Format")
hospital_info = read_excel(here("salary", "2016.xlsx"), 
                           sheet =  "Appendix A")
# from http://www.hscrc.state.md.us/Documents/Hospitals/DataReporting/FINAL%20FY%202018%20OP%20Data%20Submission%20Regs%2020170719.xlsx
h_info_2018 = read_excel(
  here("salary", 
       "additional_data",
       "FINAL FY 2018 OP Data Submission Regs 20170719.xlsx"),
  sheet = "Provider ID Codes",
  skip = 1
) %>% 
  filter(!is.na(HOSPID)) %>% 
  filter(!HOSPID %in% "pending new # OHCQ") %>% 
  rename_all(tolower) %>%
  rename(hospital_number = `hospid`,
         hospital_name = x__1) %>% 
  # keep only md
  filter(grepl("^21", hospital_number)) 

h_info_2018 = h_info_2018 %>%   
  select(hospital_number, hospital_name) %>% 
  mutate(
    hospital_number = parse_number_no_na(hospital_number),
    true_hospital_number = hospital_number,
    hospital_number = ifelse(hospital_number > 210000,
                             hospital_number - 210000,
                             hospital_number)    
  )

# hosp_address = 
#   read_csv("salary/additional_data/Medicare_Certification.csv")
# hosp_address = hosp_address %>% 
#     rename_all(tolower) %>% 
#     rename_all(function(x) gsub(" ", "_", x)) 
# hosp_address = hosp_address %>% 
#   rename(hospital_number = `cms_certification_number_(ccn)*`,
#          type = type_of_ownership) %>% 
#   select(state:type) %>% 
#   filter(state == "MD") %>% 
#   mutate(
#     hospital_number = parse_number_no_na(hospital_number),
#     true_hospital_number = hospital_number,
#     hospital_number = ifelse(hospital_number > 217000,
#                              hospital_number - 217000,
#                              hospital_number)    
#   )  



hospital_info = hospital_info %>% 
  rename(hospital_number = `Hospital Number`,
         hospital_name = `Hospital Name`) %>% 
  mutate(
    hospital_number = parse_number_no_na(hospital_number),
    true_hospital_number = hospital_number,
    hospital_number = ifelse(hospital_number > 210000,
                             hospital_number - 210000,
                             hospital_number)    
  )
stopifnot(
  all(hospital_info$hospital_number %in% h_info_2018$hospital_number)
)
# stopifnot(
#   all(hospital_info$hospital_number %in% hosp_address$hospital_number)
# )


# aj = h_info_2018[ 
# !h_info_2018$hospital_number %in% hospital_info$hospital_number,]
aj = h_info_2018[ h_info_2018$hospital_number %in% c(8994), ]
hospital_info = full_join(hospital_info, aj) %>% 
  arrange(hospital_number)

code_info = read_excel(here("salary", "2016.xlsx"), 
                       sheet =  "Appendix B", skip = 1) %>% 
  rename_all(tolower) %>% 
  rename(job_code = code,
         job_title = description)


n_bad = code_info %>% 
  group_by(job_code) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  nrow
stopifnot(n_bad == 0)




remove_dollar_comma = function(x) {
  x = trimws(x)
  x = gsub(",", "", x)
  x = gsub("$", "", x, fixed = TRUE)
  x
}

years = sub("[.]xls.*", "", basename(fnames))
names(fnames) = years
df = lapply(fnames, read_excel, na = c("", "NA", "N/A"))
df = lapply(df, function(x) {
  x %>% 
    rename_all(tolower) %>% 
    rename_all(function(x) gsub(" ", "", x))
})
cn = unlist(sapply(df, colnames))
cn = sort(unique(cn))
cn

renamer = function(x) {
  cn = colnames(x)
  cn = case_when(
    cn %in% c("fb_hour_calculation", 
              "fb_hour", "fb/hourcal.", 
              "fringebenefit/hour") ~ "fb_hour",
    cn %in% c("fb_salary_calculation", 
              "fb/salcal.", "fringebenefit/salary", 
              "fringebenefit/salary") ~ "fb_sal",    
    cn %in% c("hours_paid", 
              "hourspaid") ~ "hours_paid",
    cn %in% c("hospital_number", 
              "hospitalnumber", 
              "hospid",
              "hospnumb") ~ "hospital_number",
    cn %in% c("jobcode", 
              "job_code") ~ "job_code",  
    cn %in% c("jobecodetitle", 
              "jobcodetitle",
              "job_classification",
              "jobtitle") ~ "job_title",  
    cn %in% c("wsf",
              "base_wages_and_salaries_paid",
              "wageandsalary") ~ "wages_and_salary",
    cn %in% c("other_wages_and_salaries_paid",
              "otherwages",
              "othws") ~ "other_wages_and_salary",    
    cn %in% c("numberoffte's",
              "fte",
              "nofte") ~ "number_fte",
    cn %in% c("no_of_employees", 
              "numberofemployees", 
              "empl") ~ "num_employees",      
    cn %in% c("hospital_name", 
              "hname",
              "hospitalname") ~ "hospital_name",
    cn %in% c("totalcosts",
              "total",
              "total_cost") ~ "total_cost",    
    cn %in% c("ratew/fringebenefit",
              "ratew/fb",
              "avg_per_hour",
              "hr_cost") ~ "dollar_per_hour_with_fringe",
    cn %in% c("hr_work") ~ "hours_work",
    cn %in% c("baseyear",
              "year") ~ "base_year",
    cn %in% c("range_max",
              "max") ~ "max_rate",
    cn %in% c("range_min",
              "min") ~ "min_rate",   
    cn %in% c("range_min",
              "min") ~ "min_rate",       
    TRUE ~ cn
  )
  colnames(x) = cn 
  x
}

split_hosp = function(x) {
  cn = colnames(x)
  if ("hospital" %in% cn) {
    check = c("hospital_name", "hospital_number") %in% cn
    if (all(check)) {
      x$hospital = NULL
      x$hospital_number = as.character(x$hospital_number)
      return(x)
    } 
    print(head(x))
    stopifnot(!any(c("hospital_name", "hospital_number") %in% cn))
    x = x %>% 
      mutate(
        hospital = trimws(hospital),
        hospital_number = sub("(\\d*)([.]|) (.*)", "\\1", hospital),
        hospital_name = sub("(\\d*)([.]|) (.*)", "\\3", hospital)) %>% 
      select(-hospital)
  }
  x$hospital_number = as.character(x$hospital_number)
  return(x)
}


add_fte = function(x) {
  if ("number_fte" %in% colnames(x)) {
    return(x)
  }
  x$number_fte = x$num_employees
  return(x)
}



df = lapply(df, renamer)
df = mapply(function(x, y) {
  x$year = y
  x = x %>% 
    select(year, everything())
  x
}, df, as.numeric(names(df)), SIMPLIFY = FALSE)
df = lapply(df, split_hosp)

df = lapply(df, add_fte)


cn = unlist(sapply(df, colnames))
cn = sort(unique(cn))
all_common_vars = cn

num_vars = vars(dollar_per_hour_with_fringe,
                fb_hour,
                fb_sal,
                hours_paid,
                hours_work,
                max_rate,
                min_rate,
                num_employees,
                number_fte,
                other_wages_and_salary,
                tot_fb,
                total_cost,
                wages_and_salary)
cnum_vars = sub("~", "", as.character(num_vars))
df = lapply(df, function(x) {
  sd = setdiff(all_common_vars, colnames(x))
  if (length(sd) > 0) {
    x[sd] = NA
  }
  x = x %>% 
    mutate_at(.vars = num_vars,
              remove_dollar_comma)
  x = x %>% 
    select(year, 
           hospital_number, 
           hospital_name, 
           job_code, job_title,
           everything())
  x
})

# make the numeric!
df = lapply(df, function(x) {
  x = x %>% 
    mutate_at(.vars = num_vars,
              parse_number_no_na)
})


tmp = lapply(df, function(x) {
  na_job = is.na(x$job_title)
  if (any(na_job)) {
    print(x[head(which(na_job)),])
  }
})

############################
# put it all together
############################
df = bind_rows(df)

df = df %>% 
  mutate(hospital_number = trimws(hospital_number),
         hospital_number = parse_number_no_na(hospital_number),
         hospital_name = trimws(hospital_name))

code_tab = split(df, df$job_code)
tab = lapply(code_tab, function(x) {
  table(x$job_title, x$year, useNA = "ifany")
})

##############################
# Make sure 
##############################
check_codes = sapply(code_tab, function(x) {
  if (any(is.na(x$job_title))) {
    return(all(is.na(x$total_cost) | x$total_cost == 0))
  } else { 
    return(TRUE)
  }
})
stopifnot(all(check_codes))

sort(unique(df$job_title), na.last = TRUE)


df = df %>% 
  mutate(orig_hospital_number = hospital_number,
         hospital_number = ifelse(hospital_number > 210000,
                                  hospital_number - 210000,
                                  hospital_number)
  )
table(df$hospital_number)

search = grep(paste("good",
                    "shady", "memorial", "easton", 
                    "southern", 
                    "jose", "rehab", "ortho", 
                    "umcc", "ummc", "levin", "kernan", 
                    "adventi", sep = "|"), 
              tolower(hospital_info$hospital_name))

hospital_info[search, ]

df = df %>% 
  mutate(
    hospital_name = case_when(
      hospital_name %in% "Easton Memorial F.S." ~ "UM-Easton",
      hospital_name %in% c("Good Sam.",
                           "Medstar Good Sa",
                           "Medstar Good Sam") ~ "MedStar Good Sam",
      hospital_name %in% "St.Jose" ~ "UM-St. Joe" ,
      hospital_name %in% "So. MD." ~ "MedStar Southern MD",
      hospital_name %in% c("Kernan", 
                           "UM Rehab&Ortho", 
                           "UM Rehab&Ortho Inst") ~ "UMROI",
      TRUE ~ hospital_name
    )
  )

# need this for later %in% inside call
inside = setdiff(df$hospital_number, hospital_info$hospital_number)
bad = df[ df$hospital_number %in% inside,]
table(bad$hospital_name, useNA = "ifany")
other_same = df[ 
  df$hospital_name %in% sort(unique(bad$hospital_name)) & 
    !df$hospital_number %in% inside,]
table(other_same$hospital_name, other_same$hospital_number )
table(bad$hospital_name, bad$year, useNA = "ifany")
table(bad$hospital_number, bad$year, useNA = "ifany")


df = df %>% 
  mutate(
    hospital_number = ifelse(
      hospital_number %in% inside,
      case_when(
        hospital_name %in% "UM-Easton" ~ 37,
        hospital_name %in% "MedStar Good Sam" ~ 56,
        hospital_name %in% "Shady Grove" ~ 57,
        hospital_name %in% "UMROI" ~ 58,
        hospital_name %in% "MedStar Southern MD"  ~ 62,
        hospital_name %in% "UM-St. Joe" ~ 63,
        hospital_name %in% "Levindale" ~ 64,
        TRUE ~ hospital_number,
      ), hospital_number)
  )

df = df %>% 
  mutate(
    hospital_number = case_when(
      # 
      # https://www.ahd.com/free_profile/210063/University_of_Maryland_Saint_Joseph_Medical_Center/Towson/Maryland/        
      hospital_number == 7 ~ 63,
      # https://www.ahd.com/free_profile/210062/MedStar_Southern_Maryland_Hospital/Clinton/Maryland/        
      hospital_number == 54 ~ 62,
      TRUE ~ hospital_number,
    ), hospital_number)


inside = setdiff(df$hospital_number, hospital_info$hospital_number)
bad = df[ df$hospital_number %in% inside,]
table(bad$hospital_name, useNA = "ifany")
other_same = df[ 
  df$hospital_name %in% sort(unique(bad$hospital_name)) & 
    !df$hospital_number %in% inside,]
table(other_same$hospital_name, other_same$hospital_number )
table(bad$hospital_name, bad$year, useNA = "ifany")
table(bad$hospital_number, bad$year, useNA = "ifany")


# can we assume UMMC for U. UMCC
# only for 2011 and 2012
# what about U. UMCC
d2012 = df %>% 
  filter(year %in% c(2011,2012))
d_good = df %>% 
  filter(year > 2012)
bad_ids = setdiff(d_good$hospital_number, d2012$hospital_number)
table(df$hospital_name[df$hospital_number %in% bad_ids])

#https://data.cms.gov/inpatient-provider-lookup/search-results?stateCode=MD
# from https://www.ahd.com/free_profile/210063/University_of_Maryland_Saint_Joseph_Medical_Center/Towson/Maryland/
missing = setdiff( hospital_info$hospital_number, df$hospital_number)
stopifnot(length(missing) == 0)

# get the outptu - reorder some columns
res = df %>% 
  rename(data_hospital_name = hospital_name) 
# hospital_info because better names?
res = left_join(res, hospital_info)
res =  res %>% 
  select(year, hospital_number, hospital_name, job_code, job_title,
         everything()) %>% 
  select(-data_hospital_name, data_hospital_name)
ss = split(res, as.character(res$hospital_number))
ss = lapply(ss, function(x) table(x$data_hospital_name))

sd = setdiff(res$job_code, code_info$job_code)
bad = res[ res$job_code %in% sd,]
stopifnot(all(is.na(bad$hours_work ) | bad$hours_work == 0))
res = res %>% 
  mutate(hours_work = ifelse(hours_work == 0,
                             NA,
                             hours_work))
c_hours = res %>% 
  mutate(has_work = !is.na(hours_work),
         has_paid = !is.na(hours_paid)) %>% 
  group_by(year) %>% 
  summarize_at(.vars = vars(has_work, has_paid),
               sum)
print(c_hours)

res = res %>% 
  mutate(hours_work = ifelse(
    is.na(hours_work),
    hours_paid,
    hours_work) ) %>% 
  select(-hours_paid) %>% 
  filter(!is.na(hours_work))
table(res$year)

sd = setdiff(res$job_code, code_info$job_code)
stopifnot(length(sd) == 0)

# unifying the code
res = res %>% 
  select(-job_title) %>% 
  left_join(code_info)

res = res %>% 
  mutate(
    other_wages_and_salary = ifelse(
      is.na(other_wages_and_salary), 
      0, other_wages_and_salary),
    wages_and_salary = ifelse(
      is.na(wages_and_salary), 
      0, wages_and_salary),    
    total_salary = wages_and_salary + other_wages_and_salary,
    dollar_per_hour = total_salary / hours_work
    ) 

write_rds(res, path = here("salary", "salary_data.rds"),
          compress = "xz")

