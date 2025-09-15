
source(here::here("./prep/prep.R"))
source(here::here("./scripts/00 cleaning script.R"))

# function for flextables

# Tables on collaboration
# Define colors

flex_fun <- function(data) {

header_color <- "#00457c"
row_color <- "#EDF0DF"

  ft <- data |> 
  flextable::flextable() |> 
    flextable::bold(part = "header") |>  # Make column labels bold
    flextable::color(color = header_color, part = "header") |>  # Corrected header text color
    flextable::bg(i = seq(1, nrow(data), by = 2), bg = row_color, part = "body") |>  # Alternate row colors
    flextable::autofit() |> 
    flextable::align(align = "center", part = "all")  
    
  return(ft)
 
}


#Descriptives
df_orgs <- dat6_non6 |> 
  group_by(desc_org) |> 
  count() |> 
  mutate(total = nrow(dat6_non6)
         , percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |> 
  dplyr::select("Type of organization" = desc_org
         , "Percent" = percent)

df_orgs_ft <- flex_fun(df_orgs)

women_led <- dat6_non6 %>% 
  drop_na(women_led) %>% 
  filter(women_led != "No") %>% 
  count(women_led) %>% 
  mutate(percent = paste0(round(n/172 *100, digits = 0), "%", " (", n, ")")
         , org = case_when(women_led == "Yes" ~ "Women-led Org.")) %>% 
  dplyr::select(org
                , percent)
  
gesi_focus <-  dat6_non6 %>% 
  drop_na(gesi_focus) %>% 
  filter(gesi_focus != "No") %>% 
  count(gesi_focus) %>% 
  mutate(percent = paste0(round(n/172*100, digits = 0), "%", " (", n, ")")
         , org = case_when(gesi_focus == "Yes" ~ "GESI-focused Org.")) %>% 
  dplyr::select(org
                , percent)

gen_focus_disadv <- dat6_non6 %>% 
  drop_na(gen_focus_serves_disadv_pop) %>% 
  count(gen_focus_serves_disadv_pop) %>% 
  mutate(percent = paste0(round(n/172*100, digits = 0), "%", " (", n, ")")
         , org = case_when(gen_focus_serves_disadv_pop == "Yes" ~ "Org. Focusd on Disadvantaged Populations")) %>% 
  dplyr::select(org
                , percent)

GESI_orgs <- dplyr::bind_rows(women_led, gesi_focus, gen_focus_disadv) %>% 
  rename("Type of Organization" = org)

GESI_orgs_ft <- flex_fun(GESI_orgs)

# organizations by state
df_state <- dat6_non6 |> 
  mutate(states_fixed2 = case_when(states_fixed2 %in% c("?", "London, UK", "Denmark"
                                                        , "Accra, Ghana", "New York"
                                                        , "Washington, DC") ~ "Other"
                                   , is.na(states_fixed2) ~ "Other"
                                   , TRUE ~ states_fixed2)) |> 
  group_by(states_fixed2) |> 
  count() |> 
  mutate(total = nrow(dat6_non6)
         , percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |>
  dplyr::select("State" = states_fixed2
         , "Percent" = percent)

#orgs by cohort
df_orgs_cohort <- dat6_non6 |> 
  group_by(cohort) |> 
  count() |> 
  mutate(total = nrow(dat6_non6)
         , percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |>
  dplyr::select("Cohort" = cohort
         , "Percent" = percent)

#org type by cohort
df_orgs_desc_cohort <- dat6_non6 |> 
  group_by(cohort) |> 
  summarize(n_govt = sum(desc_org == "MDA", na.rm = TRUE)
            , n_private = sum(desc_org == "Private Business", na.rm=TRUE)
            , n_ngo = sum(desc_org == "NGO", na.rm=TRUE)
            , n_ingo = sum(desc_org == "INGO", na.rm=TRUE)) |> 
  mutate(total = n_govt + n_private + n_ngo + n_ingo
         , percent_govt = paste0(round(n_govt/total*100, digits = 0), "%", " (", n_govt, ")")
         , percent_private = paste0(round(n_private/total*100, digits = 0), "%", " (", n_private, ")")
         , percent_ngo = paste0(round(n_ngo/total*100, digits = 0), "%", " (", n_ngo, ")")
         , percent_ingo = paste0(round(n_ingo/total*100, digits =0), "%", " (", n_ingo, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")" )) |> 
  dplyr::select("Cohort" = cohort
         ,  "MDAs" = percent_govt
         , "Private businesses" = percent_private
         , "NGOs" = percent_ngo
         , "INGOs" = percent_ingo
         , "Total" = percent_total)
#cohorts by states
df_cohort_states <- dat6_non6 |> 
  mutate(states_fixed2 = case_when(states_fixed2 %in% c("?", "London, UK", "Denmark"
                                                        , "Accra, Ghana", "New York"
                                                        , "Washington, DC") ~ "Other"
                                   , is.na(states_fixed2) ~ "Other"
                                   , TRUE ~ states_fixed2)) |> 
  group_by(states_fixed2) |> 
  summarize(n_bc = sum(cohort == "Behavior Change", na.rm = TRUE)
            , n_jb = sum(cohort == "JoinBodi", na.rm=TRUE)
            , n_mj = sum(cohort == "Media and Journalism", na.rm=TRUE)
            , n_cj = sum(cohort == "Criminal Justice", na.rm=TRUE)
            , n_non = sum(cohort == "Non Grantee", na.rm = TRUE)) |> 
  mutate(total = n_bc + n_jb + n_mj + n_cj + n_non
         , percent_bc = paste0(round(n_bc/total*100, digits = 0), "%", " (", n_bc, ")")
         , percent_jb = paste0(round(n_jb/total*100, digits = 0), "%", " (", n_jb, ")")
         , percent_mj = paste0(round(n_mj/total*100, digits = 0), "%", " (", n_mj, ")")
         , percent_cj = paste0(round(n_cj/total*100, digits =0), "%", " (", n_cj, ")")
         , percent_non = paste0(round(n_non/total*100, digits = 0), "%", " (", n_non, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")" )) |> 
  dplyr::select("State" = states_fixed2
         , "Behavior Change" = percent_bc
         , "JoinBodi" = percent_jb
         , "Media and Journalism" = percent_mj
         , "Criminal Justice" = percent_cj
         , "Non Grantee" = percent_non
         , "Total" = percent_total)

#organizations by type by states

df_orgs_state <- dat6_non6 |> 
  mutate(states_fixed2 = case_when(states_fixed2 %in% c("?", "London, UK", "Denmark"
                                                        , "Accra, Ghana", "New York"
                                                        , "Washington, DC") ~ "Other"
                                   , is.na(states_fixed2) ~ "Other"
                                   , TRUE ~ states_fixed2)) |> 
  group_by(states_fixed2) |> 
  summarize(n_govt = sum(desc_org == "MDA", na.rm = TRUE)
            , n_private = sum(desc_org == "Private Business", na.rm=TRUE)
            , n_ngo = sum(desc_org == "NGO", na.rm=TRUE)
            , n_ingo = sum(desc_org == "INGO", na.rm=TRUE)) |> 
  mutate(total = n_govt + n_private + n_ngo + n_ingo
         , percent_govt = paste0(round(n_govt/total*100, digits = 0), "%", " (", n_govt, ")")
         , percent_private = paste0(round(n_private/total*100, digits = 0), "%", " (", n_private, ")")
         , percent_ngo = paste0(round(n_ngo/total*100, digits = 0), "%", " (", n_ngo, ")")
         , percent_ingo = paste0(round(n_ingo/total*100, digits =0), "%", " (", n_ingo, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")" )) |>  
  dplyr::select("State" = states_fixed2
         , "MDAs" = percent_govt
         , "Private businesses" = percent_private
         , "NGOs" = percent_ngo
         , "INGOs" = percent_ingo
         , "Total" = percent_total)

# grantee orgs
df_grantee <- dat6_non6 |> 
  group_by(grantee) |> 
  count() |> 
  mutate(total = nrow(dat6_non6)
         , percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |>
  dplyr::select("Grantee" = grantee
         , "Percent" = percent)

# teeth orgs
df_teeth <- dat6_non6 |> 
  group_by(teeth) |> 
  count() |> 
  mutate(total = nrow(dat6_non6)
         , percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |>
  dplyr::select("Teeth or voice org." = teeth
         , "Percent" = percent)


#this calculates percents.
private_orgs <- round(length(dat5_non5$desc_org[dat5_non5$desc_org=="Private Business"])/(nrow(dat5_non5))*100, 0)

govt_orgs <- round(length(dat5_non5$desc_org[dat5_non5$desc_org=="MDA"])/(nrow(dat5_non5))*100, 0)

ngo_orgs <- round(length(dat5_non5$desc_org[dat5_non5$desc_org=="NGO"])/(nrow(dat5_non5))*100, 0)

ingo_orgs <- round(length(dat5_non5$desc_org[dat5_non5$desc_org=="INGO"])/(nrow(dat5_non5))*100, 0)



#calculating the level of collaboration
df_collab <- matches_non |> 
  summarize(n_high = sum(level_collab1 == "High", na.rm = TRUE)
            , n_med = sum(level_collab1 == "Medium", na.rm = TRUE)
            , n_low = sum(level_collab1 == "Low", na.rm = TRUE)) |> 
  mutate(total = n_high + n_med + n_low 
         , percent_high = round(n_high/total*100, digits = 0)
         , percent_med =  round(n_med/total*100, digits = 0)
         , percent_low =  round(n_low/total*100, digits = 0)
         )

df_collab_ft <- df_collab |>                             
  mutate(percent_high = paste0(round(n_high/total*100, digits = 0), "%", " (", n_high, ")")
         , percent_med = paste0(round(n_med/total*100, digits = 0), "%", " (", n_med, ")")
         , percent_low = paste0(round(n_low/total*100, digits = 0), "%", " (", n_low, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")" )) |> 
  dplyr::select("High collaboration" = percent_high
         , "Medium collaboration" = percent_med
         , "Low collaboration" = percent_low
         , "Total" = percent_total)

df_collab_high <- matches_non |> 
  group_by(from_cohort, to_cohort) |> 
  summarize(n_high = sum(level_collab1 == "High", na.rm = TRUE)
            , .groups = "drop") |> 
  complete(from_cohort, to_cohort, fill = list(n_high = 0))

df_collab_med <-  matches_non |> 
  group_by(from_cohort, to_cohort) |> 
  summarize(n_med = sum(level_collab1 == "Medium", na.rm = TRUE)
            , .groups = "drop") |> 
  complete(from_cohort, to_cohort, fill = list(n_med = 0))

df_collab_low <-  matches_non |> 
  group_by(from_cohort, to_cohort) |> 
  summarize(n_low = sum(level_collab1 == "Low", na.rm = TRUE)
            , .groups = "drop") |> 
  complete(from_cohort, to_cohort, fill = list(n_low = 0))

df_collab_cohort <- df_collab_high |> 
  select(from_cohort, to_cohort, n_high) |> 
  left_join(select(df_collab_med, from_cohort, to_cohort, n_med),
            by = c("from_cohort", "to_cohort")) |> 
  left_join(select(df_collab_low,from_cohort, to_cohort, n_low),
            by = c("from_cohort", "to_cohort")) |> 
  mutate(total = n_high + n_med + n_low
        , percent_high = paste0(round(n_high / total*100, digits = 0), "%", " (", n_high, ")")
         , percent_med = paste0(round(n_med / total*100, digits = 0), "%", " (", n_med, ")")
         , percent_low = paste0(round(n_low / total*100, digits = 0), "%", " (", n_low, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_cohort
         , "To" = to_cohort
         , "High Collaboration" = percent_high
         , "Medium Collaboration" = percent_med
         , "Low Collaboration" = percent_low
         , "Total" = percent_total)

#connections by grantee
df_collab_grantee <- matches_non |> 
  group_by(from_grantee, to_grantee) |> 
  summarize(n_high = sum(level_collab1 == "High", na.rm = TRUE)
            , n_med = sum(level_collab1 == "Medium", na.rm = TRUE)
            , n_low = sum(level_collab1 == "Low", na.rm = TRUE)
  ) |> 
  mutate(total = n_high + n_med + n_low
         , percent_high = round(n_high / total*100, digits = 0)
         , percent_med =  round(n_med / total*100, digits = 0)
         , percent_low =  round(n_low / total*100, digits = 0))
         
df_collab_grantee_ft <- df_collab_grantee |>       
  mutate(percent_high = paste0(round(n_high / total*100, digits = 0), "%", " (", n_high, ")")
         , percent_med = paste0(round(n_med / total*100, digits = 0), "%", " (", n_med, ")") 
         , percent_low = paste0(round(n_low / total*100, digits = 0), "%", " (", n_low, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_grantee
         , "To" = to_grantee
         , "High collaboration" = percent_high
         , "Medium collaboration" = percent_med
         , "Low collaboration" = percent_low
         , "Total" = percent_total)

df_collab_grantee
#connections by org type
df_collab_desc_org <- matches_non |> 
  group_by(from_desc_org, to_desc_org) |> 
  summarize(n_high = sum(level_collab1 == "High", na.rm = TRUE)
            , n_med = sum(level_collab1 == "Medium", na.rm = TRUE)
            , n_low = sum(level_collab1 == "Low", na.rm = TRUE)
  ) |> 
  mutate(total = n_high + n_med + n_low
         , percent_high = round(n_high / total*100, digits = 0)
         , percent_med = round(n_med / total*100, digits = 0) 
         , percent_low = round(n_low / total*100, digits = 0))

#organization by teeth or voice
teeth_orgs <- dat5_non5 |> 
  group_by(teeth) |> 
  count() |> 
  ungroup() |> 
  mutate(percent_teeth = n/sum(n)*100)

teeth_orgs_ft <- teeth_orgs |> 
  mutate(percent_teeth = paste0(round(percent_teeth, digits = 0), "%", " (", n, ")"))

teeth_orgs_desc <- dat5_non5 |> 
  group_by(teeth, desc_org) |> 
  count() 

#connections by teeth
df_teeth <- matches_non |> 
  group_by(from_teeth, to_teeth) |> 
  count() 

df_collab_teeth <- matches_non |> 
  group_by(from_teeth, to_teeth) |> 
  summarize(n_high = sum(level_collab1 == "High", na.rm = TRUE)
            , n_med = sum(level_collab1 == "Medium", na.rm = TRUE)
            , n_low = sum(level_collab1 == "Low", na.rm = TRUE)
  ) |> 
  mutate(total = n_high + n_med + n_low
         , percent_high = round(n_high / total*100, digits = 0)
         , percent_med = round(n_med / total*100, digits = 0) 
         , percent_low = round(n_low / total*100, digits = 0))


df_collab_teeth_ft <- df_collab_teeth |> 
  mutate(percent_high = paste0(round(n_high/total*100, digits = 0), "%", " (", n_high, ")")
         , percent_med = paste0(round(n_med/total*100, digits = 0), "%", " (", n_med, ")")
         , percent_low = paste0(round(n_low/total*100, digits = 0), "%", " (", n_low, ")")
    , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_teeth
                , "To" = to_teeth
                , "High Collaboration" = percent_high
                , "Medium Collaboration" = percent_med
                , "Low Collaboration" = percent_low
                , "Total" = percent_total)

# frequency of collaborations
df_collab_freq <- matches_non |> 
  summarize(weekly = sum(freq_collab == "Weekly", na.rm = TRUE)
            , monthly = sum(freq_collab == "Monthly", na.rm = TRUE)
            , quarterly = sum(freq_collab == "Quarterly", na.rm = TRUE)
            , annually = sum(freq_collab == "Annually", na.rm = TRUE)) |> 
  mutate(total = weekly + monthly + quarterly + annually 
         , percent_weekly = paste0(round(weekly/total*100, digits = 0), "%", " (", weekly, ")")
         , percent_monthly = paste0(round(monthly/total*100, digits = 0), "%", " (", monthly, ")")
         , percent_quarterly = paste0(round(quarterly/total*100, digits = 0), "%", " (", quarterly, ")")
         , percent_annually = paste0(round(annually/total*100, digits = 0), "%", " (", annually, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("Weekly Collaboration" = percent_weekly
         , "Monthly Collaboration" = percent_monthly
         , "Quarterly Collaboration" = percent_quarterly
         , "Annual Collaboration" = percent_annually
         , "Total" = percent_total)

#collab freq grantee
df_collab_freq_grantee <- matches_non |> 
  group_by(from_grantee, to_grantee) |> 
  summarize(weekly = sum(freq_collab == "Weekly", na.rm = TRUE)
            , monthly = sum(freq_collab == "Monthly", na.rm = TRUE)
            , quarterly = sum(freq_collab == "Quarterly", na.rm = TRUE)
            , annually = sum(freq_collab == "Annually", na.rm = TRUE)) |> 
  mutate(total = weekly + monthly + quarterly + annually
         , percent_weekly = paste0(round(weekly/total*100, digits = 0), "%", " (", weekly, ")")
         , percent_monthly = paste0(round(monthly/total*100, digits = 0), "%", " (", monthly, ")")
         , percent_quarterly = paste0(round(quarterly/total*100, digits = 0), "%", " (", quarterly, ")")
         , percent_annually = paste0(round(annually/total*100, digits = 0), "%", " (", annually, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_grantee
         , "To" = to_grantee
         , "Weekly" = percent_weekly
         , "Monthly" = percent_monthly
         , "Quarterly" = percent_quarterly
         , "Annually" = percent_annually
         , "Total" = percent_total)

#collab freq teeth
df_collab_freq_teeth <- matches_non |> 
  group_by(from_teeth, to_teeth) |> 
  summarize(weekly = sum(freq_collab == "Weekly", na.rm = TRUE)
            , monthly = sum(freq_collab == "Monthly", na.rm = TRUE)
            , quarterly = sum(freq_collab == "Quarterly", na.rm = TRUE)
            , annually = sum(freq_collab == "Annually", na.rm = TRUE)) |> 
  mutate(total = weekly + monthly + quarterly + annually
         , percent_weekly = paste0(round(weekly/total*100, digits = 0), "%", " (", weekly, ")")
         , percent_monthly = paste0(round(monthly/total*100, digits = 0), "%", " (", monthly, ")")
         , percent_quarterly = paste0(round(quarterly/total*100, digits = 0), "%", " (", quarterly, ")")
         , percent_annually = paste0(round(annually/total*100, digits = 0), "%", " (", annually, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_teeth
         , "To" = to_teeth
         , "Weekly" = percent_weekly
         , "Monthly" = percent_monthly
         , "Quarterly" = percent_quarterly
         , "Annually" = percent_annually
         , "Total" = percent_total)
##collab freq org type
df_collab_freq_desc_org <- matches_non |> 
  group_by(from_desc_org, to_desc_org) |> 
  summarize(weekly = sum(freq_collab == "Weekly", na.rm = TRUE)
            , monthly = sum(freq_collab == "Monthly", na.rm = TRUE)
            , quarterly = sum(freq_collab == "Quarterly", na.rm = TRUE)
            , annually = sum(freq_collab == "Annually", na.rm = TRUE)) |> 
  mutate(total = weekly + monthly + quarterly + annually 
         , percent_weekly = paste0(round(weekly/total*100, digits = 0), "%", " (", weekly, ")")
         , percent_monthly = paste0(round(monthly/total*100, digits = 0), "%", " (", monthly, ")")
         , percent_quarterly = paste0(round(quarterly/total*100, digits = 0), "%", " (", quarterly, ")")
         , percent_annually = paste0(round(annually/total*100, digits = 0), "%", " (", annually, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("From" = from_desc_org
         , "To" = to_desc_org
         , "Weekly" = percent_weekly
         , "Monthly" = percent_monthly
         , "Quarterly" = percent_quarterly
         , "Annually" = percent_annually
         , "Total" = percent_total)

#collab freq of level of collaboration
collab_freq_level <- matches_non |> 
  group_by(level_collab1) |> 
  summarize(weekly = sum(freq_collab == "Weekly", na.rm = TRUE)
            , monthly = sum(freq_collab == "Monthly", na.rm = TRUE)
            , quarterly = sum(freq_collab == "Quarterly", na.rm = TRUE)
            , annually = sum(freq_collab == "Annually", na.rm = TRUE)) |> 
  mutate(total = weekly + monthly + quarterly + annually
         , percent_weekly = paste0(round(weekly/total*100, digits = 0), "%", " (", weekly, ")")
         , percent_monthly = paste0(round(monthly/total*100, digits = 0), "%", " (", monthly, ")")
         , percent_quarterly = paste0(round(quarterly/total*100, digits = 0), "%", " (", quarterly, ")")
         , percent_annually = paste0(round(annually/total*100, digits = 0), "%", " (", annually, ")")
         , percent_total = paste0(round(total/sum(total)*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("Level of Collaboration" = level_collab1
         , "Weekly" = percent_weekly
         , "Monthly" = percent_monthly
         , "Quarterly" = percent_quarterly
         , "Annually" = percent_annually
         , "Total" = percent_total) |> 
  arrange(desc(Total))


# Tables on collaboration
# Define colors
header_color <- "#00457c"
row_color <- "#EDF0DF"

#Scenarios

df_scenario <- dat6_non6 |> 
  filter(!is.na(scenarios) &
           scenarios != "Don’t know/Decline to answer") |> 
  group_by(scenarios) |> 
  count()

# complementary tactics
df_compl_tactics <- matches_non |> 
  mutate(compl_tactics1 = case_when(str_detect(compl_tactics, "High") ~ "High"
                                    , str_detect(compl_tactics, "Medium") ~ "Medium"
                                    , str_detect(compl_tactics, "Low") ~ "Low"
                                    , TRUE ~ compl_tactics)) |> 
  group_by(compl_tactics1) |> 
  count() |> 
  ungroup() |> 
  mutate(total = nrow(matches_non)
         , percent = round(n/total*100, digits = 0))
  
df_strat_collab <- matches_non |> 
    mutate(strat_collab1 = case_when(str_detect(strat_collab, "High") ~ "High"
                                     , str_detect(strat_collab, "Medium") ~ "Medium"
                                     , str_detect(strat_collab, "Low") ~ "Low"
                                     , TRUE ~ strat_collab)) |> 
  group_by(strat_collab1) |> 
  count() |> 
  mutate(total = nrow(matches_non)
         , percent = round(n/total*100, digits = 0))

df_strat_collab_cohort <- matches_non |> 
  mutate(strat_collab1 = case_when(str_detect(strat_collab, "High") ~ "High"
                                   , str_detect(strat_collab, "Medium") ~ "Medium"
                                   , str_detect(strat_collab, "Low") ~ "Low"
                                   , TRUE ~ strat_collab)) |> 
  filter(strat_collab1 != "Don't Know/Decline to Answer") |> 
  group_by(from_cohort, to_cohort, strat_collab1) |> 
  count() |> 
  pivot_wider(names_from = strat_collab1
              , values_from = n
              , values_fill = 0) |> 
  mutate(total = High + Medium + Low
         , percent_high = paste0(round(High/total *100, digits = 0), "%", " (", High, ")")
         , percent_med = paste0(round(Medium/total *100, digits = 0), "%", " (", Medium, ")")
         , percent_low = paste0(round(Low/total *100, digits = 0), "%", " (", Low, ")")
         , Total = paste0(round(total/sum(total) *100, digits = 0), "%", " (", total, ")")
         ) |> 
  dplyr::select("From Cohort" = from_cohort
                , "To Cohort" = to_cohort
                , "High" = percent_high
                , "Medium" = percent_med
                , "Low" = percent_low
                , Total)

df_strat_level_collab <- matches_non |> 
  mutate(strat_collab1 = case_when(str_detect(strat_collab, "High") ~ "High"
                                   , str_detect(strat_collab, "Medium") ~ "Medium"
                                   , str_detect(strat_collab, "Low") ~ "Low"
                                   , TRUE ~ strat_collab)) |> 
  filter(strat_collab1 != "Don't Know/Decline to Answer") |> 
  group_by(level_collab1, strat_collab1) |>
  count()



df_soc_acct_approach <- dat6_non6 |> 
  filter(soc_acct_approaches != "Don’t know/Decline to answer") |>
  mutate(soc_acct_approaches1 = case_when(soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
         , soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
         , soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
         , soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
         , TRUE ~ soc_acct_approaches)) |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches1)) |> 
         #, soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "A"
                                           # , soc_acct_approaches %in% "Approach B" ~ "B"
                                          #  , soc_acct_approaches %in% "Approach C" ~ "C"
                                            #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                            #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                            #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                            #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                           # , TRUE ~ soc_acct_approaches)) |> 
  group_by(soc_acct_approaches1) |> 
  count() |> 
  mutate(total = nrow(dat6_non6),
         percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")"),
         soc_acct_approaches1 = factor(soc_acct_approaches1, levels = c("Approach A", "Approach B", "Approach C", "A and B", "A and C", "B and C", "A, B, and C"))) |>
  arrange(soc_acct_approaches1) |> 
  dplyr::select("Social Accountability Approach" = soc_acct_approaches1
              , "Percent" = percent) 
  

df_soc_acct_approaches_totals <- dat6_non6 |> 
  #group_by(soc_acct_approaches) |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  filter(soc_acct_approaches != "Don’t know/Decline to answer") |> 
         #& !is.na(so_acct_approaches)) |> 
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches)
         , soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "Approach A"
                                            , soc_acct_approaches %in% "Approach B" ~ "Approach B"
                                            , soc_acct_approaches %in% "Approach C" ~ "Approach C"
                                            #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                            #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                            #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                            #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                            , TRUE ~ soc_acct_approaches)) |> 
  group_by(soc_acct_approaches1) |> 
  count()  |> 
  mutate(total = nrow(dat6_non6),
    percent = paste0(round(n/total*100, digits = 0), "%", " (", n, ")")) |> 
  dplyr::select("Social Accountability Approach" = soc_acct_approaches1
                , "Percent" = percent)

#soc_acct_approaches
df_soc_acct_approaches_org <- dat6_non6 |> 
  #group_by(soc_acct_approaches) |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches)
    , soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "A"
                                          , soc_acct_approaches %in% "Approach B" ~ "B"
                                          , soc_acct_approaches %in% "Approach C" ~ "C"
                                          #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                          #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                          #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                          #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                          , TRUE ~ soc_acct_approaches)) |> 
  group_by(desc_org, soc_acct_approaches1) |> 
  count() |> 
  pivot_wider(names_from = soc_acct_approaches1
              , values_from = n
              , values_fill = 0) # |> 

df_soc_acct_approaches_grantee <-  dat6_non6 |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches)
         , soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "A"
                                            , soc_acct_approaches %in% "Approach B" ~ "B"
                                            , soc_acct_approaches %in% "Approach C" ~ "C"
                                            #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                            #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                            #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                            #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                            , TRUE ~ soc_acct_approaches)) |> 
  group_by(grantee, soc_acct_approaches1) |> 
  count() |> 
  pivot_wider(names_from = soc_acct_approaches1
              , values_from = n
              , values_fill = 0) 

soc_acct_approaches_teeth <- dat6_non6 |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches)
         , soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "A"
                                            , soc_acct_approaches %in% "Approach B" ~ "B"
                                            , soc_acct_approaches %in% "Approach C" ~ "C"
                                            #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                            #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                            #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                            #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                            , TRUE ~ soc_acct_approaches)) |> 
  group_by(teeth, soc_acct_approaches1) |> 
  count() |> 
  pivot_wider(names_from = soc_acct_approaches1
              , values_from = n
              , values_fill = 0)

soc_acct_approaches_cohort <- dat6_non6 |> 
  separate_rows(soc_acct_approaches, sep = "; ") |>
  mutate(soc_acct_approaches1 = str_trim(soc_acct_approaches)
         , soc_acct_approaches1 = case_when(soc_acct_approaches %in% "Approach A" ~ "A"
                                            , soc_acct_approaches %in% "Approach B" ~ "B"
                                            , soc_acct_approaches %in% "Approach C" ~ "C"
                                            #, soc_acct_approaches == "Approach A; Approach C" ~ "A and C"
                                            #, soc_acct_approaches == "Approach A; Approach B" ~ "A and B"
                                            #, soc_acct_approaches == "Approach B; Approach C" ~ "B and C"
                                            #, soc_acct_approaches == "Approach A; Approach B; Approach C" ~ "A, B, and C"
                                            , TRUE ~ soc_acct_approaches)) |> 
  group_by(cohort, soc_acct_approaches1) |> 
  count() |> 
  pivot_wider(names_from = soc_acct_approaches1
              , values_from = n
              , values_fill = 0)


#ways that orgs collaborate

collab_list <- c("Resource sharing (e.g., sharing of organizational resources or expertise and human resources, pooling funding for certain events)"
                 , "Joint anticorruption advocacy (e.g., amplifying messages through media about corruption-related cases or work)"
                 , "Learning, information, and knowledge sharing (e.g., leveraging each other’s network/contact sharing, attending anticorruption training and capacity building workshops)"
                 , "Joint anticorruption activity design or implementation (e.g., media support to investigate or co-write about corruption cases)"
                 , "Legal services")


ways_collab_df <- matches_non |> 
  separate_rows(ways_collab.x, sep = "; ") |> 
  mutate(ways_collab = case_when(ways_collab.x %in% collab_list ~ ways_collab.x
                                 , TRUE ~ "Other")) |> 
  group_by(ways_collab) |> 
  count() |> 
  arrange(desc(n))


  

ways_collab_grantee <- matches_non |> 
  separate_rows(ways_collab.x, sep = "; ") |> 
  mutate(ways_collab = case_when(ways_collab.x %in% collab_list ~ ways_collab.x
                                 , TRUE ~ "Other")) |> 
  group_by(ways_collab, from_grantee, to_grantee) |> 
  count() 

ways_collab_grantee_ft <- ways_collab_grantee |> 
  mutate(grantee_pair = paste(from_grantee, to_grantee, sep = "-")) |> 
  ungroup() |> 
  dplyr::select(ways_collab, grantee_pair, n) |> 
  pivot_wider(names_from = grantee_pair,
              values_from = n,
              #values_fill = 0  # fill missing combos with 0s
  ) |> 
  filter(ways_collab != "Other") |> 
  mutate(total = `Grantee-Grantee` + `Grantee-Non Grantee` + `Non Grantee-Grantee` + `Non Grantee-Non Grantee`
          , percent_gg = paste0(round(`Grantee-Grantee`/total*100), "%", " (", `Grantee-Grantee`, ")")
          , percent_gn = paste0(round(`Grantee-Non Grantee`/total*100), "%", " (", `Grantee-Non Grantee`, ")")
          , percent_ng = paste0(round(`Non Grantee-Grantee`/total*100), "%", " (", `Non Grantee-Grantee`, ")")
          , percent_nn = paste0(round(`Non Grantee-Non Grantee`/total*100), "%", " (", `Non Grantee-Non Grantee`, ")")
          , Total = paste0(round(total/330*100, digits = 0), "%", " (", total, ")")) |> 
  dplyr::select("Type of Collaboration" = ways_collab
                , 'Grantee - Grantee' = percent_gg
                , "Grantee - Non-Grantee" = percent_gn
                , "Non-Grantee - Grantee" = percent_ng
                , "Non-Grantee - Non-Grantee" = percent_nn
                , Total)
  
ways_collab_grantee_ft$`Type of Collaboration` <- ways_collab_grantee_ft$`Type of Collaboration` |> 
  recode("Resource sharing (e.g., sharing of organizational resources or expertise and human resources, pooling funding for certain events)" = "Resource sharing"
         , "Joint anticorruption advocacy (e.g., amplifying messages through media about corruption-related cases or work)" = "Joint anticorruption advocacy"
         , "Learning, information, and knowledge sharing (e.g., leveraging each other’s network/contact sharing, attending anticorruption training and capacity building workshops)" = "Learning, information, and knowledge sharing"
         , "Joint anticorruption activity design or implementation (e.g., media support to investigate or co-write about corruption cases)" = "Joint anticorruption activity design or implementation"
         )

  ##|> 
 # flextable() |> 
  #set_caption(as_paragraph(as_b("Figure 2: Grantee and Non Grantee Collaboration")
  #                         , "\n"), align_with_table = FALSE) |> 
  #align(j = NULL, align = "left", part = "caption") |> 
  #bold(part = "header") |>  # Make column labels bold
  #color(part = "header", color = header_color) |>  # Corrected header text color
  #bg(i = seq(1, nrow(df_collab_grantee), by = 2), bg = row_color, part = "body") |>  # Alternate row colors
  #autofit() |> 
  #align(align = "center", part = "all")



# Define colors
header_color <- "#00457c"
row_color <- "#EDF0DF"

df_collab_grantee_ft <- df_collab_grantee_ft |> 
  #select(from_grantee, to_grantee, percent_high, percent_med, percent_low) |> 
  #mutate(percent_high = paste0(percent_high, "%", " (", n_high, ")")
        # , percent_med = paste0(percent_med, "%", " (", n_med, ")")
         #, percent_low = paste0(percent_low, "%", " (", n_low, ")")) |> 
  dplyr::select(From, To, 'High collaboration', 'Medium collaboration'
                , 'Low collaboration', Total) |> 
  flextable() |> 
  #set_caption(as_paragraph(as_b("Figure 2: Grantee and Non Grantee Collaboration")
  #                         , "\n"), align_with_table = FALSE) |> 
  #align(j = NULL, align = "left", part = "caption") |> 
  bold(part = "header") |>  # Make column labels bold
  color(part = "header", color = header_color) |>  # Corrected header text color
  bg(i = seq(1, nrow(df_collab_grantee), by = 2), bg = row_color, part = "body") |>  # Alternate row colors
  autofit() |> 
  align(align = "center", part = "all")
  

