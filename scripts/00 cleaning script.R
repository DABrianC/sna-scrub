
#This script cleans the dataset 

#pulls in all the packages that are needed
source(here::here("./prep/prep.R"))

#reads in the data from worksheet 1 and cleans the names
#dat <- read_xlsx(here::here("./data/SNA Endline Grantee Survey_Clean Data_working.xlsx")) |> 
 # janitor::clean_names()

### Endline grantee data ----

#reads in the data from worksheet 2 and cleans the names
dat <- readxl::read_xlsx((here::here("./data/SNA Endline Grantee Survey_Clean Data_working.xlsx"))
                      , sheet = 2) |> 
                       janitor::clean_names()

#rename columns in dat object
dat1 <- dat |> 
  dplyr::rename(from = org_name
                , desc_org = which_of_the_following_best_describes_your_organization
                , state = in_what_state_is_your_organization_located_if_your_organization_has_multiple_offices_select_all_that_apply
                , work_fed_local = does_your_organization_work_primarily_at_the_federal_level_or_within_states
                , states_work = in_what_states_does_your_organization_work_multiple_responses_allowed
                , pos_title = what_is_your_position_title_at_your_organization
                , pers_work_ac = do_you_personally_directly_work_on_anti_corruption_related_activities_programs_projects_or_efforts_with_other_on_nigeria_grantee_organizations_or_external_non_grantee_organizations
                , others_work_ac = are_there_other_individuals_at_your_organization_that_work_directly_on_anticorruption_projects_with_support_from_the_mac_arthur_foundation
                , two_names_emails = please_provide_up_to_two_names_titles_and_email_addresses_of_individuals_at_your_organization_in_the_space_provided
                , ac_proj_names = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_please_list_all_current_mac_arthur_foundation_funded_projects
                , scenarios = please_read_the_two_scenarios_below_and_select_the_answer_choice_that_best_applies_to_your_organization_s_work_scenario_a_my_organization_supports_the_development_and_enforcement_of_laws_and_regulations_we_support_implementation_of_systems_for_transparency_and_or_monitor_compliance_with_existing_laws_and_regulations_we_directly_and_or_indirectly_support_use_of_incentives_to_discourage_corruption_and_sanctions_to_punish_it_scenario_b_my_organization_demands_accountability_advocates_for_reforms_engages_citizens_in_anticorruption_issues_such_as_promoting_changes_in_attitudes_and_behaviors_monitors_public_projects_and_legal_compliance_and_or_publishes_reports_on_corruption_and_anticorruption_issues
                , soc_acct_types = social_accountability_refers_to_the_ways_in_which_citizens_work_with_state_institutions_and_the_responsiveness_and_performance_of_those_institutions_social_accountability_strategies_try_to_improve_public_sector_performance_by_bolstering_both_citizen_engagement_and_government_responsiveness_please_select_all_the_types_of_social_accountability_strategies_your_organization_engages_in_as_part_of_your_on_nigeria_anti_corruption_work_my_organization
                , soc_acct_approaches = the_answer_choices_below_describe_different_social_accountability_approaches_these_are_general_descriptions_so_you_may_find_that_no_description_completely_fits_your_organization_s_on_nigeria_anticorruption_work_therefore_please_select_all_the_choices_that_may_apply_to_your_organization_s_anticorruption_work_approach_a_my_organization_s_work_seeks_to_hold_governments_accountable_by_supporting_participation_in_formal_political_processes_like_elections_and_or_by_supporting_citizens_to_organize_and_collectively_advocate_for_government_officials_and_or_agencies_demand_explanations_or_raise_awareness_of_government_performance_approach_b_my_organization_s_work_seeks_to_develop_and_or_support_internal_checks_and_oversight_processes_within_and_between_government_officials_or_institutions_so_that_they_can_hold_one_another_accountable_we_help_state_actors_investigate_and_sanction_irregularities_such_as_corrupt_behavior_or_procedural_violations_approach_c_my_organization_s_work_seeks_to_create_and_or_support_spaces_or_processes_in_which_citizens_participate_in_oversight_processes_or_directly_oversee_the_performance_of_public_or_private_sector_officials_agencies_institutions_and_or_organizations
                , org1 = name_of_organization_1_114
                , level_collab_org1 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_115
                , ways_collab_org1 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_116
                , init_collab_org1 = who_generally_initiates_the_collaboration_123
                , desc_collab_org1 = what_description_best_applies_to_your_collaboration_with_this_organization_124
                , freq_collab_org1 = how_often_do_you_usually_collaborate_125
                , compl_tactics_org1 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_126
                , strat_collab_org1 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_127
                , prov_contact_info_org1 = will_you_provide_contact_information_for_the_collaborating_organization_128
                , another_org1 = do_you_have_another_organization_to_add_130
                , org2 = name_of_organization_2_131
                , level_collab_org2 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_132
                , ways_collab_org2 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_133
                , init_collab_org2 = who_generally_initiates_the_collaboration_140
                , desc_collab_org2 = what_description_best_applies_to_your_collaboration_with_this_organization_141
                , freq_collab_org2 = how_often_do_you_usually_collaborate_142
                , compl_tactics_org2 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_143
                , strat_collab_org2 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_144
                , prov_contact_info_org2 = will_you_provide_contact_information_for_the_collaborating_organization_145
                , another_org2 = do_you_have_another_organization_to_add_147
                , org3 = name_of_organization_3_148
                , level_collab_org3 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_149
                , ways_collab_org3 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_150
                , init_collab_org3 = who_generally_initiates_the_collaboration_157
                , desc_collab_org3 = what_description_best_applies_to_your_collaboration_with_this_organization_158
                , freq_collab_org3 = how_often_do_you_usually_collaborate_159
                , compl_tactics_org3 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_160
                , strat_collab_org3 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_161
                , prov_contact_info_org3 = will_you_provide_contact_information_for_the_collaborating_organization_162
                , another_org3 = do_you_have_another_organization_to_add_164
                , org4 = name_of_organization_4_165
                , level_collab_org4 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_166
                , ways_collab_org4 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_167
                , init_collab_org4 = who_generally_initiates_the_collaboration_174
                , desc_collab_org4 = what_description_best_applies_to_your_collaboration_with_this_organization_175
                , freq_collab_org4 = how_often_do_you_usually_collaborate_176
                , compl_tactics_org4 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_177
                , strat_collab_org4 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_178
                , prov_contact_info_org4 = will_you_provide_contact_information_for_the_collaborating_organization_179
                , another_org4 = do_you_have_another_organization_to_add_181
                , org5 = name_of_organization_5_182
                , level_collab_org5 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_183
                , ways_collab_org5 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_184
                , init_collab_org5 = who_generally_initiates_the_collaboration_191
                , desc_collab_org5 = what_description_best_applies_to_your_collaboration_with_this_organization_192
                , freq_collab_org5 = how_often_do_you_usually_collaborate_193
                , compl_tactics_org5 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_194
                , strat_collab_org5 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_195
                , prov_contact_info_org5 = will_you_provide_contact_information_for_the_collaborating_organization_196
                , desc_ac_collab_2024 = please_provide_a_brief_description_of_an_anticorruption_related_collaboration_in_2024_that_you_are_most_proud_of_how_did_you_collaborate_and_what_results_did_you_achieve
                , approach_a = please_provide_a_brief_example_of_approach_a_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , approach_b = please_provide_a_brief_example_of_approach_b_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , fut_collab = is_there_an_organization_that_you_plan_to_collaborate_with_on_your_anticorruption_efforts_in_the_future_please_provide_the_organization_s_name_and_why_you_wish_to_collaborate_with_them
                , fgd_part = thank_you_for_your_participation_we_will_be_selecting_a_few_respondents_to_conduct_a_focus_group_discussion_with_representatives_from_multiple_on_nigeria_grantee_organizations_so_that_we_can_better_understand_and_contextualize_the_responses_you_ve_provided_selected_respondents_would_be_invited_to_participate_in_one_60_to_90_minute_group_discussion_would_you_be_interested_in_participating_in_a_focus_group_discussion
                , fgd_contact_name = please_provide_your_full_name_and_email_address_where_we_may_contact_you_to_participate_in_a_focus_group_discussion
                , fgd_contact_email = x205
                , eval_stakehlds = as_part_of_the_final_evaluation_of_on_nigeria_in_2025_we_hope_to_hear_from_accountability_ecosystem_actors_throughout_nigeria_thinking_about_your_work_who_are_the_non_grantee_stakeholders_that_are_most_important_for_us_to_speak_with_during_the_final_evaluation_separate_from_data_collection_related_to_this_social_network_analysis_please_list_up_to_three_individuals_these_can_be_the_same_as_or_different_from_the_individuals_you_listed_in_the_table_in_section_c_for_each_provide_their_organization_title_and_contact_information
                , contact_info_org1 = x129
                , contact_info_org2 = x146
                , contact_info_org3 = x163
                , contact_info_org4 = x180
                , contact_info_org5 = x197
                , org6 = name_of_organization_1_198
                , level_collab_org6 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_199
                , ways_collab_org6 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_200
                , init_collab_org6 = who_generally_initiates_the_collaboration_207
                , desc_collab_org6 = what_description_best_applies_to_your_collaboration_with_this_organization_208
                , freq_collab_org6 = how_often_do_you_usually_collaborate_209
                , compl_tactics_org6 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_210
                , strat_collab_org6 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_211
                , prov_contact_info_org6 = will_you_provide_contact_information_for_the_collaborating_organization_212
                , contact_info_org6 = x213
                , another_org6 = do_you_have_another_organization_to_add_214
                , org7 = name_of_organization_2_215
                , level_collab_org7 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_216
                , ways_collab_org7 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_217
                , init_collab_org7 = who_generally_initiates_the_collaboration_224
                , desc_collab_org7 = what_description_best_applies_to_your_collaboration_with_this_organization_225
                , freq_collab_org7 = how_often_do_you_usually_collaborate_226
                , compl_tactics_org7 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_227
                , strat_collab_org7 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_228
                , prov_contact_info_org7 = will_you_provide_contact_information_for_the_collaborating_organization_229
                , contact_info_org7 = x230
                , another_org7 = do_you_have_another_organization_to_add_231
                , org8 = name_of_organization_3_232
                , level_collab_org8 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_233
                , ways_collab_org8 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_234
                , init_collab_org8 = who_generally_initiates_the_collaboration_241
                , desc_collab_org8 = what_description_best_applies_to_your_collaboration_with_this_organization_242
                , freq_collab_org8 = how_often_do_you_usually_collaborate_243
                , compl_tactics_org8 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_244
                , strat_collab_org8 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_245
                , prov_contact_info_org8 = will_you_provide_contact_information_for_the_collaborating_organization_246
                , contact_info_org8 = x247
                , another_org8 = do_you_have_another_organization_to_add_248
                , org9 = name_of_organization_4_249
                , level_collab_org9 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_250
                , ways_collab_org9 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_251
                , init_collab_org9 = who_generally_initiates_the_collaboration_258
                , desc_collab_org9 = what_description_best_applies_to_your_collaboration_with_this_organization_259
                , freq_collab_org9 = how_often_do_you_usually_collaborate_260
                , compl_tactics_org9 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_261
                , strat_collab_org9 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_262
                , prov_contact_info_org9 = will_you_provide_contact_information_for_the_collaborating_organization_263
                , contact_info_org9 = x264
                , another_org9 = do_you_have_another_organization_to_add_265
                , org10 = name_of_organization_5_266
                , level_collab_org10 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_267
                , ways_collab_org10 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_268
                , init_collab_org10 = who_generally_initiates_the_collaboration_275
                , desc_collab_org10 = what_description_best_applies_to_your_collaboration_with_this_organization_276
                , freq_collab_org10 = how_often_do_you_usually_collaborate_277
                , compl_tactics_org10 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_278
                , strat_collab_org10 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_279
                , prov_contact_info_org10 = will_you_provide_contact_information_for_the_collaborating_organization_280
                , contact_info_org10 = x281
                , another_org10 = do_you_have_another_organization_to_add_282
                , org11 = name_of_organization_6
                , level_collab_org11 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_284
                , ways_collab_org11 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_285
                , init_collab_org11 = who_generally_initiates_the_collaboration_292
                , desc_collab_org11 = what_description_best_applies_to_your_collaboration_with_this_organization_293
                , freq_collab_org11 = how_often_do_you_usually_collaborate_294
                , compl_tactics_org11 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_295
                , strat_collab_org11 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_296
                , prov_contact_info_org11 = will_you_provide_contact_information_for_the_collaborating_organization_297
                , contact_info_org11 = x298
                , another_org11 = do_you_have_another_organization_to_add_299
                , org12 = name_of_organization_7
                , level_collab_org12 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_301
                , ways_collab_org12 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_302
                , init_collab_org12 = who_generally_initiates_the_collaboration_309
                , desc_collab_org12 = what_description_best_applies_to_your_collaboration_with_this_organization_310
                , freq_collab_org12 = how_often_do_you_usually_collaborate_311
                , compl_tactics_org12 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_312
                , strat_collab_org12 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_313
                , prov_contact_info_org12 = will_you_provide_contact_information_for_the_collaborating_organization_314
                , contact_info_org12 = x315
                , another_org12 = do_you_have_another_organization_to_add_316
                , org13 = name_of_organization_8
                , level_collab_org13 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_318
                , ways_collab_org13 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_319
                , init_collab_org13 = who_generally_initiates_the_collaboration_326
                , desc_collab_org13 = what_description_best_applies_to_your_collaboration_with_this_organization_327
                , freq_collab_org13 = how_often_do_you_usually_collaborate_328
                , compl_tactics_org13 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_329
                , strat_collab_org13 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_330
                , prov_contact_info_org13 = will_you_provide_contact_information_for_the_collaborating_organization_331
                , contact_info_org13 = x332
                , another_org13 = do_you_have_another_organization_to_add_333
                , org14 = name_of_organization_9
                , level_collab_org14 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_335
                , ways_collab_org14 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_336
                , init_collab_org14 = who_generally_initiates_the_collaboration_343
                , desc_collab_org14 = what_description_best_applies_to_your_collaboration_with_this_organization_344
                , freq_collab_org14 = how_often_do_you_usually_collaborate_345
                , compl_tactics_org14 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_346
                , strat_collab_org14 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_347
                , prov_contact_info_org14 = will_you_provide_contact_information_for_the_collaborating_organization_348
                , contact_info_org14 = x349
                , another_org14 = do_you_have_another_organization_to_add_350
  ) |> 
  dplyr::select(-x206)




#do some pivots and uniting to tidy it
dat2 <- dat1[-1,] |> 
  pivot_longer(cols = module:x10
               , names_to = NULL
               , values_to = "module"
               , values_drop_na = TRUE) |> 
  tidyr::unite(col = "state", state:x50
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "states_work", states_work:x89
           , sep = "; "
           , remove = TRUE
           , na.rm = TRUE) |>
  unite(col = "desc_org", desc_org:x12
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "two_names_emails", two_names_emails:x94
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ac_proj_names", ac_proj_names:x99
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "soc_acct_types", soc_acct_types:x109
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "soc_acct_approaches", soc_acct_approaches:x113
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org1", ways_collab_org1:x122
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org2", ways_collab_org2:x139
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org3", ways_collab_org3:x156
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org4", ways_collab_org4:x173
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org5", ways_collab_org5:x190
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org6", ways_collab_org6:x204
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |>
  unite(col = "ways_collab_org7", ways_collab_org7:x223
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |>
  unite(col = "ways_collab_org8", ways_collab_org8:x240
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org9", ways_collab_org9:x257
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org10", ways_collab_org10:x274
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org11", ways_collab_org11:x291
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org12", ways_collab_org12:x308
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org13", ways_collab_org13:x325
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org14", ways_collab_org14:x342
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
    unite(col = "fgd_part", fgd_part:x358
        , sep = ", "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "eval_stakehlds", eval_stakehlds:x364
        , sep = ", "
        , remove = TRUE
        , na.rm = TRUE)

#need to pivot the  data so that 
#there's a single column for the organizations identified in the survey
#Step one is to pivot it longer

#the questions of partners variables are in .value column
#the to organization number is in the group column
#responses to questions are in the value column
dat3 <- dat2 |>
  pivot_longer(cols = matches(".*org\\d+$|_org\\d+$")
               , names_to = c(".values", "group")
               , names_pattern = "(.*?)(?:_org)?(\\d+)$"
               , values_drop_na = TRUE) 

# Step 2 is to then pivot it wider and filter out the NAs
# since it kept a row for up to 14 rows for each 
dat4 <- dat3 |>  
  pivot_wider(names_from = .values
              , values_from = value) |> 
  filter(!is.na(org)) |> 
  rename(to = org) #removes blank rows based on the org column

dat4$to <- dat4$to |> 
  str_trim() |> 
  recode("Accountability Lab" = "Accountability Lab Nigeria"
         , "Independent Corrupt Practices and Other Related Offences" = "ICPC" 
         , "Independent Corrupt Practices Commission (ICPC)" = "ICPC" 
         , "Independent Corrupt Practices Commission" = "ICPC" 
         , "Independent Corrupt Practices and other related Offences Commission (ICPC)" = "ICPC"
         , "Independence Corruption Practices and Other Related Offences Commission" = "ICPC"
         , "The Independent Corrupt Practices and Other Related Offences Commission (ICPC)" = "ICPC" 
         , "Independent Corrupt Practices and other related Offences Commission (ICPC)" = "ICPC"
         , "The Independent Corrupt Practices and Other Related Offenses Commission (ICPC)" = "ICPC"
         , "Independent Corrupt Practices and Other Related Offences Commission" = "ICPC"
         , "ACAN"  = "ICPC Anti-Corruption Academy of Nigeria"
         , "ICPC/ACAN" = "ICPC Anti-Corruption Academy of Nigeria"
         , "Africa Centre for Media, Information and Literacy (AFRICMIL)" = "African Centre for Media and Information Literacy (AFRIMIL)"
         , "AFRIMIL" = "African Centre for Media and Information Literacy (AFRIMIL)"
         , "Nigerian Police Force (Kindly ignor the previous contact given for NPF please)" = "Nigerian Police Force"
         , "TheCable Foundation" = "Cable Newspaper Journalism Foundation"
         , "The Cable" = "Cable Newspaper Journalism Foundation"
         , "Policy and Legal Advo" = "Policy and Legal Advocacy Centre (PLAC)"
         , "Policy Legal and Advocacy Centre (PLAC)" = "Policy and Legal Advocacy Centre (PLAC)"
         , "Independent National Electoral Commission (INEC)" = "Independent National Electoral Commission"
         , "Wole Soyinka center," = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "Wole Soyinka Centre for Investigative Journalism" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "WOLE SOTINKA CENTRE FOR INVESTIGATIVE JOURNALISM" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "Wole Soyinka Centre for Investigative Journalism" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "Code of Conduct Bereau" = "Code of Conduct Bureau"
         , "Economic and Financial Crimes Commission (EFCC)" = "Economic and Financial Crimes Commission"
         , "Mumbayya House, Kano" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training" # I don't know if this is correct
         , "Mambayya House, Kano" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training"
         , "African Center for Leadership, Strategy and Development" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "ICIR" = "The International Centre for Investigative Reporting (ICIR)"
         , "nclusive Skills Development Initiative ISDI" = "Inclusive Skills Development Initiative (ISDI)"
         , "Policy Legal and Advocacy Centre  (PLAC)" = "Policy and Legal Advocacy Centre (PLAC)"
         , "ECONOMIC AND FINANCIAL CRIMES COMMISSION (EFCC)" = "Economic and Financial Crimes Commission"
         , "Africa Centre for Media &Information Literacy (AFRICMIL))" = "African Centre for Media and Information Literacy (AFRIMIL)"
         , "Mambayya House, Center for Democratic Development" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training"
         , "African Center for Media and Information Literacy" = "African Centre for Media and Information Literacy (AFRIMIL)"
         , "Shehu Musa Yar Adua Foundation" = "Shehu Musa Yar’Adua Foundation"
         , "Shehu Musa Yar'adua Foundation" = "Shehu Musa Yar’Adua Foundation"
         , "Shehu Musa Yar’adua Foundation" = "Shehu Musa Yar’Adua Foundation"
         , "Yar'Adua Foundation" = "Shehu Musa Yar’Adua Foundation"
         , "EFCC" = "Economic and Financial Crimes Commission"
         , "Centre for Democracy & Development (CDD)" = "Centre for Democracy and Development"
         , "PRIMORG" = "Progressive Impact Organization for Community Development"
         , "Primorg" = "Progressive Impact Organization for Community Development"
         , "Centre for Fiscal Transparency and Integrity Watch" = "Center for Fiscal Transparency and Public Integrity"
         , "CABLENEWS" = "Cable Newspaper Journalism Foundation"
         , "Budgit" = "BudgIT Foundation"
         , "BudgIT" = "BudgIT Foundation"
         , "FRSC" = "Federal Road Safety Corps"
         , "FIDA" = "International Federation of Women Lawyers (FIDA)"
         , "Mambayya house" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training"
         , "Women Radio" = "Women Radio 91.7"
         , "Center for Information Technology and Development" = "Centre for Information Technology and Development (CITAD)"
         , "CITAD" = "Centre for Information Technology and Development (CITAD)"
         , "HumAngle" = "HumAngle Media Limited"
         , "HumAngle Foundation" = "HumAngle Media Limited"
         , "Arewa24" = "AREWA24"
         , "NIPSS" = "National Institute for Policy and Strategic Studies, Kuru, Nigeria"
         , "Connected Development" = "Connected Development (CODE)"
         , "NIFU" = "Nigerian Financial Intelligence Unit"
         , "Moving Image" = "Moving Image Limited"
         , "CHRICED" = "Resource Centre for Human Rights and Civic Education (CHRICED)"
         , "Code of Conduct Bureau,  Abuja" = "Code of Conduct Bureau"
         , "Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)" = "Partners West Africa Nigeria" 
         , "PLSI" = "Paradigm Leadership Support Initiative (PLSI)"
         , "Wole Soyinka Center for Investigative Journalism (WSCIJ)" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "WAJSIC" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "Public Complaint and Anti-Corruption Commission." = "Public Complaints & Anti Corruption Commission"
         , "Kano State Public Complaint and Anti-Corruption Commission (PCACC)" = "Public Complaints & Anti Corruption Commission"
         , "Kano State Public Complaints and Anti-Corruption Commission" = "Public Complaints & Anti Corruption Commission"
         , "Lux Terra Foundation" = "Lux Terra Leadership Foundation"
         , "Integrity Organisation" = "Integrity"
         , "Integrity Organization" = "Integrity"
         , "Centre for Media Policy and Accountability" = "Centre for Media, Policy and Accountability"
         , "NISER" = "Nigerian Institute for Social and Economic Research"
         , "HEDA Resource" = "HEDA Resource Centre"
         , "Tiger Eye Foundation" = "Tiger Eye Social Foundation"
         , "Centre for Social Legal Studies" = "Centre for Socio-Legal Studies"
         , "WRAPA" = "Women's Rights Advancement and Protection Alternative"
         , "Nigerian Police Force" = "Nigeria Police Force"
         , "Administration of Criminal Justice Monitoring Committee (ACJMC)" = "Administration of Criminal Justice Monitoring Committee"
         , "YAR'DUA CENTRE" = "Shehu Musa Yar’Adua Foundation"
         , "SERAP" = "Socio-Economic Rights and Accountability Project"
         , "HDI" = "UNDP"
         , "CJID" = "Centre for Journalism Innovation and Development (CJID)"
         , "FIJ" = "Foundation for Investigative Journalism"
         , "PremiumTimes" = "Premium Times"
         , "Bayero University Kano" = "Bayero University, Kano"
         , "Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)" = "Partners West Africa Nigeria"
         , "African Centre for Leadership Strategy & Development (Centre LSD)" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "African Centre for Leadership, strategy and development (Centre LSD)" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "African Centre for Leadership Strategy and Development (Centre LSD)" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "All farmers Association of Nigeria (AFAN)" = "All Farmers Association of Nigeria (AFAN)"
         , "BBC Africa Eye" = "BBC"
         , "Civic Media lab" = "New Thoughts Media Support Foundation"
         , "Daily Trust" = "Daily Trust Foundation"
         , "Federal Radio Coperation of Nigeria" = "Federal Radio Cooperation of Nigeria"
         , "Mambayya House, the Aminu Kano Centre for Democratic Research and Training" = "Mambayya House"
         , "Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)" = "Partners West Africa Nigeria"
         , "International Centre for Investigative Journalism" = "The International Centre for Investigative Reporting (ICIR)"
         , "Civic media lab" = "New Thoughts Media Support Foundation" 
         , "Daily Trust" = "Daily Trust Foundation"
         , "Alliance for African (Afa)" = "Alliance for Africa (AFA)"
         , "Connected Development(CODE)" = "Connected Development (CODE)"
         , "Basic Right Watch" = "Basic Rights Watch"
         , "Anticorruption Academy of Nigeria" = "ICPC Anti-Corruption Academy of Nigeria"
         , "The Anti-Corruption Academy of Nigeria" = "ICPC Anti-Corruption Academy of Nigeria"
         , "Mambayya House" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training"
         , "Nigeria Police Force" = "Nigerian Police Force"
         , "The International Centre for Investigative Journalism" = "The International Centre for Investigative Reporting (ICIR)"
         , "Wikki Times" = "WikkiTimes Media Limited"
         , "Christian center for Mission, family and Leadership development ( CEMFLED)" = "Christian Centre for Mission, family and Leadership development (CEMFLED)"
         , "Foundation for Investigative Journalism Nigeria" = "Foundation for Investigative Journalism"
         , "Ikenga online" = "IkengaOnline"
         , "United Nation Office on Drugs and Crimes (UNODC)" = "United Nations Office on Drugs and Crime (UNODC)"
         , "Civic Media Lab" = "New Thoughts Media Support Foundation"
         , "Partners West Africa" = "Partners West Africa Nigeria"
         , "EEEI-Engage Empower Educate Initiative" = "Engage Empower Educate Initiate (EEEI)"
         , "Women in Media (WiM)" = "African Women in Media (AWiM)"
         , "African Women in Media" = "African Women in Media (AWiM)"
         , "JAMB" = "Joint Admissions and Matriculation Board (JAMB)"
         , "Alliance for Africa (AfA)" = "Alliances for Africa (AfA)"
         , "Human Development Initiatives" = "Human Development Initiaties Foundation"
         , "NESG/PIC" = "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)"
         , "Policy Innovation Centre" = "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)"
         , "The International Centre for Investigative Reporting" = "The International Centre for Investigative Reporting (ICIR)"
         , "NFIU" = "Nigerian Financial Intelligence Unit (NFIU)") 

dat4$from <- dat4$from |> 
  recode("ICPC /ACAN" = "ICPC Anti-Corruption Academy of Nigeria"
         , "PROGRESSIVE IMPACT ORGANIZATION FOR COMMUNITY DEVELOPMENT" = "Progressive Impact Organization for Community Development"
         , "African Center for Leadership Strategy and Development (Centre LSD)" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "African Centre for Leadership Strategy and Development (Centre LSD)" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "Center for Fiscal Transparency and Integrity Watch" = "Center for Fiscal Transparency and Public Integrity"
         , "Cable Newspaper journalism foundation" = "Cable Newspaper Journalism Foundation"
         , "HumAngle Media" = "HumAngle Media Limited"
         , "UNODC" = "United Nations Office on Drugs and Crime (UNODC)"
         , "Al-habibiyyah Islamic Society" = "Al-Habibiya Islamic Society"
         , "Behavioral Insights Team (BIT)" = "Behavioural Insights Team (BIT)"
         , "Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)" = "Partners West Africa Nigeria"
         , "SIGNATURE DEVELOPMENT AND MEDIA FOUNDATION" = "Signature Development and Media Foundation"
         , "Daria Media Nigeria Likited" = "Daria Media Nigeria Limited"
         , "CLEEN FOUNDATION" = "CLEEN Foundation"
         , "Connected Development(CODE)" = "Connected Development (CODE)"
         , "Nigerian Institute of Social and Economic Research" = "Nigerian Institute for Social and Economic Research"
         , "Policy Innovation Centre" = "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)"
         )


#remove the row with Collaborative Media Engagement for Development, Inclusivity and Accountability 26 partners

dat4 <- dat4 |> 
  filter(to != "Collaborative Media Engagement for Development, Inclusivity and Accountability 26 partners")

#add the 26 partners for CMEDIA project

cmedia <- readxl::read_xlsx(here::here("./data/CMEDIA orgs_12-12-24.xlsx"))

cmedia$to <- cmedia$to |> 
  str_trim() |> 
  recode("Ikenga online" = "IkengaOnline"
         , "Wikki Times" = "WikkiTimes Media Limited"
         , "Frontfoot Media Initiative" = "FrontFoot Media Initiative")

dat4 <- dat4 |> 
  bind_rows(cmedia)

#split BIT/Griot into two rows

bit_griot <- dat4 |> 
  filter(to == "BIT/GRIOT") |> 
  mutate(to_split = str_split(to, "/")) |> 
  unnest(to_split) 

bit_griot$to <- bit_griot$to_split

bit_griot$to <- bit_griot$to |> 
  recode("BIT" = "Behavioural Insights Team (BIT)"
         , "GRIOT" = "Griot Studios")

bit_griot <- bit_griot[,-36]

dat4 <- dat4 |> 
  filter(to != "BIT/GRIOT")

dat4 <- dat4 |> 
  bind_rows(bit_griot)

dat4 <- dat4 |> 
  mutate(module = case_when(
    from == "UNDP" ~ "JoinBodi"
    , from == "Behavioural Insights Team (BIT)" ~ "Behavior Change"
    , TRUE ~ module
  ))



#remove american research center
dat4 <- dat4 |> 
  filter(from != "Accountability Research Center (American University)")

#fix an issue with African Centre for Leadership, Strategy
# and Development (Centre LSD)

dat4 <- dat4 |> 
  mutate(to = case_when(from == "African Centre for Leadership, Strategy and Development (Centre LSD)"
                        & to == "African Centre for Leadership, Strategy and Development (Centre LSD)" ~ "Shehu Musa Yar’Adua Foundation"
                        , TRUE ~ to))

#write this to an excel file
#This is the whole dataset after cleaning
#writexl::write_xlsx(dat4
 #                  , path = "./data/SNA Endline Grantee Survey_CleanData_12-12-24.xlsx")



#Create a list of all the organizations in the dataset and then join
# it with the columns that are associated with the "to" orgs
edgelist_from <- dat4 |> 
  select(3) |> 
  unique()

edgelist_to <- dat4 |>
  select(to) |> 
  unique()

edgelist_all <- bind_rows(edgelist_from, edgelist_to) |>
  unite("all", from:to, na.rm = TRUE, remove = FALSE) |>
  distinct(all, .keep_all = TRUE)

#We need to have a single column with all the orgs listed
#This contains all the metadata for the grantee organizations
dat_joined <- edgelist_all |> 
  full_join(dat4, by = join_by("all" == "from")
            , keep = TRUE) |> 
  distinct(all
          , .keep_all = TRUE) |> 
  select(all, desc_org, state, work_fed_local, states_work
         , ac_proj_names, scenarios, soc_acct_types, soc_acct_approaches
         , desc_ac_collab_2024, module, group, to.y, to.x)

dat_joined$cohort <- case_when(is.na(dat_joined$module) ~ "Non-grantee"
                         , TRUE ~ dat_joined$module)

dat_joined$grantee <- case_when(dat_joined$cohort == "Non-grantee" ~ "Non-grantee"
                                , TRUE ~ "Grantee")


#This is our master list of organizations and their data
#writexl::write_xlsx(dat_joined
 #                   , path = "./data/SNA Endline Grantee Survey_CleanData_11-19-24.xlsx")

#This is a list of connections with all the attributes that 
# I've associated with the "to" organizations
matches <- dat4 |> 
  select(from, to, level_collab, ways_collab, init_collab, desc_collab
         , freq_collab, compl_tactics, strat_collab)

#writexl::write_xlsx(matches
 #                    , path = "./data/SNA Endline Grantee Survey_connections_11-19-24.xlsx")

g <- graph_from_data_frame(matches
                           , directed = TRUE
                           , vertices = dat_joined
                           )



### Endline non-grantee data ----

non <- readxl::read_xlsx((here::here("./data/SNA Endline Non Grantee Survey_Clean Data_working.xlsx"))) |> 
  janitor::clean_names()

non1 <- non |> 
dplyr::rename(from = org_name
              , desc_org = which_of_the_following_best_describes_your_organization
              , state = in_what_state_is_your_organization_s_primary_office_located
              , work_fed_local = does_your_organization_work_primarily_at_the_federal_level_or_within_states
              , states_work = in_addition_to_your_primary_office_in_what_states_does_your_organization_work_select_all_that_apply
              , pos_title = what_is_your_title_position_at_your_organization
              , pers_work_ac = do_you_personally_directly_work_on_anticorruption_related_activities_programs_projects_or_efforts_in_collaboration_with_other_organizations
              , others_work_ac = if_you_do_not_personally_directly_work_on_anticorruption_activities_programs_projects_or_efforts_in_collaboration_with_other_organizations_are_there_other_individuals_at_your_organization_that_work_directly_on_anticorruption_related_activities_and_programs
              , two_names_emails = since_you_do_not_personally_directly_work_with_any_other_organizations_we_would_like_to_share_this_survey_with_other_individuals_at_your_organization_that_work_directly_on_anticorruption_related_activities_and_programs_in_collaboration_with_other_organizations_please_provide_up_to_two_names_titles_and_email_addresses_of_individuals_at_your_organization_in_the_space_provided
              #, ac_proj_names = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_please_list_all_current_mac_arthur_foundation_funded_projects
              , scenarios = please_read_the_two_scenarios_below_and_select_the_answer_choice_that_best_applies_to_your_organization_s_work_scenario_a_my_organization_supports_the_development_and_enforcement_of_laws_and_regulations_we_support_implementation_of_systems_for_transparency_and_or_monitor_compliance_with_existing_laws_and_regulations_we_directly_and_or_indirectly_support_the_use_of_incentives_to_discourage_corruption_and_sanctions_to_punish_it_scenario_b_my_organization_demands_accountability_advocates_for_reforms_engages_citizens_in_anticorruption_issues_such_as_promoting_changes_in_attitudes_and_behaviors_monitors_public_projects_and_legal_compliance_and_or_publishes_reports_on_corruption_and_anticorruption_issues
              , soc_acct_types = social_accountability_refers_to_the_ways_in_which_citizens_work_with_state_institutions_and_the_responsiveness_and_performance_of_those_institutions_social_accountability_strategies_try_to_improve_public_sector_performance_by_bolstering_both_citizen_engagement_and_government_responsiveness_please_select_all_the_types_of_social_accountability_strategies_your_organization_engages_in_as_part_of_your_anticorruption_work_my_organization
              , soc_acct_approaches = the_answer_choices_below_describe_different_social_accountability_approaches_these_are_general_descriptions_so_you_may_find_that_no_description_completely_fits_your_organization_s_anticorruption_work_therefore_please_select_all_the_choices_that_may_apply_to_your_organization_s_anticorruption_work_approach_a_my_organization_seeks_to_hold_governments_accountable_by_supporting_participation_in_formal_political_processes_like_elections_and_or_by_supporting_citizens_to_organize_and_collectively_advocate_for_government_officials_and_or_agencies_demand_explanations_or_raise_awareness_of_government_performance_approach_b_my_organization_seeks_to_develop_and_or_support_internal_checks_and_oversight_processes_within_and_between_government_officials_or_institutions_so_that_they_can_hold_one_another_accountable_we_help_state_actors_investigate_and_sanction_irregularities_such_as_corrupt_behavior_or_procedural_violations_approach_c_my_organization_seeks_to_create_and_or_support_spaces_or_processes_in_which_citizens_participate_in_or_directly_oversee_the_performance_of_public_or_private_sector_officials_agencies_institutions_and_or_organizations
              , org1 = name_of_organization_1if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , org1_other = x76
              , level_collab_org1 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_77
              , ways_collab_org1 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_78
              , init_collab_org1 = who_generally_initiates_the_collaboration_85
              , desc_collab_org1 = what_description_best_applies_to_your_collaboration_with_this_organization_86
              , freq_collab_org1 = how_often_do_you_usually_collaborate_87
              , compl_tactics_org1 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_88
              , strat_collab_org1 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_89
              #, prov_contact_info_org1 = will_you_provide_contact_information_for_the_collaborating_organization_128
              , another_org1 = do_you_have_another_organization_to_add_90
              , org2 = name_of_organization_2if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org2 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_93
              , ways_collab_org2 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_94
              , init_collab_org2 = who_generally_initiates_the_collaboration_101
              , desc_collab_org2 = what_description_best_applies_to_your_collaboration_with_this_organization_102
              , freq_collab_org2 = how_often_do_you_usually_collaborate_103
              , compl_tactics_org2 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_104
              , strat_collab_org2 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_105
              #, prov_contact_info_org2 = will_you_provide_contact_information_for_the_collaborating_organization_145
              , another_org2 = do_you_have_another_organization_to_add_106
              , org3 = name_of_organization_3if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org3 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_109
              , ways_collab_org3 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_110
              , init_collab_org3 = who_generally_initiates_the_collaboration_117
              , desc_collab_org3 = what_description_best_applies_to_your_collaboration_with_this_organization_118
              , freq_collab_org3 = how_often_do_you_usually_collaborate_119
              , compl_tactics_org3 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_120
              , strat_collab_org3 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_121
              #, prov_contact_info_org3 = will_you_provide_contact_information_for_the_collaborating_organization_162
              , another_org3 = do_you_have_another_organization_to_add_122
              , org4 = name_of_organization_4if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org4 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_125
              , ways_collab_org4 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_126
              , init_collab_org4 = who_generally_initiates_the_collaboration_133
              , desc_collab_org4 = what_description_best_applies_to_your_collaboration_with_this_organization_134
              , freq_collab_org4 = how_often_do_you_usually_collaborate_135
              , compl_tactics_org4 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_136
              , strat_collab_org4 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_137
              #, prov_contact_info_org4 = will_you_provide_contact_information_for_the_collaborating_organization_179
              , another_org4 = do_you_have_another_organization_to_add_138
              , org5 = name_of_organization_5if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org5 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_141
              , ways_collab_org5 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_142
              , init_collab_org5 = who_generally_initiates_the_collaboration_149
              , desc_collab_org5 = what_description_best_applies_to_your_collaboration_with_this_organization_150
              , freq_collab_org5 = how_often_do_you_usually_collaborate_151
              , compl_tactics_org5 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_152
              , strat_collab_org5 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_153
              #, prov_contact_info_org5 = will_you_provide_contact_information_for_the_collaborating_organization_196
              , another_org5 = do_you_have_another_organization_to_add_154
              , org6 = name_of_organization_6if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org6 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_157
              , ways_collab_org6 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_158
              , init_collab_org6 = who_generally_initiates_the_collaboration_165
              , desc_collab_org6 = what_description_best_applies_to_your_collaboration_with_this_organization_166
              , freq_collab_org6 = how_often_do_you_usually_collaborate_167
              , compl_tactics_org6 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_168
              , strat_collab_org6 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_169
              , another_org6 = do_you_have_another_organization_to_add_170
              , org7 = name_of_organization_7if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org7 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_173
              , ways_collab_org7 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_174
              , init_collab_org7 = who_generally_initiates_the_collaboration_181
              , desc_collab_org7 = what_description_best_applies_to_your_collaboration_with_this_organization_182
              , freq_collab_org7 = how_often_do_you_usually_collaborate_183
              , compl_tactics_org7 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_184
              , strat_collab_org7 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_185
              , another_org7 = do_you_have_another_organization_to_add_186
              , org8 = name_of_organization_8if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org8 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_189
              , ways_collab_org8 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_190
              , init_collab_org8 = who_generally_initiates_the_collaboration_197
              , desc_collab_org8 = what_description_best_applies_to_your_collaboration_with_this_organization_198
              , freq_collab_org8 = how_often_do_you_usually_collaborate_199
              , compl_tactics_org8 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_200
              , strat_collab_org8 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_201
              , another_org8 = do_you_have_another_organization_to_add_202
              , org9 = name_of_organization_9if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org9 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_205
              , ways_collab_org9 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_206
              , init_collab_org9 = who_generally_initiates_the_collaboration_213
              , desc_collab_org9 = what_description_best_applies_to_your_collaboration_with_this_organization_214
              , freq_collab_org9 = how_often_do_you_usually_collaborate_215
              , compl_tactics_org9 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_216
              , strat_collab_org9 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_217
              , another_org9 = do_you_have_another_organization_to_add_218
              , org10 = name_of_organization_10if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org10 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_221
              , ways_collab_org10 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_222
              , init_collab_org10 = who_generally_initiates_the_collaboration_229
              , desc_collab_org10 = what_description_best_applies_to_your_collaboration_with_this_organization_230
              , freq_collab_org10 = how_often_do_you_usually_collaborate_231
              , compl_tactics_org10 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_232
              , strat_collab_org10 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_233
              , another_org10 = do_you_have_another_organization_to_add_234
              , org11 = name_of_organization_11if_you_dont_see_the_name_of_the_organization_you_would_like_to_add_please_select_other_and_include_the_name_in_the_comment_box
              , level_collab_org11 = how_would_you_describe_the_level_of_your_collaboration_on_anticorruption_efforts_with_this_organization_237
              , ways_collab_org11 = in_what_ways_do_you_collaborate_with_this_organization_on_anticorruption_select_all_that_apply_238
              , init_collab_org11 = who_generally_initiates_the_collaboration_245
              , desc_collab_org11 = what_description_best_applies_to_your_collaboration_with_this_organization_246
              , freq_collab_org11 = how_often_do_you_usually_collaborate_247
              , compl_tactics_org11 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_248
              , strat_collab_org11 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anticorruption_laws_policies_and_or_regulations_249
              , another_org11 = do_you_have_another_organization_to_add_250
              , desc_ac_collab_2021_2024 = please_provide_a_brief_description_of_an_anti_corruption_related_collaboration_since_2021_that_you_are_most_proud_of_with_whom_did_you_collaborate_how_did_you_collaborate_and_what_results_did_you_achieve
              , approach_a = please_provide_a_brief_example_of_approach_a_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization_please_mention_the_specific_organizations_you_collaborated_with_on_this_example
              , approach_b = please_provide_a_brief_example_of_approach_b_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization_please_mention_the_specific_organizations_you_collaborated_with_on_this_example
              , want_collab_org = is_there_an_organization_you_are_not_currently_collaborating_with_that_you_plan_to_or_feel_it_would_be_important_to_collaborate_with_on_your_anticorruption_efforts_in_the_near_future_please_provide_the_organization_s_name_and_why_you_wish_to_collaborate_with_them
              #, fut_collab = is_there_an_organization_that_you_plan_to_collaborate_with_on_your_anticorruption_efforts_in_the_future_please_provide_the_organization_s_name_and_why_you_wish_to_collaborate_with_them
              , fgd_part = thank_you_for_your_participation_we_will_be_selecting_a_few_respondents_to_conduct_a_focus_group_discussion_with_representatives_from_multiple_organizations_that_collaborate_with_on_nigeria_grantees_so_that_we_can_better_understand_and_contextualize_the_responses_you_ve_provided_selected_respondents_would_be_invited_to_participate_in_one_60_to_90_minute_group_discussion_would_you_be_interested_in_participating_in_a_focus_group_discussion
              #, fgd_contact_name = please_provide_your_full_name_and_email_address_where_we_may_contact_you_to_participate_in_a_focus_group_discussion
              , fgd_contact_email = x264
              , org_works_ac = does_your_organization_do_any_work_related_to_reducing_corruption_strengthening_transparency_and_or_improving_government_accountability
              
)

non2 <- non1[-1,] |> 
  select(-consent, -ignore_5, - ignore_6, -ignore_7, - ignore_8
         , - ignore_9, -ignore_10) |> 
  mutate(desc_org = case_when(desc_org == "Other (please specify)" ~ x12
            , TRUE ~ desc_org)) |> 
  select(-x12) |> 
  tidyr::unite(col = "states_work", states_work:x52
               , sep = "; "
               , remove = TRUE
               , na.rm = TRUE) |> 
  tidyr::unite(col = "soc_acct_types", soc_acct_types:x70
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  tidyr::unite(col = "two_names_emails", two_names_emails:x60
               , sep = "; "
               , remove = TRUE
               , na.rm = TRUE) |> 
  tidyr::unite(col = "soc_acct_approaches", soc_acct_approaches:x74
               , sep = "; "
               , remove = TRUE
               , na.rm = TRUE) |> 
  mutate(org1 = case_when(org1 == "0ther" ~ org1_other
                          , TRUE ~ org1)) |> 
  unite(col = "ways_collab_org1", ways_collab_org1:x84
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  mutate(org2 = case_when(org2 == "0ther" ~ x92
                          , TRUE ~ org2))  |> 
  unite(col = "ways_collab_org2", ways_collab_org2:x100
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  mutate(org3 = case_when(org3 == "0ther" ~ x108
                          , TRUE ~ org3)) |> 
  unite(col = "ways_collab_org3", ways_collab_org3:x116
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  mutate(org4 = case_when(org4 == "0ther" ~ x124
                          , TRUE ~ org4)) |> 
  unite(col = "ways_collab_org4", ways_collab_org4:x132
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org5", ways_collab_org5:x148
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org6", ways_collab_org6:x164
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org7", ways_collab_org7:x180
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org8", ways_collab_org8:x196
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |>
  unite(col = "ways_collab_org9", ways_collab_org9:x212
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  unite(col = "ways_collab_org10", ways_collab_org10:x228
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  mutate(org10 = case_when(org10 == "0ther" ~ x220
                           , TRUE ~ org10)) |> 
  unite(col = "ways_collab_org11", ways_collab_org11:x244
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |> 
  mutate(org11 = case_when(org11 == "0ther" ~ x236
                           , TRUE ~ org11)) |> 
  select(-c(x124, x108, x92
            , pos_title, pers_work_ac, others_work_ac
            , two_names_emails, org1_other, another_org1
            , another_org2, another_org3, another_org4, 
            , another_org5, another_org6, another_org6
            , another_org7, another_org8, another_org9
            , another_org10, fgd_part, please_provide_your_full_name_and_email_address_where_we_may_contact_you_to_participate_in_a_focus_group_discussion
            , x257, x258, x259, x260, x261, x262, x263, fgd_contact_email
            , x265)) #removing columns not needed for SNA

#collapses all the questions about the relationships into columns
non3 <- non2 |> 
  pivot_longer(cols = matches(".*org\\d+$|_org\\d+$")
               , names_to = c(".values", "group")
               , names_pattern = "(.*?)(?:_org)?(\\d+)$"
               , values_drop_na = TRUE)


# Step 2 is to then pivot it wider and filter out the NAs
# since it kept a row for up to 14 rows for each 
non4 <- non3 |>  
  pivot_wider(names_from = .values
              , values_from = value) |> 
  filter(!is.na(org)) |> 
  rename(to = org) |> #removes blank rows based on the org column
  select(-c(x140, x156, x172, x188, x204,
            x220, x236)) #removing some extra blank columns

# unique(non4$from)
# need to clarify names for 
# ICPC
# HDI
# NFIU

#rename some orgs in "from" column
non4$from <- non4$from |> 
  str_trim() |> 
  recode("CIvic Media Lab" = "New Thoughts Media Support Foundation"
         , "Civic Media Lab" = "New Thoughts Media Support Foundation"
         , "Christian centre for Mission, family and Leadership development (CEMFLED)" = "Christian Centre for Mission, family and Leadership development (CEMFLED)"
         , "Wikki Times" = "WikkiTimes Media Limited"
         , "HDI" = "Human Development Initiatives Foundation"
         , "NFIU" = "Nigerian Financial Intelligence Unit (NFIU)")

non4$to <- non4$to |> 
  str_trim() |> 
  recode("AD4R-TV" = "AD 4 Radio"
         , "African Centre for Leadership, Strategy & Development" = "African Centre for Leadership, Strategy and Development (Centre LSD)"
         , "African Centre for Media & Information Literacy" = "African Centre for Media and Information Literacy (AFRIMIL)"
         , "Centre for Democracy and Development (Nigeria)" = "Centre for Democracy and Development"
         , "Centre for Journalism Innovation and Development" = "Centre for Journalism Innovation and Development (CJID)"
         , "Civil Society Legislative Advocacy Centre" = "Civil Society Legislative Advocacy Centre (CISLAC)"
         , "Connected Development Initiative" = "Connected Development (CODE)"
         , "FUTVIEW SERVICES LTD" = "Futview Services, Ltd."
         , "International Centre for Investigative Reporting" = "The International Centre for Investigative Reporting (ICIR)"
         , "Joint National Association of Persons with Disabilities" = "Joint National Association of Persons with Disabilities (JONAPWD)"
         , "Mambayya House, Aminu Kano Center for Democratic Research" = "Mambayya House, the Aminu Kano Centre for Democratic Research and Training"
         , "National Institute for Policy and Strategic Studies" = "National Institute for Policy and Strategic Studies, Kuru, Nigeria"
         , "Nigeria correctional service" = "Nigeria Correctional Services"
         , "Public and Private Development Centre" = "Public and Private Development Centre (PPDC)"
         , "Socio-Economic Rights & Accountability Project" = "Socio-Economic Rights and Accountability Project"
         , "Step Up Nigeria" = "Step Up for Social Development & Empowerment in Nigeria (Step Up Nigeria)"
         , "Wole Soyinka Centre for Investigative Journalism" = "Wole Soyinka Centre for Investigative Journalism (WSCIJ)"
         , "Al-Habibiyyah Islamic Society" = "Al-Habibiya Islamic Society"
         , "Shehu Musa Yar'Adua Foundation" = "Shehu Musa Yar’Adua Foundation"
         , "SOLACEBASE COMMUNICATIONS LTD" = "Solacebase Communications, Ltd."
         , "Alliance for Africa (AfA)" = "Alliances for Africa (AfA)"
         , "Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)" = "Partners West Africa Nigeria"
         , "Policy Innovation Centre" = "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)"
         , "JAMB" = "Joint Admissions and Matriculation Board (JAMB)"
         , "The International Centre for Investigative Journalism" = "The International Centre for Investigative Reporting (ICIR)"
         , "EEEI-Engage Empower Educate Initiative" = "Engage Empower Educate Initiate (EEEI)"
         , "Women in Media (WiM)" = "African Women in Media (AWiM)"
         , "African Women in Media (AWiM)" = "African Women in Media (AWiM)"
         , "Human Development Initiatives" = "Human Development Initiatives Foundation"
         , "TAPE Initiative" = "Transparency, Accountability, and Performance Excellence (TAPE) Initiative"
         , "ICPC" = "Independent Corrupt Practices Commission (ICPC)"
         , "Safer-Media Initiative" = "Safer Media Initiative")

#unique(non4$to)
# need to clarify names for
# African Centre for Leadership, Strategy & Development
# Centre for Democracy and Development (Nigeria)
# United Nations Office on Drugs and Crime (UNODC)
# ICPC Anti-Corruption Academy of Nigeria
# Rule of Law and Empowerment Initiative also known as Partners West Africa Nigeria (PWAN)
# "0ther"
# Economic and Financial Crimes Commission
# Law Hub Development & Advocacy Centre
# Independent National Electoral Commission

# Join dat4 with non4

dat4_non4 <- bind_rows(dat4, non4, .id = "df_id")

dat4_non4 <- dat4_non4 |> 
  select(-c(others_work_ac, two_names_emails, fgd_contact_email
            #, to_split
            ))

dat4_non4 <- dat4_non4 |> 
  mutate(module = case_when(is.na(module) ~ "Non Grantee"
         , TRUE ~ module))

dat4_non4$to <- dat4_non4$to |> 
  recode("Alliance for Africa (AFA)" = "Alliances for Africa (AFA)"
         , "Center for Information Technology and Development" = "Centre for Information Technology and Development (CITAD)"
         , "Center for Information Technology and Development (CITAD)" = "Centre for Information Technology and Development (CITAD)"
         , "Solace" = "Solacebase Communications, Ltd."
         , "African Women in Media" = "African Women in Media (AWiM)"
         , "Center for Fiscal Transparency and Public integrity" = "Center for Fiscal Transparency and Public Integrity"
         , "ICPC" = "Independent Corrupt Practices Commission (ICPC)")


dat4_non4$from <- dat4_non4$from |> 
  recode("ICPC" = "Independent Corrupt Practices Commission (ICPC)")
#Create a list of all the organizations in the dataset and then join
# it with the columns that are associated with the "to" orgs

all_orgs_non <- data.frame(organization = unique(c(dat4_non4$from, dat4_non4$to))) 


#edgelist_all_dat_non <- bind_rows(edgelist_from_dat_non, edgelist_to_dat_non) |>
 # unite("all", from:to, na.rm = TRUE, remove = FALSE) |>
  #distinct(all, .keep_all = TRUE)


dat5_non5 <- all_orgs_non |> 
  left_join(dat4_non4, by = join_by("organization" == "from")
            , keep = TRUE) |> 
  #rename(all = from.x) |> ## Restart here - 2/10/25
  distinct(organization, .keep_all = TRUE) |> 
  select(organization, df_id, desc_org, state, work_fed_local
         , states_work, ac_proj_names, scenarios, soc_acct_types
         , soc_acct_approaches, ways_collab, cohort = module, group, to) 

dat5_non5 <- dat5_non5 |> 
  mutate(grantee = case_when(cohort != "Non Grantee" ~ "Grantee"
                             , TRUE ~ "Non Grantee")
         , cohort = case_when(is.na(cohort) ~ "Non Grantee"
                              , TRUE ~ cohort)
         , desc_org = case_when(desc_org == "International Non-Governmental Organization (INGO)" ~ "INGO"
                                , TRUE ~ desc_org))

#arrange alphabetically
dat5_non5 <- arrange(dat5_non5, organization)

# add locations to the dat5_non5 object
dat5_non5 <- dat5_non5 |> 
  mutate(states_fixed = case_when(str_detect(state, ";") ~ NA_character_
                                  , is.na(state) ~ NA_character_
                                  , TRUE ~ state))

#write dat5_non5 to disk and add acronyms here
#writexl::write_xlsx(dat5_non5, path = "./data/dat5_non5.xlsx")

acronyms_types <- readxl::read_xlsx(here::here("./data/dat5_non5_acronyms_orgtypes.xlsx"))

dat5_non5 <- dat5_non5 |> 
  left_join(acronyms_types, by = join_by(organization)) |> 
  select(-desc_org.x) |> 
  dplyr::rename(desc_org = desc_org.y)


dat6_non6 <- dat5_non5 |> 
  mutate(states_fixed2 = case_when(organization == "African Centre for Leadership, Strategy and Development (Centre LSD)" ~ "Federal Capital Territory"
                                   , organization == "Arewa Research and Development Project" ~ "Kaduna"
                                   , organization == "AREWA24" ~ "Lagos"
                                   , organization == "BudgIT Foundation" ~ "Lagos"
                                   , organization == "Cable Newspaper Journalism Foundation" ~ "Lagos"
                                   , organization == "Centre for Democracy and Development" ~ "Federal Capital Territory"
                                   , organization == "Centre for Information Technology and Development (CITAD)" ~ "Kano"
                                   , organization == "Behavioural Insights Team (BIT)" ~ "New York"
                                   , organization == "CLEEN Foundation" ~ "Lagos"
                                   , organization == "Connected Development (CODE)" ~ "Federal Capital Territory"
                                   , organization == "HEDA Resource Centre" ~ "Lagos"
                                   , organization == "HumAngle Media Limited" ~ "Federal Capital Territory"
                                   , organization == "Joint National Association of Persons with Disabilities (JONAPWD)" ~ "Federal Capital Territory"
                                   , organization == "Legal Defence and Assistance Project (LEDAP)" ~ "Lagos"
                                   , organization == "Nigerian Institute of Advanced Legal Studies" ~ "Lagos"
                                   , organization == "Nigerian Institute for Social and Economic Research" ~ "Federal Capital Territory"
                                   , organization == "Palace of Priests Assembly" ~ "Federal Capital Territory"
                                   , organization == "Paradigm Leadership Support Initiative (PLSI)" ~ "Federal Capital Territory"
                                   , organization == "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)" ~ "Federal Capital Territory"
                                   , organization == "Public and Private Development Centre (PPDC)" ~ "Federal Capital Territory"
                                   , organization == "Resource Centre for Human Rights and Civic Education (CHRICED)" ~ "Kano"
                                   , organization == "Partners West Africa Nigeria" ~ "Federal Capital Territory"
                                   , organization == "Signature Development and Media Foundation" ~ "Federal Capital Territory"
                                   , organization == "Socio-Economic Rights and Accountability Project" ~ "Lagos"
                                   , organization == "Step Up for Social Development & Empowerment in Nigeria (Step Up Nigeria)" ~ "Federal Capital Territory"
                                   , organization == "Tiger Eye Social Foundation" ~ "Accra, Ghana"
                                   , organization == "UNDP" ~ "Federal Capital Territory"
                                   , organization == "Women's Rights Advancement and Protection Alternative" ~ "Federal Capital Territory"
                                   , organization == "Servicom" ~ "Federal Capital Territory"
                                   , organization == "The International Centre for Investigative Reporting (ICIR)" ~ "Federal Capital Territory"
                                   , organization == "Judiciary" ~ "Federal Capital Territory"
                                   , organization == "Center for Fiscal Transparency and Public integrity" ~ "Federal Capital Territory"
                                   , organization == "Federal Road Safety Corps" ~ "Federal Capital Territory"
                                   , organization == "Secondary Education Board" ~ "Federal Capital Territory"
                                   , organization == "International Federation of Women Lawyers (FIDA)" ~ "Federal Capital Territory"
                                   , organization == "Mambayya House, the Aminu Kano Centre for Democratic Research and Training" ~ "Kano"
                                   , organization == "Human rights Radio" ~ "Federal Capital Territory"
                                   , organization == "FCT Council of Imam Initiatives" ~ "Federal Capital Territory"
                                   , organization == "All Farmers Association of Nigeria (AFAN)" ~ "Federal Capital Territory"
                                   , organization == "Equal Access International" ~ "Washington, DC"
                                   , organization == "Center for Dryland Agriculture, BUK" ~ "Kano"
                                   , organization == "Griot Studios" ~ "Federal Capital Territory"
                                   , organization == "National Youth Service Corp" ~ "Federal Capital Territory"
                                   , organization == "Code of Conduct Bureau" ~ "Federal Capital Territory"
                                   , organization == "BUK Radio and TV" ~ "Kano"
                                   , organization == "Kano State Science Board" ~ "Kano"
                                   , organization == "Federal Ministry of Finance Budget and National Planning" ~ "Federal Capital Territory"
                                   , organization == "Premium Times" ~ "Federal Capital Territory"
                                   , organization == "National Orientation Agency (NOA)" ~ "Federal Capital Territory"
                                   , organization == "Alliances for Africa (AFA)" ~ "Lagos"
                                   , organization == "African Women in Media (AWiM)" ~ "Ondo"
                                   , organization == "The Human Rights Advancement, Development and Advocacy Centre (HURIDAC)" ~ "Ogun"
                                   , organization == "Nigerian Police Force" ~ "Federal Capital Territory"
                                   , organization == "Bureau of Rural and Community Development, Ekiti" ~ "Ekiti"
                                   , organization == "Sustainable Development Goals Office, Ekiti" ~ "Ekiti"
                                   , organization == "Leads Nigeria" ~ "Kaduna"
                                   , organization == "Chatham House" ~ "London, UK"
                                   , organization == "Independent National Electoral Commission" ~ "Federal Capital Territory"
                                   , organization == "The Justice Reform Project" ~ "?"
                                   , organization == "The Maritime Anti-Corruption Network" ~ "Denmark"
                                   , organization == "Inclusive Friends Association (IFA)" ~ "Federal Capital Territory"
                                   , organization == "Inclusive Skills Development Initiative (ISDI)" ~ "Federal Capital Territory"
                                   , organization == "National Judicial Institute" ~ "Federal Capital Territory"
                                   , organization == "JuriTrust" ~ "Federal Capital Territory"
                                   , organization == "Centre for Socio-Legal Studies" ~ "Federal Capital Territory"
                                   , organization == "National Association of Law Teachers" ~ "Kwara"
                                   , organization == "Joint Admissions and Matriculation Board (JAMB)" ~ "Federal Capital Territory"
                                   , organization == "Pentecostal Fellowship of Nigeria" ~ "Lagos"
                                   , organization == "Frontline Youth Creativity Initiative (FYCI)" ~ "Federal Capital Territory"
                                   , organization == "Coalition of Associations 4 Leadership, Peace, Empowerment and Development" ~ "Kaduna"
                                   , organization == "National Assembly" ~ "Federal Capital Territory"
                                   , organization == "Nigerian Television Authority" ~ "Federal Capital Territory"
                                   , organization == "Federal Radio Cooperation of Nigeria" ~ "Federal Capital Territory"
                                   , organization == "Engage Empower Educate Initiate (EEEI)" ~ "Federal Capital Territory"
                                   , organization == "Basic Rights Watch" ~ "Federal Capital Territory"
                                   , organization == "Clear TV" ~ "Lagos"
                                   , organization == "Daily Nigerian" ~ "Federal Capital Territory"
                                   , organization == "Centre for Media and Society (CEMESO)" ~ "Lagos"
                                   , organization == "Dataphyte" ~ "Federal Capital Territory"
                                   , organization == "Futview Services, Ltd." ~ "Niger"
                                   , organization == "Human Development Initiaties Foundation" ~ "Lagos"
                                   , organization == "International Centre for Development Reporting" ~ "Lagos"
                                   , organization == "Africa Centre for Development Journalism" ~ "Lagos"
                                   , organization == "International Press Centre (IPC)" ~ "Lagos"
                                   , organization == "Kanem Press" ~ "Borno"
                                   , organization == "Law Hub Development & Advocacy Centre" ~ "Federal Capital Territory"
                                   , organization == "National Human Rights Commission" ~ "Federal Capital Territory"
                                   , organization == "National Point" ~ "Rivers"
                                   , organization == "Neptune Prime" ~ "Federal Capital Territory"
                                   , organization == "Nigeria Security & Civil Defence Corps (NSCDC)" ~ "Federal Capital Territory"
                                   , organization == "Nigerian Bar Association" ~ "Federal Capital Territory"
                                   , organization == "Nigerian Union of Journalists, Kano Branch" ~ "Kano"
                                   , organization == "PR Nigeria" ~ "Federal Capital Territory"
                                   , organization == "Solacebase Communications, Ltd." ~ "Kano"
                                   , organization == "Transparency, Accountability, and Performance Excellence (TAPE) Initiative" ~ "Federal Capital Territory"
                                   , organization == "War Against Injustice" ~ "Kano"
                                   , organization == "Xchange Hama Media Ltd" ~ "Bauchi"
                                   , organization == "Public Complaints & Anti Corruption Commission" ~ "Kano"
                                   , organization == "Safer-Media Initiative" ~ "Federal Capital Territory"
                                   , TRUE ~ states_fixed)) 

#write to disk
#writexl::write_xlsx(dat6_non6,
 #                   path = "./data/all_orgs.xlsx")

#more columns can be added to the above object if needed

matches_non <- dat4_non4 |> 
  select(from, to, level_collab, ways_collab, init_collab, desc_collab
         , freq_collab, compl_tactics, strat_collab)

#calculating the level of collaboration
matches_non <- matches_non |> 
  mutate(level_collab1 = case_when(str_detect(level_collab,  "High") ~ "High"
                                   , str_detect(level_collab, "Medium") ~ "Medium"
                                   , str_detect(level_collab, "Low") ~ "Low"))

matches_non <- matches_non |> 
  left_join(dat5_non5, by = join_by("from" == "organization")) |> 
  rename(from_cohort = cohort) |> 
  left_join(dat5_non5, by = join_by("to.x" == "organization")) |> 
  rename(to_cohort = cohort) |> 
  select(1:10, from_cohort, to_cohort) |> 
  left_join(dat5_non5, by = join_by("from" == "organization")) |> 
  rename(from_grantee = grantee) |> 
  left_join(dat5_non5, by = join_by("to.x" == "organization")) |> 
  rename(to_grantee = grantee) |> 
  select(1:12, from_grantee, to_grantee) |> 
  left_join(dat5_non5, by = join_by("from" == "organization")) |> 
  rename(from_desc_org = desc_org) |> 
  left_join(dat5_non5, by = join_by("to.x" == "organization")) |> 
  rename(to_desc_org = desc_org) |> 
  select(1:14, from_desc_org, to_desc_org) |> 
  left_join(dat5_non5, by = join_by("from" == "organization")) |> 
  rename(from_teeth = teeth) |> 
  left_join(dat5_non5, by = join_by("to.x" == "organization")) |> 
  rename(to_teeth = teeth) |> 
  select(1:16, from_teeth, to_teeth) 


#writexl::write_xlsx(matches_non
#                    , path = "./data/SNA Endline Nongrantee Survey_connections_2-7-25.xlsx")

#Sept. 16 2025 - I'm adding made up names to this
#document.

set.seed(5837)

dat6_non6 <- dat6_non6 |> 
  mutate(random_name = randomNames(n = nrow(dat6_non6), 
              name.order = "first.last",
              name.sep = " ",
              sample.with.replacement = FALSE))

dat6_non6 <- dat6_non6 |> 
  relocate(random_name, .before = organization)

#making a list of only the data I need for scrubbing names
dat6_non6_names_only <- dat6_non6 |> 
  select(organization, random_name)

#assigning the random names to matches_non
matches_non <- matches_non |> 
  left_join(dat6_non6_names_only, by =  join_by(from == organization)) |> 
  rename(from_random = random_name)

matches_non <- matches_non |> 
  left_join(dat6_non6_names_only, by = join_by(to.x == organization)) |> 
  rename(to_random = random_name)
  
matches_non <- matches_non |> 
  #select(-from, - to.x) |> 
  relocate(from_random, .before = from) |> 
  relocate(to_random, .before = to.x)

matches_non <- matches_non |> 
  select(-from, - to.x)

matches_non <- matches_non |> 
  rename(from = from_random, to.x = to_random)
        
dat6_non6 <- dat6_non6 |> 
  select(-organization) |> 
  rename(organization = random_name)

#The graph object for non-grantees
g_non <- graph_from_data_frame(matches_non
                           , directed = TRUE
                           , vertices = dat6_non6
)

g_non

## baseline data ----


#read in dataframe
base <- readxl::read_xlsx(here::here("./data/SNA Baseline Grantee Survey_7.27.22_cleaned.xlsx")) |> 
  janitor::clean_names()


base1 <- base |> 
  #str_trim() |> 
  dplyr::rename(from = what_is_the_name_of_your_organization
                , desc_org = which_of_the_following_best_describes_your_organization
                , Abia = in_what_state_is_your_organization_located_if_your_organization_has_multiple_offices_select_all_that_apply_abia
                , work_fed_local = does_your_organization_work_primarily_at_the_federal_level_or_within_states
                , states_work = in_what_states_does_your_organization_work_multiple_responses_allowed_abia
                , pos_title = what_is_your_position_title_at_your_organization
                , pers_work_ac = do_you_personally_directly_work_on_anti_corruption_related_activities_programs_projects_or_efforts_with_other_on_nigeria_grantee_organizations_or_external_non_grantee_organizations
                , others_work_ac = are_there_other_individuals_at_your_organization_that_work_directly_on_anticorruption_projects_with_support_from_the_mac_arthur_foundation
                , two_names_emails = please_provide_up_to_two_names_titles_and_email_addresses_of_individuals_at_your_organization_in_the_space_provided
                , ac_proj_names = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_project_1
                , ac_proj_names2 = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_project_2
                , ac_proj_names3 = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_project_3
                , ac_proj_names4 = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_project_4
                , ac_proj_names5 = what_is_the_name_of_the_anticorruption_projects_that_you_and_your_colleagues_work_on_with_support_from_the_mac_arthur_foundation_project_5
                , scenarios = please_read_the_two_scenarios_below_and_select_the_answer_choice_that_best_applies_to_your_organization_s_work_scenario_a_my_organization_supports_the_development_and_enforcement_of_laws_and_regulations_we_support_implementation_of_systems_for_transparency_and_or_monitor_compliance_with_existing_laws_and_regulations_we_directly_and_or_indirectly_support_use_of_incentives_to_discourage_corruption_and_sanctions_to_punish_it_scenario_b_my_organization_demands_accountability_advocates_for_reforms_engages_citizens_in_anticorruption_issues_such_as_promoting_changes_in_attitudes_and_behaviors_monitors_public_projects_and_legal_compliance_and_or_publishes_reports_on_corruption_and_anticorruption_issues
                , soc_acct_types = social_accountability_refers_to_the_ways_in_which_citizens_work_with_state_institutions_and_the_responsiveness_and_performance_of_those_institutions_social_accountability_strategies_try_to_improve_public_sector_performance_by_bolstering_both_citizen_engagement_and_government_responsiveness_please_select_all_the_types_of_social_accountability_strategies_your_organization_engages_in_as_part_of_your_on_nigeria_anti_corruption_work_my_organization
                , soc_acct_approaches = the_answer_choices_below_describe_different_social_accountability_approaches_these_are_general_descriptions_so_you_may_find_that_no_description_completely_fits_your_organization_s_on_nigeria_anticorruption_work_therefore_please_select_all_the_choices_that_may_apply_to_your_organization_s_anticorruption_work_approach_a_my_organization_s_work_seeks_to_hold_governments_accountable_by_supporting_participation_in_formal_political_processes_like_elections_and_or_by_supporting_citizens_to_organize_and_collectively_advocate_for_government_officials_and_or_agencies_demand_explanations_or_raise_awareness_of_government_performance_approach_b_my_organization_s_work_seeks_to_develop_and_or_support_internal_checks_and_oversight_processes_within_and_between_government_officials_or_institutions_so_that_they_can_hold_one_another_accountable_we_help_state_actors_investigate_and_sanction_irregularities_such_as_corrupt_behavior_or_procedural_violations_approach_c_my_organization_s_work_seeks_to_create_and_or_support_spaces_or_processes_in_which_citizens_participate_in_oversight_processes_or_directly_oversee_the_performance_of_public_or_private_sector_officials_agencies_institutions_and_or_organizations
                , org1 = name_of_organization_1
                , level_collab_org1 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_115
                , ways_collab_org1 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_116
                , init_collab_org1 = who_generally_initiates_the_collaboration_122
                , desc_collab_org1 = what_description_best_applies_to_your_collaboration_with_this_organization_123
                , freq_collab_org1 = how_often_do_you_usually_collaborate_124
                , compl_tactics_org1 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_125
                , strat_collab_org1 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_126
 #               , prov_contact_info_org1 = will_you_provide_contact_information_for_the_collaborating_organization
                , org2 = name_of_organization_2
                , level_collab_org2 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_128
                , ways_collab_org2 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_129
                , init_collab_org2 = who_generally_initiates_the_collaboration_135
                , desc_collab_org2 = what_description_best_applies_to_your_collaboration_with_this_organization_136
                , freq_collab_org2 = how_often_do_you_usually_collaborate_137
                , compl_tactics_org2 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_138
                , strat_collab_org2 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_139
 #               , prov_contact_info_org2 = will_you_provide_contact_information_for_the_collaborating_organization_145
 #               , another_org2 = do_you_have_another_organization_to_add_147
                , org3 = name_of_organization_3
                , level_collab_org3 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_141
                , ways_collab_org3 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_142
                , init_collab_org3 = who_generally_initiates_the_collaboration_148
                , desc_collab_org3 = what_description_best_applies_to_your_collaboration_with_this_organization_149
                , freq_collab_org3 = how_often_do_you_usually_collaborate_150
                , compl_tactics_org3 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_151
                , strat_collab_org3 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_152
 #               , prov_contact_info_org3 = will_you_provide_contact_information_for_the_collaborating_organization_162
 #               , another_org3 = do_you_have_another_organization_to_add_164
                , org4 = name_of_organization_4
                , level_collab_org4 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_154
                , ways_collab_org4 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_155
                , init_collab_org4 = who_generally_initiates_the_collaboration_160
                , desc_collab_org4 = what_description_best_applies_to_your_collaboration_with_this_organization_161
                , freq_collab_org4 = how_often_do_you_usually_collaborate_162
                , compl_tactics_org4 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_163
                , strat_collab_org4 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_164
 #               , prov_contact_info_org4 = will_you_provide_contact_information_for_the_collaborating_organization_179
 #               , another_org4 = do_you_have_another_organization_to_add_181
                , org5 = name_of_organization_5
                , level_collab_org5 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_166
                , ways_collab_org5 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_167
                , init_collab_org5 = who_generally_initiates_the_collaboration_171
                , desc_collab_org5 = what_description_best_applies_to_your_collaboration_with_this_organization_172
                , freq_collab_org5 = how_often_do_you_usually_collaborate_173
                , compl_tactics_org5 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_174
                , strat_collab_org5 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_175
 #               , prov_contact_info_org5 = will_you_provide_contact_information_for_the_collaborating_organization_196
 #               , desc_ac_collab_2024 = please_provide_a_brief_description_of_an_anticorruption_related_collaboration_in_2024_that_you_are_most_proud_of_how_did_you_collaborate_and_what_results_did_you_achieve
                , org6 = name_of_organization_6
                , level_collab_org6 = how_would_you_describe_the_level_of_your_collaboration_on_anti_corruption_efforts_with_this_organization_177
                , ways_collab_org6 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_178
                , init_collab_org6 = who_generally_initiates_the_collaboration_182
                , desc_collab_org6 = what_description_best_applies_to_your_collaboration_with_this_organization_183
                , freq_collab_org6 = how_often_do_you_usually_collaborate_184
                , compl_tactics_org6 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_185
                , strat_collab_org6 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_186
                , another_org_add = do_you_have_another_organization_to_add 
                , desc_ac_work = please_provide_a_brief_description_of_an_anti_corruption_related_collaboration_since_2021_that_you_are_most_proud_of_how_did_you_collaborate_and_what_results_did_you_achieve
                , approach_a_collab = please_provide_a_brief_example_of_approach_a_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , approach_b_collab = please_provide_a_brief_example_of_approach_b_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , fut_collab = is_there_an_organization_that_you_plan_to_collaborate_with_on_your_anti_corruption_efforts_in_the_future_please_provide_the_organization_s_name_and_why_you_wish_to_collaborate_with_them
               
) |> 
  dplyr::select(-c(1, 2, 4, 9, 10, 12, 50
         , 89, 90, 92:94)) #these are all blank columns

base2 <- base1 |> 
  pivot_longer(cols = media_and_journalism:joinbodi
               , names_to = NULL
               , values_to = "cohort"
               , values_drop_na = TRUE) |> 
  unite(col = "state"
        , Abia:fct_49
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) |>  #there's one oddity with the 
                        #state data here. The first column
                        #is for state Abia, but one response
                        #in that column is Kano -- unclear what happened here
  unite(col = "states_work"
        , states_work:fct_88
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% #all of these columns are blank?
  unite(col = "soc_acct_types"
        , soc_acct_types:other_please_specify_109
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "soc_acct_approaches"
        , soc_acct_types:approach_c
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org1"
        , ways_collab_org1:x121
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org2"
        , ways_collab_org2:x134
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org3"
        , ways_collab_org3:x147
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org4"
        , ways_collab_org4:x159
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org5"
        , ways_collab_org5:x170
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE) %>% 
  unite(col = "ways_collab_org6"
        , ways_collab_org6:x181
        , sep = "; "
        , remove = TRUE
        , na.rm = TRUE)


#the questions of partners variables are in .value column
#the to organization number is in the group column
#responses to questions are in the value column
base3 <- base2 |>
  pivot_longer(cols = matches(".*org\\d+$|_org\\d+$")
               , names_to = c(".values", "group")
               , names_pattern = "(.*?)(?:_org)?(\\d+)$"
               , values_drop_na = TRUE) 

# Step 2 is to then pivot it wider and filter out the NAs
# since it kept a row for up to 6 rows for each org
base4 <- base3 |>  
  pivot_wider(names_from = .values
              , values_from = value) |> 
  filter(!is.na(org)) |> 
  rename(to = org) #removes blank rows based on the org column

base4$from <- str_trim(base4$from)

base4$to <- str_trim(base4$to)

#separate one bunch of orgs all put into the same row

base4 <- base4 %>% 
  mutate(to = case_when(from == "Wadata Media and Advocacy Centre (WAMAC)" 
  ~ str_replace(to, "^(1[.)])\\s*", ""), TRUE ~ to)) %>% 
  mutate(to = case_when(from == "Wadata Media and Advocacy Centre (WAMAC)"
                        ~ str_replace_all(to, "\\d[.)]\\s*", "; "), TRUE ~ to)) %>%
  separate_rows(to, sep = ";\\s*") %>% 
  mutate(to = str_trim(to))
  


all_orgs_base <- data.frame(organization = unique(c(base4$from, base4$to))) %>% 
  arrange(organization)

#pivot_longer(cols = module:x10
 #            , names_to = NULL
  #           , values_to = "module"
  #           , values_drop_na = TRUE) |> 
#  tidyr::unite(col = "state", state:x50
 #              , sep = "; "
 #              , remove = TRUE
 #              , na.rm = TRUE)
#base_g <- graph_from_data_frame(base, directed = TRUE)

#plot(base_g)

#base_g

### Create acronyms from existing names ---
# Function to extract or generate acronyms (only for multi-word names)
extract_or_generate_acronym <- function(name) {
  # Try to extract existing acronym from parentheses
  acronym <- stringr::str_extract(name, "\\(([^)]+)\\)")  # Extract text inside parentheses
  
  # If an acronym exists, remove parentheses and return
  if (!is.na(acronym)) {
    return(stringr::str_replace_all(acronym, "[()]", ""))  # Remove parentheses
  }
  
  # Split name into words
  words <- unlist(stringr::str_split(name, "\\s+"))
  
  # If name has only one word, return it as is
  if (length(words) == 1) {
    return(words)  
  }
  
  # Otherwise, generate acronym from capital letters
  generated_acronym <- stringr::str_extract_all(name, "[A-Z]") %>% unlist() %>% paste0(collapse = "")
  
  return(generated_acronym)
}

# Generate acronyms for all nodes
acronyms <- sapply(V(g_non)$name, extract_or_generate_acronym)

# Ensure uniqueness by appending numbers to duplicates

make_unique <- function(acronyms) {
  seen <- list()
  for (i in seq_along(acronyms)) {
    original <- acronyms[i]
    new_acronym <- original
    count <- 1
    
    # While the acronym already exists, append a number
    while (new_acronym %in% seen) {
      count <- count + 1
      new_acronym <- paste0(original, count)
    }
    
    # Store unique acronym
    seen[[new_acronym]] <- TRUE
    acronyms[i] <- new_acronym
  }
  return(acronyms)
}

# Apply uniqueness function
V(g_non)$acronym <- make_unique(acronyms)

# add locations to the dat5_non5 object
#dat5_non5 <- dat5_non5 |> 
#  mutate(states_fixed = case_when(str_detect(state, ";") ~ NA_character_
#                                  , is.na(state) ~ NA_character_
 #                                 , TRUE ~ state))

#dat6_non6 <- dat5_non5 |> 
 # mutate(states_fixed2 = case_when(organization == "African Centre for Leadership, Strategy and Development (Centre LSD)" ~ "Federal Capital Territory"
   #                                , organization == "Arewa Research and Development Project" ~ "Kaduna"
   #                                , organization == "AREWA24" ~ "Lagos"
   #                                , organization == "BudgIT Foundation" ~ "Lagos"
   #                                , organization == "Cable Newspaper Journalism Foundation" ~ "Lagos"
   #                                , organization == "Centre for Democracy and Development" ~ "Federal Capital Territory"
   #                                , organization == "Centre for Information Technology and Development (CITAD)" ~ "Kano"
   #                                , organization == "Behavioural Insights Team (BIT)" ~ "New York"
   #                                , organization == "CLEEN Foundation" ~ "Lagos"
   #                                , organization == "Connected Development (CODE)" ~ "Federal Capital Territory"
   #                                , organization == "HEDA Resource Centre" ~ "Lagos"
   #                                , organization == "HumAngle Media Limited" ~ "Federal Capital Territory"
   #                                , organization == "Joint National Association of Persons with Disabilities (JONAPWD)" ~ "Federal Capital Territory"
   #                                , organization == "Legal Defence and Assistance Project (LEDAP)" ~ "Lagos"
   #                                , organization == "Nigerian Institute of Advanced Legal Studies" ~ "Lagos"
   #                                , organization == "Nigerian Institute for Social and Economic Research" ~ "Federal Capital Territory"
   #                                , organization == "Palace of Priests Assembly" ~ "Federal Capital Territory"
   #                                , organization == "Paradigm Leadership Support Initiative (PLSI)" ~ "Federal Capital Terriroty"
   #                                , organization == "Nigeria Economic Summit Group Policy Innovation Centre (NESG/PIC)" ~ "Federal Capital Territory"
   #                                , organization == "Public and Private Development Centre (PPDC)" ~ "Federal Capital Territory"
  #                                , organization == "Resource Centre for Human Rights and Civic Education (CHRICED)" ~ "Kano"
   #                                , organization == "Partners West Africa Nigeria" ~ "Federal Capital Territory"
   #                                , organization == "Signature Development and Media Foundation" ~ "Federal Capital Territory"
   #                                , organization == "Socio-Economic Rights and Accountability Project" ~ "Lagos"
   #                                , organization == "Step Up for Social Development & Empowerment in Nigeria (Step Up Nigeria)" ~ "Federal Capital Territory"
   #                                , organization == "Tiger Eye Social Foundation" ~ "Accra, Ghana"
   #                                , organization == "UNDP" ~ "Federal Capital Territory"
   #                                , organization == "Women's Rights Advancement and Protection Alternative" ~ "Federal Capital Territory"
   #                                , organization == "Servicom" ~ "Federal Capital Territory"
   #                                , organization == "The International Centre for Investigative Reporting (ICIR)" ~ "Federal Capital Territory"
   #                                , organization == "Judiciary" ~ "Federal Capital Territory"
   #                                , organization == "Center for Fiscal Transparency and Public integrity" ~ "Federal Capital Territory"
#                                , organization == "Federal Road Safety Corps" ~ "Federal Capital Territory"
#                                , organization == "Secondary Education Board" ~ "Federal Capital Territory"
#                                , organization == "International Federation of Women Lawyers (FIDA)" ~ "Federal Capital Territory"
#                                , organization == "Mambayya House, the Aminu Kano Centre for Democratic Research and Training" ~ "Kano"
#                                , organization == "Human rights Radio" ~ "Federal Capital Territory"
#                                , organization == "FCT Council of Imam Initiatives" ~ "Federal Capital Territory"
#                                , organization == "All Farmers Association of Nigeria (AFAN)" ~ "Federal Capital Territory"
#                                , organization == "Equal Access International" ~ "Washington, DC"
#                                , organization == "Center for Dryland Agriculture, BUK" ~ "Kano"
#                                , organization == "Griot Studios" ~ "Federal Capital Territory"
#                                 , organization == "National Youth Service Corp" ~ "Federal Capital Territory"
#                                 , organization == "Code of Conduct Bureau" ~ "Federal Capital Territory"
#                                 , organization == "Public Complaints & Anti Corruption Commission" ~ "?"
#                                 , organization == "Buk Radio and TV" ~ "Kano"
#                                , organization == "Kano State Science Board" ~ "Kano"
#                                 , organization == "Federal Ministry of Finance Budget and National Planning" ~ "Federal Capital Territory"
#                                 , organization == "Premium Times" ~ "Federal Capital Territory"
#                                , organization == "National Orientation Agency (NOA)" ~ "Federal Capital Territory"
#                                , organization == "Alliances for Africa" ~ "Lagos"
#                                 , organization == "Women in Media (WiM)" ~ "?"
#                                 , organization == "The Human Rights Advancement, Development and Advocacy Centre (HURIDAC)" ~ "Ogun"
#                                , organization == "Nigerian Police Force" ~ "Federal Capital Territory"
#                                 , organization == "Bureau of Rural and Community Development, Ekiti" ~ "Ekiti"
#                                 , organization == "Sustainable Development Goals Office, Ekiti" ~ "Ekiti"
#                                , organization == "Leads Nigeria" ~ "Kaduna"
#                                , organization == "Chatham House" ~ "London, UK"
#                                , organization == "Independent National Electoral Commission" ~ "Federal Capital Territory"
#                                 , organization == "The Justice Reform Project" ~ "?"
#                                 , organization == "The Maritime Anti-Corruption Network" ~ "Denmark"
#                                 , organization == "Inclusive Friends Association (IFA)" ~ "Federal Capital Territory"
#                                 , organization == "Inclusive Skills Development Initiative (ISDI)" ~ "Federal Capital Territory"
#                                , organization == "National Judicial Institute" ~ "Federal Capital Territory"
#                                , organization == "JuriTrust" ~ "Federal Capital Territory"
#                                , organization == "Centre for Socio-Legal Studies" ~ "Federal Capital Territory"
#                                 , organization == "National Association of Law Teachers" ~ "Kwara"
#                                , organization == "Joint Admissions and Matriculation Board (JAMB)" ~ "Federal Capital State"
#                                , organization == "Pentecostal Fellowship of Nigeria" ~ "Lagos"
#                                , organization == "Frontline Youth Creativity Initiative (FYCI)" ~ "Federal Capital Territory"
#                                 , organization == "Coalition of Associations 4 Leadership, Peace, Empowerment and Development" ~ "Kaduna"
#                                , organization == "National Assembly" ~ "Federal Capital Territory"
#                                , organization == "Nigerian Television Authority" ~ "Federal Capital Territory"
#                                , organization == "Federal Radio Cooperation of Nigeria" ~ "Federal Capital Territory"
#                                , organization == "Engage Empower Educate Initiate (EEEI)" ~ "Federal Capital Territory"
#                                , organization == "Basic Rights Watch" ~ "Federal Capital Territory"
#                                , organization == "African Women in Media (AWiM)" ~ "Ondo"
#                                , organization == "Centre for Media and Society (CEMESO)" ~ "Lagos"
#                                , organization == "Dataphyte" ~ "Federal Capital Territory"
#                                 , organization == "Futview Services, Ltd." ~ "Niger"
#                                                                     , TRUE ~ states_fixed)) |> 
#select(1, 15, 16, 2:14)
