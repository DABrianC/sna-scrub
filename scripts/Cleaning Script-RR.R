
# Remember to pull when you start 
# and to commit and push when you're done

#pulls in all the packages that are needed
source(here::here("./prep/prep.R"))


#read in dataframe for baseline data
base <- readxl::read_xlsx(here::here("./data/SNA Baseline Grantee Survey_7.27.22_cleaned.xlsx")) |> 
  janitor::clean_names()

#rename variables
#to see where progress stands, run the script
# and then view the base1 object (Environment tab on the right)
# see what variables should be renamed
# if you get to a point that you want to start 
# condensing data you can make a base2 object and
# test out pivot_longer() and unite() using
# the "00 cleaning script.R" as an example.
base1 <- base |> 
  dplyr::rename(from = what_is_the_name_of_your_organization
                , desc_org = which_of_the_following_best_describes_your_organization
                , state = in_what_state_is_your_organization_located_if_your_organization_has_multiple_offices_select_all_that_apply_abia
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
                , ways_collab_org6 = in_what_ways_do_you_collaborate_with_this_organization_on_anti_corruption_select_all_that_apply_167
                , init_collab_org6 = who_generally_initiates_the_collaboration_182
                , desc_collab_org6 = what_description_best_applies_to_your_collaboration_with_this_organization_183
                , freq_collab_org6 = how_often_do_you_usually_collaborate_184
                , compl_tactics_org6 = to_what_extent_did_you_collaborate_with_this_organization_to_develop_and_use_complementary_tactics_complementary_tactics_could_include_litigation_media_coverage_citizen_monitoring_and_other_means_to_help_address_corruption_185
                , strat_collab_org6 = to_what_extent_did_you_collaborate_with_this_organization_at_a_strategic_level_strategic_collaboration_might_focus_on_holding_state_actors_to_account_and_supporting_monitoring_and_enforcement_of_anti_corruption_laws_policies_and_or_regulations_186
                , another_org_add = do_you_have_another_organization_to_add 
                , desc_ac_work = please_provide_a_brief_description_of_an_anti_corruption_related_collaboration_since_2021_that_you_are_most_proud_of_how_did_you_collaborate_and_what_results_did_you_achieve
                , approach_a = please_provide_a_brief_example_of_approach_a_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , approach_b = please_provide_a_brief_example_of_approach_b_as_defined_above_if_any_used_by_your_organization_in_collaboration_with_another_organization
                , fut_collab = is_there_an_organization_that_you_plan_to_collaborate_with_on_your_anti_corruption_efforts_in_the_future_please_provide_the_organization_s_name_and_why_you_wish_to_collaborate_with_them
                #               , fgd_part = thank_you_for_your_participation_we_will_be_selecting_a_few_respondents_to_conduct_a_focus_group_discussion_with_representatives_from_multiple_on_nigeria_grantee_organizations_so_that_we_can_better_understand_and_contextualize_the_responses_you_ve_provided_selected_respondents_would_be_invited_to_participate_in_one_60_to_90_minute_group_discussion_would_you_be_interested_in_participating_in_a_focus_group_discussion
                #               , fgd_contact_name = please_provide_your_full_name_and_email_address_where_we_may_contact_you_to_participate_in_a_focus_group_discussion
                #               , fgd_contact_email = x205
                #               , eval_stakehlds = as_part_of_the_final_evaluation_of_on_nigeria_in_2025_we_hope_to_hear_from_accountability_ecosystem_actors_throughout_nigeria_thinking_about_your_work_who_are_the_non_grantee_stakeholders_that_are_most_important_for_us_to_speak_with_during_the_final_evaluation_separate_from_data_collection_related_to_this_social_network_analysis_please_list_up_to_three_individuals_these_can_be_the_same_as_or_different_from_the_individuals_you_listed_in_the_table_in_section_c_for_each_provide_their_organization_title_and_contact_information
                #               , contact_info_org1 = x129
                #               , contact_info_org2 = x146
                #               , contact_info_org3 = x163
                #               , contact_info_org4 = x180
                #               , contact_info_org5 = x197)
  )
#base_g <- graph_from_data_frame(base, directed = TRUE)

#plot(base_g)

#base_g
