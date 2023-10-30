# Title: Get on Taylor's Radar...
# Author: Greg Chism
# Date: 10/17/2022
# Description: Describing the album profiles of Taylor Swift's albums

# Load required packages
if(!require(pacman))
  install.packages("pacman")

pacman::p_load(tidyverse,
               ggraph,
               gridExtra,
               igraph,
               showtext,
               wesanderson)

set.seed(123)

# Read in data
patient_riskx_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

rawRisk <- patient_riskx_profiles |> 
  janitor::clean_names() |> 
  pivot_longer(cols = contains("age_group"), names_to = "ageGroup", values_to = "ageCount") |>
  pivot_longer(cols = contains("sex"), names_to = "sex", values_to = "sexCount") |> 
  pivot_longer(cols = contains("predicted_risk"), names_to = "predictedRisk", values_to = "riskProb") |>
    mutate(ageGroup = str_replace_all(ageGroup, "age_group_", ""),
           sex = str_replace_all(sex, "sex_", ""),
           predictedRisk = str_replace_all(predictedRisk, "predicted_risk_of_", ""),
           Cardiovascular_Disorders = pmax(
          angina_events_in_prior_year,
          atrial_fibrillation_incident_in_prior_year,
          heart_failure_in_prior_year,
          coronary_artery_disease_cad_in_prior_year,
          deep_vein_thrombosis_dvt_in_prior_year,
          heart_valve_disorder_in_prior_year,
          hypertension_in_prior_year,
          peripheral_vascular_disease_in_prior_year,
          hemorrhagic_stroke_in_an_inpatient_setting_in_prior_year,
          non_hemorrhagic_stroke_in_an_inpatient_setting_in_prior_year,
          0
        ),
        Respiratory_Disorders = pmax(
        occurrence_of_asthma_in_prior_year,
        chronic_obstructive_pulmonary_disease_copd_in_prior_year,
        dyspnea_in_prior_year,
        acute_respiratory_failure_in_prior_year,
        pneumonia_in_prior_year,
        sleep_apnea_in_prior_year,
        0
      ),
      Mental_Health_Disorders = pmax(
        occurrence_of_alcoholism_in_prior_year,
        occurrence_of_anxiety_in_prior_year,
        major_depressive_disorder_with_no_occurrence_of_certain_psychiatric_disorder_in_prior_year,
        psychotic_disorder_in_prior_year,
        0
      ),
      Digestive_System_Disorders = pmax(
        gastroesophageal_reflux_disease_in_prior_year,
        acute_gastrointestinal_gi_bleeding_in_prior_year,
        inflammatory_bowel_disease_in_prior_year,
        chronic_hepatitis_in_prior_year,
        0
      ),
      Endocrine_Nutritional_Metabolic_Diseases = pmax(
        type_1_diabetes_and_no_prior_specific_non_t1dm_diabetes_in_prior_year,
        type_2_diabetes_mellitus_dm_with_no_type_1_or_secondary_dm_in_prior_year,
        hyperlipidemia_in_prior_year,
        hypothyroidism_in_prior_year,
        obesity_in_prior_year,
        0
      ),
      Musculoskeletal_Disorders = pmax(
        osteoarthritis_in_prior_year,
        low_back_pain_in_prior_year,
        osteoporosis_in_prior_year,
        rheumatoid_arthritis_in_prior_year,
        0
      ),
      Neurological_Disorders = pmax(
        antiepileptics_in_prior_year,
        occurrence_of_neuropathy_in_prior_year,
        seizure_in_prior_year,
        0
      ),
      Urinary_Disorders = pmax(
        acute_kidney_injury_aki_in_prior_year,
        chronic_kidney_disease_or_end_stage_renal_disease_in_prior_year,
        urinary_tract_infectious_disease_in_prior_year,
        0
      ),
      Others = pmax(
        anemia_in_prior_year,
        any_cancer_excl_prostate_cancer_and_benign_cancer_in_prior_year,
        edema_in_prior_year,
        skin_ulcer_in_prior_year,
        sepsis_in_prior_year,
        0
      )
    ) |>
  pivot_longer(cols = 72:80, names_to = "reasonVisit", values_to = "visitTrue") |> 
  select(1, 66:73) |> 
  distinct() |> 
  filter(visitTrue == 1 & ageCount == 1 & sexCount == 1) |> 
  mutate(devPhase = case_when(
    ageGroup == "0_4"   ~ "Infancy and Toddlerhood",
    ageGroup == "5_9"   ~ "Early Childhood",
    ageGroup %in% c("10_14") ~ "Middle Childhood",
    ageGroup %in% c("15_19", "20_24") ~ "Adolescence",
    ageGroup %in% c("25_29", "30_34", "35_39") ~ "Early Adulthood",
    ageGroup %in% c("40_44", "45_49", "50_54") ~ "Middle Adulthood",
    ageGroup %in% c("55_59", "60_64", "65_69") ~ "Late Adulthood",
    TRUE ~ "Late Adulthood"
  ),
  riskProbTrue = ifelse(riskProb > 0.01, 1, 0),
  sex = ifelse(sex == "female", "Female", "Male")) 

  edges <- rawRisk |>
    select(reasonVisit, predictedRisk, riskProbTrue, riskProb, devPhase, sex) |>
    filter(riskProbTrue > 0) |>
      group_by(reasonVisit, predictedRisk, devPhase, sex) |>
      summarise(n = sum(riskProbTrue), .groups = "drop",
                riskProb = mean(riskProb)) |>
    rename(from = reasonVisit, to = predictedRisk) 
  
  edges_enhanced <- edges %>%
    mutate(from = paste0(from, "_", devPhase, "_", sex),
           to = paste0(to, "_", devPhase, "_", sex)) 

  
# Load in font

  font_add_google("Montserrat", family = "Montserrat")
  font_add_google("Raleway", family = "Raleway")
    
  # Calculate global min and max riskProb values
  global_min <- 0.01
  global_max <- 0.11
  
  # Calculate the breaks based on the 6 bins requirement
  break_points <- seq(global_min, global_max, length.out = 6)
  
  # Making graphs and plotting function
  plot_network <- function(data) {
    # Create a unique identifier for edges
    data$from_unique <- paste(data$from, data$devPhase, data$sex, sep = ".")
    data$to_unique <- paste(data$to, data$devPhase, data$sex, sep = ".")
    
    # Create the graph object from the edge data
    graph <- graph_from_data_frame(data, directed = TRUE)
    
    # Compute the average riskProb for each 'from' vertex
    from_risk <- aggregate(riskProb ~ from, data=data, FUN=mean)
    
    # Compute the average riskProb for each 'to' vertex
    to_risk <- aggregate(riskProb ~ to, data=data, FUN=mean)
    
    # Combine the two and deduplicate
    all_risk <- rbind(from_risk, setNames(to_risk, names(from_risk)))
    all_risk <- aggregate(riskProb ~ from, data=all_risk, FUN=mean)
    
    # Get the layout coordinates
    layout_coords <- data.frame(name = V(graph)$name, layout = layout_nicely(graph))
    
    # Rename columns to match graph naming
    names(all_risk) <- c("name", "riskProb")
    
    # Assign riskProb as a vertex attribute to the graph
    V(graph)$riskProb <- all_risk$riskProb[match(V(graph)$name, all_risk$name)]
    
    # Palette for networks
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    
    # Determine the node with the highest riskProb
    highest_risk_node <- which.max(V(graph)$riskProb)
    
    # Extract the name of the node and its risk probability
    highest_risk_name <- V(graph)$name[highest_risk_node]
    highest_risk_prob <- V(graph)$riskProb[highest_risk_node]
    
    # Extract the riskProb values from the graph's vertex attributes
    graph_riskProb <- V(graph)$riskProb
    
    # Extract just the category name (before the first underscore)
    category <- strsplit(highest_risk_name, "_")[[1]][1]
    
    # Create the subtitle string
    subtitle_str <- sprintf("Highest risk: %s (%.1f%%)", tools::toTitleCase(tolower(category)), highest_risk_prob * 100)
    
    # Plot the network graph for the specific group
    #break_points <- seq(from = min(V(graph)$riskProb), 
                        #to = max(V(graph)$riskProb), 
                        #by = (max(V(graph)$riskProb) - min(V(graph)$riskProb)) / 5)
    
    # Sub plots
    ggraph(graph, layout = 'graphopt') + 
      geom_edge_link(aes(alpha = after_stat(index)),
                     show.legend = FALSE) + 
      geom_node_point(aes(size = riskProb, color = riskProb)) + 
      scale_color_gradientn(name = "Risk probability", colours = pal,
                            limits = c(global_min, global_max),
                            breaks = break_points, 
                            labels = scales::percent(break_points)) + 
      scale_size_continuous(name = "Risk probability",
                            limits = c(global_min, global_max),
                            breaks = break_points,
                            labels = scales::percent(break_points)) +
      scale_edge_alpha('Edge direction', guide = 'edge_direction') +
      guides(color = guide_legend(nrow = 1, keywidth = 1, keyheight = 0.5,
                                  title.position = "top",
                                  label.position = "bottom"),
             size = guide_legend(nrow = 1, keywidth = 1, keyheight = 0.5,
                                 title.position = "top",
                                 label.position = "bottom"),
             alpha = guide_legend(override.aes = list(linetype = 0))) +  # Use guides to combine legends
      theme_graph(base_family = "Raleway", base_size = 11) +
      labs(title = paste(unique(data$sex), unique(data$devPhase)), 
           subtitle = subtitle_str) +
      theme(legend.position = "left",
            legend.direction = "horizontal",
            legend.title = element_text(size = 30),
            legend.text = element_text(size = 25),
            legend.key = element_blank(),
            legend.spacing.x = grid::unit(0.25, "cm"),
            legend.spacing.y = grid::unit(0, "cm"),
            plot.title = element_text(face = "plain", size = 30),
            plot.subtitle = element_text(size = 25),
            plot.background = element_rect(fill = "gray95"),
            plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"))
  }
  
  # Get unique combinations of sex and devPhase
  unique_combinations <- edges_enhanced %>% 
    distinct(sex, devPhase)
  
  sex <- unique_combinations$sex
  devPhase <- unique_combinations$devPhase
  
  plots <- map2(sex, devPhase, function(s, d) {
    subset_data <- edges_enhanced %>% filter(sex == s, devPhase == d)
    plot_network(subset_data)
  })
  p <- ggplot() +
    geom_text(aes(x = 0.415, y = 0.975, label = "In Network Care"), size = 15,
              family = "Montserrat", fontface = "bold") +
    geom_text(aes(x = 0, y = 0.775, label = "Sex and life stage predict\nrisk of ailment in individuals"), size = 12,
              family = "Raleway",
              hjust = 0,
              lineheight = 0.5) +
    theme_void() +
    lims(x = c(0, 1),
         y = c(0, 1))
  
  #plotlist = plots,
  # Plot all in a grid
  
  # Extract the legend from the plot
  legend_p <- get_legend(plots[[9]])
  
  # Combine the blank plot with the legend
  combined_plot <- plot_grid(legend_p, p, ncol=2, rel_widths = c(4, 1))
  
  
  # Overlay the legend onto the main plot as an inset
  p_inset_legend <- ggdraw(p) +
    draw_plot(legend_p, x = 0.475, y = 0.33, width = 0.15, height = 0.15)
  
  combined_plot <- ggarrange(p_inset_legend, plots[[9]], plots[[11]], plots[[10]], plots[[4]], plots[[3]],
            plots[[6]], plots[[5]], plots[[8]], plots[[7]], plots[[2]], plots[[1]],
            common.legend = TRUE, ncol = 4, nrow = 3,
            legend = "none") + # Adjust 'ncol' for number of columns 
    theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.background = element_rect(fill = "gray95"))

  finalPlot <- annotate_figure(combined_plot, 
                  bottom = text_grob("Data: Jenna Reps | Viz: Greg Chism", 
                                      hjust = 1.15, x = 1, size = 25,
                                     family = "Raleway")) +
    theme(plot.background = element_rect(fill = "gray95",
                                         color = "gray95"))
  
  ggsave(plot = finalPlot, here("2023", "2023-10-24", "20231024.png"), height = 9, width = 13, units = "in", dpi = 200)
  