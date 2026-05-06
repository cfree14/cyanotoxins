
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"

# Thresholds
thresh <- readxl::read_excel(file.path(outdir, "thresholds.xlsx")) %>% 
  mutate(age_group=factor(age_group, levels=c("Adults", "Children", "All")))

# Read species
spp_key <- readxl::read_excel(file.path(outdir, "species_key.xlsx"))
# freeR::check_names(spp_key$species)

# Merge data
################################################################################

# Merge data
data_orig <- purrr::map_df(1:6, function(x){
  df <- readxl::read_excel(file.path(indir, "MC_DigitizedData.xlsx"), sheet=x, 
                            na=c("No data", "No Data"))
})

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(comm_name_orig=comm_name,
         species_orig=species,
         tissue_orig=tissue) %>% 
  # Format common name
  mutate(comm_name_orig=stringr::str_to_sentence(comm_name_orig)) %>% 
  # Add species 
  left_join(spp_key) %>% 
  # Format tissue
  mutate(tissue_orig=stringr::str_to_sentence(tissue_orig),
         tissue=recode(tissue_orig,
                       "Liver and mid gland"="Liver",)) %>% 
  # Format year
  mutate(year=ifelse(is.na(year), lubridate::year(date), year)) %>% 
  # Convert to ug/kg
  mutate(mc_ug_kg=ifelse(is.na(mc_ug_kg) & !is.na(mc_ug_g), mc_ug_g*1000, mc_ug_kg),
         mc_ug_kg=ifelse(is.na(mc_ug_kg) & !is.na(mc_ng_g), mc_ng_g, mc_ug_kg)) %>% 
  # Filter
  filter(mc_ug_kg>0) %>% 
  # Arrange
  select(reference, source, system, location, site, 
         year, date, 
         taxa, comm_name, species,
         comm_name_orig, species_orig,
         tissue, tissue_orig,
         analysis_method, weight_type,
         salinity_lo, salinity_hi, salinity_range,
         mc_ng_g, mc_ng_g_se, mc_ng_g_min, mc_ng_g_q1, mc_ng_g_q3, mc_ng_g_max, 
         mc_ug_g, 
         mc_ug_kg, mc_ug_kg_lo, mc_ug_kg_hi, 
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$system)
table(data$tissue)
table(data$weight_type)

# Species key
spp_key1 <- data %>% 
  count(comm_name, species)

# Locaiton key
loc_key <- data %>% 
  count(system, location, site)




# Plot data
################################################################################

# Stats for plotting
stats <- data %>% 
  group_by(taxa, comm_name) %>% 
  summarize(n=n(),
            mc_ug_kg_max=max(mc_ug_kg)) %>% 
  ungroup() %>% 
  arrange(desc(mc_ug_kg_max)) %>% 
  mutate(label=round(mc_ug_kg_max,1),
         label=formatC(mc_ug_kg_max, format = "f", digits = 1, big.mark = ","))

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(y=factor(comm_name, levels=stats$comm_name), 
                      x=mc_ug_kg,
                      shape=tissue)) +
  facet_grid(taxa~., space="free_y", scales="free_y") +
  # Threshold
  geom_vline(data=thresh, mapping=aes(xintercept=thresh_ug_kg, 
                                      linetype=age_group,
                                      color=jurisdiction,), inherit.aes = F) +
  # Data
  geom_point() +
  # Print maximum
  geom_text(data=stats, mapping=aes(y=factor(comm_name, levels=stats$comm_name), 
                                    x=mc_ug_kg_max, 
                                    label=label), 
            hjust=-0.4, color="grey30", size=2.2, inherit.aes = F) +
  # Labels
  labs(x="Microcystin toxicity (μg/kg)", y="") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1, 1, 10, 100, 1000, 10000),
                     labels=c("0.1", "1", "10", "100", "1,000", "10,000"),
                     lim=c(NA, 50000)) +
  # Legend
  scale_linetype_manual(name="Age group", values=c("solid", "dashed", "dotted")) +
  scale_color_discrete(name="Jurisdiction") +
  scale_shape_discrete(name="Tissue type") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(outdir, "mc_concentrations_vs_action_levels.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")

