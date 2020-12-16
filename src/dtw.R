# -- Template by bubble with <3. --

# Script to use dynamic time warping to generate distance metrics between time series
# then: cluster thises distances

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(plyr)
  require(factoextra)
  require(zoo)
  require(sf)
  require(RColorBrewer)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_weekly_cases.csv",
              "/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_hex.shp",
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

cases <- read_csv(.args[1])

msoa_ref <- st_read(.args[2]) %>% 
  st_transform(4326)

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf')
uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

cases <- cases %>% 
  group_by(msoa_code) %>% 
  arrange(date) %>% 
  replace_na(list(cases_rolling_sum = 0))

# Selecting only 1000 MSOAs
n <- 1000
#n <- length(cases$msoa_code %>% unique())

# Generate wide format case trajectories for each MSOA
m_sum_df <- cases %>% 
  select(msoa_code, date, cases_rolling_sum) %>% 
  pivot_wider(names_from = date, values_from = cases_rolling_sum) %>% 
  ungroup() %>% 
  sample_n(n)

# Convert wide format data.frame to matrix
m_sum <- data.matrix(m_sum_df)
m_sum <- m_sum[, 2:ncol(m_sum)]
rownames(m_sum) <- m_sum_df$msoa_code

# Compute distance matrix of trajectories with DTW
m_dist <- parallelDist::parDist(m_sum, method="dtw")

# Visualise distance matrix
p_dist <- fviz_dist(m_dist, gradient = list(low = 'darkblue', mid = 'white', high = 'darkgreen')) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(fill = 'Distance')

# Elbow plot at different cluster values
p_elbow <- fviz_nbclust(as.matrix(m_dist), FUNcluster = kmeans)

# Defining the number of clusers
k <- 25

# Run kmeans on distance matrix with k clusters
res <- kmeans(m_dist, k)

# Define colors for each cluster
pal <- ggutils::qualitative_pal(as.character(1:k))

# Plot dinensionality reduction of communities
p_cluster <- fviz_cluster(res, data = m_dist, labelsize= 0) + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  theme_classic()

# Assign cluster labels
m_sum_df$cluster <- res$cluster

m_sum_df <- m_sum_df %>% 
  select(msoa_code, cluster)

# Assicn clusters to case data
cases_cluster <- cases %>% 
  ungroup() %>% 
  left_join(m_sum_df, by = c('msoa_code')) %>% 
  drop_na(cluster) %>% 
  group_by(date, cluster) 

p <- c(0.05, 0.95, 0.25, 0.75, 0.4, 0.6, 0.5)

p_names <- c('lower_90', 'upper_90', 'lower_50', 'upper_50', 'lower_20', 'upper_20', 'median')

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

cases_cluster <- cases_cluster %>% 
  summarize_at(vars(cases_rolling_sum), p_funs) %>% 
  ungroup()

plot_cis <- function(){
  
  cis <- list(geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, group = as.character(cluster), fill = as.character(cluster)), alpha = 0.3), 
              geom_ribbon(aes(x = date, ymin = lower_50, ymax = upper_50, group = as.character(cluster), fill = as.character(cluster)), alpha = 0.3), 
              geom_ribbon(aes(x = date, ymin = lower_20, ymax = upper_20, group = as.character(cluster), fill = as.character(cluster)), alpha = 0.3),  
              geom_line(aes(x = date, y = median, group = as.character(cluster)), color = '#324B4E'))
  
  return(cis)
  
}

# Plot case trajectories by cluster
p_type <- cases_cluster %>% 
  ggplot() + 
  plot_cis() + 
  scale_fill_manual(values = pal) + 
  facet_wrap(~cluster) + 
  theme_classic() + 
  theme(axis.title.x = element_blank()) + 
  ylab('Cases') + 
  labs(fill = 'Cluster')

# Plot a map of cluster labels
p_map <- msoa_ref %>% 
  left_join(m_sum_df, by = c('msoa_code')) %>% 
  drop_na(cluster) %>% 
  ggplot() + 
  geom_sf(aes(fill = as.character(cluster)), size = 0.1, color = 'black') + 
  scale_fill_manual(values = pal) + 
  england_lims() + 
  basemap(world, uk) + 
  theme_void() + 
  labs(fill = 'Cluster') + 
  guides(color = guide_legend(override.aes = list(size = 3)))

# Plot a map focussed on London
p_map_london <- p_map + 
  ylim(51, 52) + 
  xlim(-1, 1)

# Plot the number of MSOAs per cluster label
p_bar <- m_sum_df %>% 
  group_by(cluster) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  ggplot() + 
  geom_bar(aes(x = cluster, y = n, fill = as.character(cluster)), stat = 'identity') + 
  scale_fill_manual(values = pal) + 
  theme_classic() + 
  labs(fill = 'Cluster') + 
  xlab('Cluster') + 
  ylab('Number of MSOAs')

gutils::ggsave_png_pdf(p_type, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_type.png')
gutils::ggsave_png_pdf(p_map, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_map.png',
                       width=6, height = 7.5)
gutils::ggsave_png_pdf(p_map_london, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_map_london.png',
                       width=6, height = 3.5)
gutils::ggsave_png_pdf(p_bar, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_bar.png')
gutils::ggsave_png_pdf(p_cluster, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_cluster.png')
gutils::ggsave_png_pdf(p_dist, '/Users/hamishgibbs/Documents/Covid-19/msoa_data/output/figs/p_dist.png')

