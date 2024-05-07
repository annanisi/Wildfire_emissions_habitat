
# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(viridis)
library(rcompanion)
library(RColorBrewer)
library(gridExtra)
library(chisq.posthoc.test)
library(agricolae)
library(patchwork)
library(mgcv)

# Load data ---------------------------------------------------------------

# Set color palette for land use categories
wui.pal <- hcl.colors(n = 4, palette = 'ag_GrnYl')


# Load point data for analyses of how puma metrics and HCF vary across land use types
pts.puma <- read_csv('data/points_sample_for_puma_metrics.csv')
pts.hcf <- read_csv('data/points_sample_for_hcf.csv')

# Load building point data for burn analysis
bpts <- read.csv('data/building_locations.csv')


# Load block-group-level hcf, puma data for continuous visualization
bg <- read_csv('data/hcf_block_group.csv')


# Set factor levels for plotting ------------------------------------------

pts.puma <- pts.puma %>%
  mutate(land_use_type = fct_relevel(land_use_type, c('Sparse','Intermix','Interface','Urban')))
pts.hcf <- pts.hcf %>%
  mutate(land_use_type = fct_relevel(land_use_type, c('Sparse','Intermix','Interface','Urban')))


# Functions ---------------------------------------------------------------

# Function to get labels for post-hoc tests
labels_fxn <- function(aov_object){ # Function to get post-hoc comparison letter labels for land use type anovas
  output <- HSD.test(aov_object, 'land_use_type') %>%
    .$groups %>%
    mutate(land_use_type = row.names(.),
           land_use_type = fct_relevel(land_use_type, c('Sparse','Intermix','Interface','Urban'))) %>%
    arrange(land_use_type)
  output
}

plot_fxn <- function(data,  x_var_name, y_var_name, label_adj, xlab_title, ylab_title, color_name, aov_object){
  out <- list()
  x_var_name <- enquo(x_var_name)
  y_var_name <- enquo(y_var_name)
  labels_df <- labels_fxn(aov_object)

  temp.df <- data %>%  # data.frame to get positions of text labels
    group_by(!!x_var_name) %>%
    dplyr::summarize(mean = mean(!!y_var_name, na.rm =T),
                     sd = sd(!!y_var_name, na.rm =T),
                     n = n(),
                     max = max(!!y_var_name, na.rm =T)) %>%
    mutate(se = sd/sqrt(n),
           lower = mean - 1.96*se,
           upper = mean + 1.96*se) %>%
    left_join(labels_df[,2:ncol(labels_df)]) # without the first column that has the same name as y_var_name
  out <- ggplot(data = data, mapping = aes(x = !!x_var_name, y = !!y_var_name))+ # Plot
    geom_boxplot(fill = color_name) +
    geom_text(data = temp.df, aes(y = max + label_adj, label= groups),
              position = position_dodge(width=0.9),
              size = 4) +
    xlab(xlab_title) +
    ylab(ylab_title) +
    theme_classic()
  out
}


############################################################################
#          Categorical analysis - metrics against land use type            #
############################################################################

# Puma habitat quality plots ----------------------------------------------
dep_aov <- aov(dep_risk ~ land_use_type, data = pts.puma)
summary(dep_aov) # p < 0.001
TukeyHSD(dep_aov)

(g_rk_wui <- plot_fxn(data = pts.puma,
                                x_var_name = land_use_type,
                                y_var_name =  dep_risk,
                                label_adj = 1.5,
                                xlab_title =  "Land use type", ylab_title =  "Human-puma conflict risk",
                      color_name = wui.pal,  aov_object = dep_aov))


# Selection:
sel_aov <- aov(selection ~ land_use_type, data = pts.puma)
summary(sel_aov) # p < 0.001
TukeyHSD(sel_aov)
(g_sel_wui <- plot_fxn(data = pts.puma,
                                 x_var_name = land_use_type,
                                 y_var_name =  selection, label_adj =  .5,
                                 xlab_title = "Land use type",
                                 ylab_title =  "Puma habitat selection", color_name = wui.pal,
                                 aov_object = sel_aov))

# Lambda:
lambda_aov <- aov(lambda ~ land_use_type, data = pts.puma)
summary(lambda_aov) # p < 0.001
TukeyHSD(lambda_aov)

(g_lambda_wui <- plot_fxn(data = pts.puma,
                                    x_var_name = land_use_type,
                                    y_var_name =  lambda, .1,
                                    xlab_title =  "Land use type",
                                    ylab_title = "Mean puma population growth rate", wui.pal, aov_object =lambda_aov))

# HHCF by land use type----------------------------------------------------------

hcf_aov <- aov(AVG_TOTAL_2010 ~ land_use_type, data = pts.hcf)
summary(hcf_aov)
TukeyHSD(hcf_aov)

(g_hcf_wui <- plot_fxn(data = pts.hcf, x_var_name = land_use_type, y_var_name = AVG_TOTAL_2010, label_adj = 10, xlab_title = "Land use type", ylab_title =expression("Mean household C footprint (tCO"[2]*'e)'), color_name = wui.pal,
                     aov_object =hcf_aov))


# HHCF by sector ----------------------------------------------------------

sector_labels <- bind_rows(labels_fxn(aov_object = aov(AVG_FOOD_2010 ~ land_use_type, data = pts.hcf)) %>% mutate(sector_name = 'FOOD'),
                   labels_fxn(aov_object = aov(AVG_GOODS_2010 ~ land_use_type, data = pts.hcf)) %>% mutate(sector_name = 'GOODS'),
                   labels_fxn(aov_object = aov(AVG_HOUSING_2010 ~ land_use_type, data = pts.hcf)) %>% mutate(sector_name = 'HOUSING'),
                   labels_fxn(aov_object = aov(AVG_SERV_2010 ~ land_use_type, data = pts.hcf)) %>% mutate(sector_name = 'SERV'),
                   labels_fxn(aov_object = aov(AVG_TRANS_2010 ~ land_use_type, data = pts.hcf)) %>% mutate(sector_name = 'TRANS')) %>%
  dplyr::select(AVG_FOOD_2010, land_use_type, groups)
sector_labels

sector_names <- data.frame(sector_name = c('FOOD', 'GOODS', 'HOUSING' ,'SERV', 'TRANS'),
                           sector_name2 =c('Food', 'Goods', 'Housing', 'Services', 'Transportation'))

sector_data <- pts.hcf %>%
    dplyr::select(land_use_type, AVG_TRANS_2010:AVG_SERV_2010) %>%
    pivot_longer(cols = AVG_TRANS_2010:AVG_SERV_2010, names_to = 'sector', values_to = 'HHCF') %>%
    mutate(sector_name = substr(sector, 5, nchar(sector) - 5)) %>%
    left_join(sector_names)

sector_sum <- sector_data %>%
  group_by(land_use_type, sector_name2) %>%
  summarize(mean = mean(HHCF, na.rm =T),
            sd = sd(HHCF, na.rm =T),
            n = n(),
            max = max(HHCF, na.rm =T)) %>%
  mutate(se = sd/sqrt(n),
         lower = mean - 1.96*se,
         upper = mean + 1.96*se) %>%
  arrange(sector_name2)

g_sector_wui <- ggplot(sector_data, mapping = aes(x = land_use_type, y = HHCF))+
  geom_boxplot(fill = rep(wui.pal,5)) +
  geom_text(data = sector_sum, aes(y = max + 5, label= sector_labels$groups),
            position = position_dodge(width=0.9),
            size = 4) +
  xlab("Land use type") +
  ylab("Sector contribution to HHCF") +
  facet_wrap(~sector_name2) +
  theme_classic()
g_sector_wui


# Burn analysis -----------------------------------------------------------

bpts <- bpts %>%
  mutate(land_use_type = fct_relevel(land_use_type, c('Sparse','Intermix','Interface','Urban','Other')))

bpts_sum <- bpts %>%
  group_by(land_use_type) %>%
  summarize(N = n(),
            burned = sum(burned)) %>%
  mutate(percent_burned_in_category = burned/N,
         percent_of_burned_houses = 100*(burned/sum(burned))) %>%
  filter(!is.na(land_use_type))
bpts_sum

g.burn <- ggplot(data = bpts_sum %>% filter(land_use_type != 'Other'),
                 mapping = aes(x = land_use_type, y = percent_of_burned_houses,
                               fill = land_use_type)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_fill_manual(values = wui.pal) +
  theme_classic() +
  theme(legend.position = 'none') +
  xlab('Land use type') +
  ylab('Percent burn-threatened buildings\nby land use type')
g.burn

td <- bpts %>%  # Format data for logistic regression - make dummy variables for intermix, interface
  filter(!is.na(land_use_type),
         !is.element(land_use_type, c("Other", "Urban"))) %>% # Exclude - 0 burned homes within these categories
  mutate(Intermix = ifelse(land_use_type == 'Intermix', 1, 0),
         Interface = ifelse(land_use_type == 'Interface', 1, 0),)

br <- glm(burned ~ Intermix + Interface, data = td, family = 'binomial')
sum_br <- summary(br)$coefficients %>%
  as.data.frame() %>%
  mutate(cov_name = rownames(.))
sum_br

#### Odds ratios relative to rural (baseline):
ors <- sum_br %>%
  filter(cov_name != "(Intercept)") %>%
  mutate(or = exp(Estimate),
         l_95 = exp(Estimate - 1.96*`Std. Error`),
         u_95 = exp(Estimate + 1.96*`Std. Error`))
ors
write_csv(ors, file = 'Tables/odds_ratio_burned_buildings.csv')

# Figure 2 ----------------------------------------------------------------

fig.2 <- ggplot() + theme_void() + # blank space for conceptual diagram
  g.burn + theme(axis.text.x = element_blank()) +
  g_hcf_wui + theme(axis.text.x = element_blank()) +
  g_lambda_wui +theme(axis.text.x = element_blank()) +
  g_rk_wui + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  g_sel_wui  + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  plot_layout(nrow =3) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

fig.2

png(filename = 'Figures/figure_2_panels.png', width = 6.5, height = 8, units = 'in', res = 500, type = 'cairo')
fig.2& theme(axis.text.y = element_text(size = 7),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 10),
                 plot.margin = unit(c(3,3,3,3), "pt"))
dev.off()


# SI figure: HCF by sector -----------------------------------------------------------

jpeg(file = 'Figures/Final figures/Nisi Figure S1.jpg', height = 5, width =5, units = 'in', res = 500)
g_sector_wui +
  ylab(expression("Sector contribution to household C footprint (tCO"[2]*'e)')) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5))
dev.off()


############################################################################
#          Continuous analysis - metrics against household density         #
############################################################################

range(bg$hhd, na.rm =T) #1.672947 10898.609311
range(bg$log_hhd, na.rm =T) #0.5145869 9.2963905
log.labels<- c(1, 10, 100, 1000, 10000)
log.breaks <- log(log.labels)

# Set palettes (corresponding to study area maps)

hcf.pal <- hcl.colors(n = 4, palette = 'YlOrR')
lambda.pal <- hcl.colors(n = 10, palette   = 'BurgYl')
dep.pal <- plasma(8)
sel.pal <- hcl.colors(n = 10, palette = 'YlGnBu')

gc.hcf <-
  ggplot(data = bg, mapping = aes(x = log_hhd, y = AVG_TOTAL_2010)) +
  geom_point(alpha = .5, pch = 16, col = hcf.pal[2]) +
  geom_smooth(method = 'gam',formula = y ~ s(x, bs = 'cs'), col = 'black') +
  ylab(expression(atop("Mean household C", "footprint (tCO"[2]*'e)'))) +
  xlab(expression('Household density (households km'^-2*')')) +
  scale_x_continuous(breaks = log.breaks, labels = log.labels) +
  theme_classic()
gc.hcf

gc.lambda <- ggplot(data = bg, mapping = aes(x = log_hhd, y = mean_lambda)) +
  geom_point(alpha = .5, pch = 16, col = lambda.pal[2]) +
  geom_smooth(method = 'gam',formula = y ~ s(x, bs = 'cs', k = 5), col = 'black') +
  ylab(paste0('Mean puma population','\n', 'growth rate')) +
  xlab(expression('Household density (households km'^-2*')')) +
  scale_x_continuous(breaks = log.breaks, labels = log.labels) +
  theme_classic()
gc.lambda

gc.dep_risk <- ggplot(data = bg, mapping = aes(x = log_hhd, y = mean_dep_risk)) +
  geom_point(alpha = .5, pch = 16, col = dep.pal[2]) +
  geom_smooth(method = 'gam',formula = y ~ s(x, bs = 'cs'), col = 'black') +
  ylab(paste0("Mean human-puma",'\n', "conflict risk")) +
  xlab(expression('Household density (households km'^-2*')')) +
  scale_x_continuous(breaks = log.breaks, labels = log.labels) +
  theme_classic()
gc.dep_risk

gc.selection <-
  ggplot(data = bg, mapping = aes(x = log_hhd, y = mean_selection)) +
  geom_point(alpha = .5, pch = 16, col = sel.pal[2]) +
  geom_smooth(method = 'gam',formula = y ~ s(x, bs = 'cs', k = 4), col = 'black') +
  ylab(expression("Mean puma habitat selection")) +
  xlab(expression('Household density (households km'^-2*')')) +
  scale_x_continuous(breaks = log.breaks, labels = log.labels) +
  theme_classic()
gc.selection


# Fit GAM --------------------------------------------------------------

## The GAM for burn perimeter takes much longer to fit so it is preferable to fit outside of the plotting call (using geom_smooth) - so I fit the model and then use predict to plot.

g.burn.gam <- gam(formula = burned ~ s(log_hhd, k = 5), family = 'binomial', data = bpts)
summary(g.burn.gam)
range(bpts$log_hhd, na.rm =T)
pred.df <- data.frame(log_hhd = seq(min(bpts$log_hhd, na.rm =T),
                                    max(bpts$log_hhd, na.rm =T), by = .01))

gam.pred <- predict(g.burn.gam, newdata = pred.df, se.fit =T, type = 'response')
pred.df.gam <- pred.df %>%
  mutate(pred = gam.pred$fit,
         se = gam.pred$se.fit,
         upper = pred + 1.96*se,
         lower = pred - 1.96*se)

ggplot(data = pred.df.gam, mapping = aes(x = log_hhd, y = pred)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = 'gray60', alpha = .4) +
  theme_classic()

# Make burn plot:
gc.burn <- ggplot() +
  geom_rug(data = bpts %>% filter(burned == 0), mapping = aes(x = log_hhd), sides = 'b', alpha = .5, col = "#DC267F") +
  geom_rug(data = bpts %>% filter(burned == 1), mapping = aes(x = log_hhd), sides = 't', alpha = .5, col = "#DC267F") +
  geom_ribbon(data  = pred.df.gam,mapping = aes(ymin = lower, ymax = upper, x = log_hhd), fill = 'gray60', alpha = .4) +
  geom_line(data = pred.df.gam, mapping = aes(x = log_hhd, y = pred)) +
  ylab(expression("P(within burn perimeter)")) +
  xlab(expression('Household density (households km'^-2*')')) +
  scale_x_continuous(breaks = log.breaks, labels = log.labels) +# tod o
  theme_classic()
gc.burn


# Save Figure 3 ---------------------------------------------------------------

fig.3 <- gc.burn + gc.hcf + gc.lambda + gc.dep_risk + gc.selection +
  plot_layout(ncol = 1, axis_titles = 'collect_x') +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')


jpeg(filename = 'Figures/Final figures/Nisi Figure 3.jpeg', height = 8, width = 4, units = 'in', res = 500)
fig.3 & theme(axis.text = element_text(size = 7),
              axis.title = element_text(size = 8),
              plot.margin = unit(c(1,1,1,1), "pt"),
              axis.title.y = element_text(vjust = 0.5))

dev.off()

