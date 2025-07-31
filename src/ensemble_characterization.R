library(Rtranscpp)
library(tidyverse)
library(cluster)

library(caret)
library(doParallel)

################################################################################
###### INPUT ######
################################################################################

path_dw5  = '/path/to/xml/input/data/w500/original/'
path_rw5  = '/path/to/xml/input/data/w500/reduced/'
path_dw10 = '/path/to/xml/input/data/w1000/original/'
path_rw10 = '/path/to/xml/input/data/w1000/reduced/'
path_dw3  = '/path/to/xml/input/data/w300/original/'
path_rw3  = '/path/to/xml/input/data/w300/reduced/'
path_dw8  = '/path/to/xml/input/data/w800/original/'
path_rw8  = '/path/to/xml/input/data/w800/original/'

pattern_name_dw5  = 'pone_wd_sc'
pattern_name_rw5  = 'pone_rw5_sc'
pattern_name_dw10 = 'pone_sc'
pattern_name_rw10 = 'pone_rd_sc'

pattern_name_w3   = 'pone_w3_sc'
pattern_name_rw3  = 'pone_rw3_sc'
pattern_name_w8   = 'pone_w8_sc'
pattern_name_rw8  = 'pone_rw8_sc'


results_path = '/path/to/save/results/'
setwd(results_path)


LINE_WD <- 3
RMS_THR <- 12.0
corr_thr <- 0.75

order_list_01 <- c( "hb1_coef", "bcd_coef", "cad_coef", "dst_coef",
                   "kni_coef", "tll_coef", "gt_coef", "Kr_coef",
                   "hb_coef",  "cad_Threshold",
                   "hb_Kmax", "bcd_Kmax", "cad_Kmax",  "dst_Kmax",
                   "kni_Kmax", "tll_Kmax", "gt_Kmax", "Kr_Kmax","Kcoop",
                   "hb_Lambda", "bcd_Lambda",  "cad_Lambda", "dst_Lambda",
                   "kni_Lambda", "tll_Lambda", "gt_Lambda", "Kr_Lambda",
                   "Background", "CoactDist", "BcdCoactEff", "CadCoactEff",
                   "eve.Theta",
                   "ScaleFactor", 'RMS')

param_idx <- c("Background" = 5, "CoactDist" = 8,
              "bcd_Kmax" = 13, "bcd_Lambda" = 15, "bcd_coef" = 16,
              "cad_Kmax" = 18, "cad_Lambda" = 20, "cad_coef" = 21,
              "dst_Kmax" = 23, "dst_Lambda" = 25, "dst_coef" = 26,
              "hb_Kmax" = 28, "hb_Lambda" = 30, "hb_coef" = 31, "hb1_coef" = 32,
              "Kr_Kmax" = 34, "Kr_Lambda" = 36, "Kr_coef" = 37,
              "kni_Kmax" = 39, "kni_Lambda" = 41, "kni_coef" = 42,
              "gt_Kmax" = 44, "gt_Lambda" = 46, "gt_coef" = 47,
              "tll_Kmax" = 49, "tll_Lambda" = 51, "tll_coef" = 52,
              "eve Theta" = 56, "Kcoop" = 65, "BcdCoactEff" = 66, "CadCoactEff" = 67)

tf_idx <- c('bcd'=8, 'cad'=9, 'dst'=10, 'hb'=5, 'hb1'=7,
            'Kr'=4, 'kni'=3, 'gt'=2, 'tll'=1, ' '=6)

tf_color_00 <- c('bcd'='#e32944', 'cad'='#e3e100', 'dst'='#9e6e90', 'hb1'='#79d1e6', 'hb'='#79d1e6',
 'Kr'='#7991e6', 'kni'='#FFA07A', 'gt'='#56e190', 'tll'='#568390')

tf_color_01 <- c('bcd'='#79d1e6', 'cad'='#79d1e6', 'dst'='#79d1e6', 'hb1'='#79d1e6', 'hb'='#e32944',
                 'Kr'='#e32944', 'kni'='#e32944', 'gt'='#e32944', 'tll'='#e32944')

tf_color_02 <- c('bcd'='#0000FF', 'cad'='#0000FF', 'dst'='#0000FF', 'hb1'='#0000FF', 'hb'='#FF0000',
                 'Kr'='#FF0000', 'kni'='#FF0000', 'gt'='#FF0000', 'tll'='#FF0000')

tf_color_03 <- c('bcd'='#009E73', 'cad'='#009E73', 'dst'='#009E73', 'hb1'='#009E73', 'hb'='#E32944',
                 'Kr'='#E32944', 'kni'='#E32944', 'gt'='#E32944', 'tll'='#E32944')

tf_color_04 <- c('bcd'='#ADF32E', 'cad'='#56B4E9', 'dst'='#009E73', 'hb1'='#0072B2', 'hb'='#0072B2',
                 'Kr'='#E69F00', 'kni'='#D55E00', 'gt'='#E32944', 'tll'='#BB3BA8')

tf_color_05 <- c('cad'='#ADF32E', 'gt'='#56B4E9', 'Kr'='#0072B2', 'hb1'='#009E73', 'hb'='#009E73',
                 'dst'='#E69F00', 'kni'='#D55E00', 'bcd'='#E32944', 'tll'='#BB3BA8')

tf_color_06 <- c('bcd'='#8DE9FF', 'cad'='#8DE9FF', 'dst'='#8DE9FF', 'hb1'='#8DE9FF', 'hb'='#E32944',
                 'Kr'='#E32944', 'kni'='#E32944', 'gt'='#E32944', 'tll'='#E32944')



tf_symbol_00 <- c('bcd'=19, 'cad'=19, 'dst'=19, 'hb1'=19,
               'gt'=18, 'hb'=18, 'Kr'=18, 'tll'=18, 'kni'=18)

tf_symbol_01 <- c('bcd'=18, 'cad'=18, 'dst'=18, 'hb1'=18,
                'gt'=19, 'hb'=19, 'Kr'=19, 'tll'=19, 'kni'=19)

tf_symbol_02 <- c('bcd'=21, 'cad'=25, 'dst'=23, 'hb1'=24,
                'gt'=25, 'hb'=24, 'Kr'=21, 'tll'=22, 'kni'=23)

tf_symbol_03 <- c('bcd'=1, 'cad'=6, 'dst'=5, 'hb1'=2,
                'gt'=6, 'hb'=2, 'Kr'=1, 'tll'=0, 'kni'=5)


enhancers_coords_00 <- data.frame(
  start = c(-3800, -1600, 4600, 6600),
  end = c(-3300, -900, 5400, 8200),
  label = c("MSE3+7", "S2E", "stripe 4+6", "stripe 1+5")
)

mask_coords <- data.frame(
  start = c(-4607,-3161,  -864, 2078,  5231, 5925,  7988),
  end = c(-4305, -2395,  -472,  4222,  5401,  6104, 8419)
)

enhancers_coords_01 <- list()
enhancers_coords_01[['S2E']] <- data.frame(label = 'S2E', start = -1600, end = -900)
enhancers_coords_01[['MSE3_7']] <- data.frame(label = 'MSE3+7', start = -3800, end = -3300)
enhancers_coords_01[['stripe_4_6']] <- data.frame(label = 'stripe 4+6', start = 4600, end = 5400)
enhancers_coords_01[['stripe_1_5']] <- data.frame(label = 'stripe 1+5', start = 6600, end = 8200)

enhancer_dict <- c( '2' = 'S2E', '3' = 'MSE3_7', '4' = 'stripe_4_6', '5' = 'stripe_1_5',
                   '6' = 'stripe_4_6', '7' = 'MSE3_7' )


stripe_pct_exp <- c (
  "35.5" = '1-2', "36.5" = '1-2', "37.5" = '1-2', "38.5" = '2', "39.5" = '2', "40.5" = '2', "41.5" = '2', "42.5" = '2',
  "43.5" = '2', "44.5" = '2-3', "45.5" = '2-3', "46.5" = '2-3', "47.5" = '3', "48.5" = '3', "49.5" = '3', "50.5" = '3',
  "51.5" = '3', "52.5" = '3-4', "53.5" = '3-4', "54.5" = '4', "55.5" = '4', "56.5" = '4', "57.5" = '4', "58.5" = '4',
  "59.5" = '4', "60.5" = '4-5', "61.5" = '4-5', "62.5" = '5', "63.5" = '5', "64.5" = '5', "65.5" = '5', "66.5" = '5',
  "67.5" = '5-6', "68.5" = '5-6', "69.5" = '6', "70.5" = '6', "71.5" = '6', "72.5" = '6', "73.5" = '6', "74.5" = '6',
  "75.5" = '6-7', "76.5" = '6-7', "77.5" = '7', "78.5" = '7', "79.5" = '7', "80.5" = '7', "81.5" = '7', "82.5" = '7',
  "83.5" = '7', "84.5" = '7', "85.5" = '7', "86.5" = '7-', "87.5" = '7-', "88.5" = '7-', "89.5" = '7-', "90.5" = '7-',
  "91.5" = '7-', "92.5" = '7-'
  )


################################################################################
################################################################################

################################################################################
###### Highly Active Sites - TOP 80% ######
################################################################################

top_act_pct <- 0.8
pct_el <- '40.5'
const <- 'eve_mel'

##### rw10 ####
bs_db_top_act   <- read.csv(sprintf("%selap_%s_thr%s_top%s_bs_db_top_act.csv",  path_rw10, pattern_name_rw10, RMS_THR, top_act_pct), stringsAsFactors = FALSE)
bs_db_rep       <- read.csv(sprintf("%selap_%s_thr%s_top%s_bs_db_rep.csv",      path_rw10, pattern_name_rw10, RMS_THR, top_act_pct), stringsAsFactors = FALSE)

bs_db_top_act_pct <- bs_db_top_act %>% filter(pct_el == '40.5')
bs_db_rep_pct <- bs_db_rep %>% filter(pct_el == '40.5')

files_bm <- list()

for (file in unique(bs_db_top_act_pct$file)) {
  biological_meaning <- any(bs_db_top_act_pct[(bs_db_top_act_pct$file == file), 'enhancer_affect'])
  if (biological_meaning == TRUE) files_bm <- cbind(files_bm, file)
}
files_bm <- unlist(files_bm)

bs_db_top_act_bm <- bs_db_top_act_pct[(bs_db_top_act_pct$file %in% files_bm), ]
bs_db_rep_bm <- bs_db_rep_pct[(bs_db_rep_pct$file %in% files_bm), ]


fit_plot <- new(Organism, sprintf('%s%s', path_rw10, bs_db_top_act_bm$file[[1]]))
#####
param_db_rw10_cl <- read.csv(sprintf("%s%s_thr%s_w10_cluster_param_db.csv", results_path, pattern_name_rw10, RMS_THR), stringsAsFactors = FALSE)
#####

nk <- 3
nk_name <- sprintf('k_%s', nk)

for( k in seq(1, nk)) {
  bs_db_top_act_bm <- bs_db_top_act_pct[(bs_db_top_act_pct$file %in% files_bm), ]
  bs_db_rep_bm <- bs_db_rep_pct[(bs_db_rep_pct$file %in% files_bm), ]

  files_aux <- param_db_rw10_cl[param_db_rw10_cl[[nk_name]] == k, 'file']
  bs_db_top_act_bm <- bs_db_top_act_bm[bs_db_top_act_bm$file %in% files_aux,]
  bs_db_rep_bm <- bs_db_rep_bm[bs_db_rep_bm$file %in% files_aux,]

### TOP ACT
bs_db_top_cluster <- list()

bs_filter <- bs_db_top_act_bm[(bs_db_top_act_bm$construct == const) & (bs_db_top_act_bm$pct_el == pct_el), ]
bs_top_cluster <- unique(bs_filter[, c('tf', 'index', 'start', 'end', 'middle', 'pct_el')])
bs_top_cluster[c('meanEFV', 'stdEFV', 'meanAE', 'stdAE', 'count')] <- 0.0
for (idx in 1:length(bs_top_cluster[[1]])) {
  bs <- bs_top_cluster[idx, ]
  bs_top_cluster['count'][idx, ] <- sum((bs_filter$tf == bs$tf) & (bs_filter$index == bs$index))
  bs_top_cluster['meanAE'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'AE'])
  bs_top_cluster['stdAE'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'AE'])
  bs_top_cluster['meanEFV'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf)&(bs_filter$index == bs$index), 'EFV'])
  bs_top_cluster['stdEFV'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'EFV'])
}
bs_top_cluster[['construct']] <- const
bs_db_top_cluster <- rbind(bs_db_top_cluster, bs_top_cluster)

### REP
bs_db_rep_cluster <- list()

bs_filter <- unique(bs_db_rep_bm[(bs_db_rep_bm$construct == const) & (bs_db_rep_bm$pct_el == pct_el), ])
bs_rep_cluster <- unique(bs_filter[, c('tf', 'index', 'start', 'end', 'middle', 'pct_el')])
bs_rep_cluster[c('meanf', 'stdf', 'meanEf', 'stdEf', 'count')] <- 0.0
for (idx in 1:length(bs_rep_cluster[[1]])) {
  bs <- bs_rep_cluster[idx, ]
  bs_rep_cluster['count'][idx, ] <- sum((bs_filter$tf == bs$tf) & (bs_filter$index == bs$index))
  bs_rep_cluster['meanEf'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'Ef'])
  bs_rep_cluster['stdEf'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'Ef'])
  bs_rep_cluster['meanf'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf)&(bs_filter$index == bs$index), 'f'])
  bs_rep_cluster['stdf'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'f'])
}
bs_rep_cluster[['construct']] <- const
bs_db_rep_cluster <- rbind(bs_db_rep_cluster, bs_rep_cluster)

##### POPULAR SITES ALONG DNA SEQUENCE #####
repressor_filter <- 0.05
## activators_filter_alpha <- 0.05
## repressors_filter_alpha <- 0.05
activators_filter_alpha <- 0.1
repressors_filter_alpha <- 0.1

alpha_par_act <- 'meanEFV'# 'F' # 'EFV' # 'f' #
alpha_par_rep <- 'meanEf' # 'f' #

print(pct_el)
bs_db_rep_cluster_aux <- bs_db_rep_cluster[(bs_db_rep_cluster$meanf >= repressor_filter) & (bs_db_rep_cluster$pct_el == pct_el),]
## bs_db_rep_cluster_aux[['F']] <- 0.0
bs_db_rep_cluster_aux[['meanAE']] <- 0.0
bs_db_rep_cluster_aux[['stdAE']] <- 0.0
bs_db_rep_cluster_aux[['meanEFV']] <- 0.0
bs_db_rep_cluster_aux[['stdEFV']] <- 0.0

bs_db_top_cluster_aux <- bs_db_top_cluster[bs_db_top_cluster$pct_el == pct_el,]
bs_db_top_cluster_aux[['meanEf']] <- 0.0
bs_db_top_cluster_aux[['stdEf']] <- 0.0
bs_db_top_cluster_aux[['meanf']] <- 0.0
bs_db_top_cluster_aux[['stdf']] <- 0.0
## bs_db_top_cluster_aux[['gEf']] <- 0.0
## bs_db_top_cluster_aux[['dist']] <- 0.0
bs_db_top_cluster_aux$tf[bs_db_top_cluster_aux$tf == 'hb'] <- 'hb1'

bs_db_top_cluster_aux[['alpha']] <- 1.0
bs_db_rep_cluster_aux[['alpha']] <- 1.0

max_app <- max(bs_db_top_cluster_aux$count, bs_db_rep_cluster_aux$count)

for (idx in seq(dim(bs_db_top_cluster_aux)[1])) {
  bs <- bs_db_top_cluster_aux[idx, ]
  bs_db_top_cluster_aux['alpha'][idx, ] <- (bs$count / max_app ) * bs[alpha_par_act]
}
bs_db_top_cluster_aux['alpha'] <- bs_db_top_cluster_aux['alpha'] / max(bs_db_top_cluster_aux['alpha'])
bs_db_top_cluster_aux <- bs_db_top_cluster_aux[bs_db_top_cluster_aux$alpha >= activators_filter_alpha, ]

for (idx in seq(dim(bs_db_rep_cluster_aux)[1])) {
  bs <- bs_db_rep_cluster_aux[idx, ]
  bs_db_rep_cluster_aux['alpha'][idx, ] <- (bs$count / max_app ) * (bs[alpha_par_rep] * -1)
}
bs_db_rep_cluster_aux['alpha'] <- bs_db_rep_cluster_aux['alpha'] / max(bs_db_rep_cluster_aux['alpha'])
bs_db_rep_cluster_aux <- bs_db_rep_cluster_aux[bs_db_rep_cluster_aux$alpha >= repressors_filter_alpha, ]

## bs_db_top_act_aux[setdiff(colnames(bs_db_rep_aux),colnames(bs_db_top_act_aux))] <- 0.0
## bs_db_rep_aux[setdiff(colnames(bs_db_top_act_aux),colnames(bs_db_rep_aux))] <- 0.0

aux_text <- c(" "=1, 'Repression'=4, " "=2, 'Locus'=6.5, " "=5, 'Activation'=9,
              " "=7, " "=8, " "=7, " "=3)


TFBS <- rbind(bs_db_top_cluster_aux, bs_db_rep_cluster_aux)
TFBS[['color']] <- tf_color_02[TFBS$tf]
TFBS[['tf_idx']] <- tf_idx[TFBS$tf]
TFBS <- unique(TFBS)

x_min <- fit_plot$genes[[const]]$left_bound
x_min <- sign(x_min)*ceiling(abs(x_min)/10^floor(log10(abs(x_min))))*10^floor(log10(abs(x_min)))

x_max <- fit_plot$genes[[const]]$right_bound
x_max <- sign(x_max)*ceiling(abs(x_max)/10^floor(log10(abs(x_max))))*10^floor(log10(abs(x_max)))

x_reg_l <- x_min #-1700 #
x_reg_r <- x_max #-800 #
axis_step <- 2000 # 100 #
bp_axis_size <- abs(x_reg_l-x_reg_r)
tfbs_increase <- 15 # 0 #

sites_p <- ggplot(TFBS) +
  annotate("segment", x = x_reg_l, xend = x_reg_r, y = 6.5, yend = 6.5, color = "#000000", size = 2) +
  ## annotate("segment", x = x_reg_l, xend = x_reg_r, y = 6.5, yend = 6.5, color = "#FFFFFF", size = 2) +
  geom_segment( x = 0, y = 6.55, xend = 0, yend = 6.85, lineend = "square", linejoin = "round", size = 2,
               colour = "#000000")  +
  geom_segment( x = 0, y = 6.85, xend = 400, yend = 6.85, lineend = "butt", linejoin = "mitre", size = 2,
               arrow = arrow(length = unit(0.2, "inches")), colour = "#000000")  +
  annotate('rect', xmin = TFBS$start-tfbs_increase, xmax = TFBS$end+tfbs_increase, ymin = TFBS$tf_idx, ymax = TFBS$tf_idx+1,
           fill = TFBS$color, alpha = TFBS$alpha) +
  ## fill = TFBS$color, alpha = 1.0) +
  annotate('rect', xmin = enhancers_coords_00$start, xmax = enhancers_coords_00$end,
           ymin = 6.15, ymax = 6.85, fill="#E69F00", alpha = 1.0) +
  annotate('rect', xmin = mask_coords$start, xmax = mask_coords$end, ymin = 6.25, ymax = 6.75,
  ## annotate('tile', x = mask_coords$start, width = mask_coords$end, y = 6.25, height = 6.75,
           ## fill = "#7C3F00", alpha = 1.0, lwd = 1, lty = 1, col = '#FFFFFF', density=2) +
           fill = "#7C3F00", alpha = 1.0, lwd = 1, lty = 1, col = '#000000') +
  annotate('text', x = x_reg_l + bp_axis_size * 0.01, y = tf_idx+0.5, label = names(tf_idx),
           hjust = 0, color = "#636363", size = 12) +
           ## hjust = 0, color = "#FFFFFF", size = 7) +
  ## labs(title = bquote(bold("Sites contributing with "*.(top_act_pct * 100)*"% of expression and their regulators across multiple simulations at "
    labs(
      title = bquote(bold("Sites characterization through ensemble analysis at " *.(pct_el)*"%EL | k:"*.(k))),
      ## title = bquote(bold("Sites characterization through ensemble analysis at " *.(pct_el)*"%EL")),
      y = 'Highly Active Sites', x = 'Position (bp)') +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.1, size = 35, face = "bold"),
        panel.background = element_rect(fill = "white", color = 'transparent'),
        ## panel.background = element_rect(fill = "#FFFFFF", color = 'black'),
        ## panel.background = element_rect(fill = "#000000", color = 'black'),
        panel.border = element_rect(colour = "black", fill = 'transparent', size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25, hjust = 0.5, colour = "black"),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(size = 25, face="bold"),
        axis.title.y = element_text(size = 25, face="bold"),
        axis.ticks.length = unit(-0.5, "cm"),
        axis.ticks = element_line(color = "#000000"),
        ## axis.ticks = element_line(color = "#FFFFFF"),
        axis.ticks.length.y = unit(0, "cm"),
        aspect.ratio = 3.09 / 5,
        plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc")
        ) +
  scale_y_continuous(breaks = aux_text[seq(1, 15, 1)], expand = c(0, 0), limits = c(0.9, 11.1)) +
  scale_x_continuous(breaks = seq(-4000, 8000, axis_step), expand = c(0, 0), limits = c(x_reg_l, x_reg_r))
  ## scale_x_continuous(breaks = seq(x_reg_l, x_reg_r, axis_step), expand = c(0, 0), limits = c(x_reg_l,x_reg_r))

ggsave(sprintf('%s_sites_pct%s_act-%s_rep-%s_k%s.pdf', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep, k), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')
ggsave(sprintf('%s_sites_pct%s_act-%s_rep-%s_k%s.svg', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep, k), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')

}

## ggsave(sprintf('%s_sites_pct%s_act-%s_rep-%s.pdf', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')
## ggsave(sprintf('%s_sites_pct%s_act-%s_rep-%s.svg', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')

################################################################################
###### Ensemble Functional Cluster - Top 80% ######
################################################################################

top_act_pct <- 0.8
pct_el <- '40.5'
const <- 'eve_mel'

##### rw10 ####
bs_db_top_act   <- read.csv(sprintf("%selap_%s_thr%s_top%s_bs_db_top_act.csv",  path_rw10, pattern_name_rw10, RMS_THR, top_act_pct), stringsAsFactors = FALSE)
bs_db_coact     <- read.csv(sprintf("%selap_%s_thr%s_top%s_bs_db_coact.csv",  path_rw10, pattern_name_rw10, RMS_THR, top_act_pct), stringsAsFactors = FALSE)
bs_db_rep_tact   <- read.csv(sprintf("%selap_%s_thr%s_top%s_bs_db_rep_tact.csv",  path_rw10, pattern_name_rw10, RMS_THR, top_act_pct), stringsAsFactors = FALSE)

bs_db_top_act_pct <- bs_db_top_act %>% filter(pct_el == '40.5')
bs_db_coact_pct <- bs_db_coact %>% filter(pct_el == '40.5')
bs_db_rep_tact_pct <- bs_db_rep_tact %>% filter(pct_el == '40.5')

files_bm <- list()

for (file in unique(bs_db_top_act_pct$file)) {
  biological_meaning <- any(bs_db_top_act_pct[(bs_db_top_act_pct$file == file), 'enhancer_affect'])
  if (biological_meaning == TRUE) files_bm <- cbind(files_bm, file)
}
files_bm <- unlist(files_bm)

bs_db_top_act_bm  <- bs_db_top_act_pct[(bs_db_top_act_pct$file %in% files_bm), ]
bs_db_coact_bm    <- bs_db_coact_pct[(bs_db_coact_pct$file %in% files_bm), ]
bs_db_rep_tact_bm <- bs_db_rep_tact_pct[(bs_db_rep_tact_pct$file %in% files_bm), ]

fit_plot <- new(Organism, sprintf('%s%s', path_rw10, bs_db_top_act_bm$file[[1]]))
#####
param_db_rw10_cl <- read.csv(sprintf("%s%s_thr%s_w10_cluster_param_db.csv", results_path, pattern_name_rw10, RMS_THR), stringsAsFactors = FALSE)
#####

nk <- 3
nk_name <- sprintf('k_%s', nk)

for( k in seq(1, nk)) {

  bs_db_top_act_bm  <- bs_db_top_act_pct[(bs_db_top_act_pct$file %in% files_bm), ]
  bs_db_coact_bm    <- bs_db_coact_pct[(bs_db_coact_pct$file %in% files_bm), ]
  bs_db_rep_tact_bm <- bs_db_rep_tact_pct[(bs_db_rep_tact_pct$file %in% files_bm), ]

  files_aux <- param_db_rw10_cl[param_db_rw10_cl[[nk_name]] == k, 'file']
  bs_db_top_act_bm <- bs_db_top_act_bm[bs_db_top_act_bm$file %in% files_aux,]
  bs_db_coact_bm <- bs_db_coact_bm[bs_db_coact_bm$file %in% files_aux,]
  bs_db_rep_tact_bm <- bs_db_rep_tact_bm[bs_db_rep_tact_bm$file %in% files_aux,]


### TOP ACT
  bs_db_top_cluster <- list()

  bs_filter <- bs_db_top_act_bm[(bs_db_top_act_bm$construct == const) & (bs_db_top_act_bm$pct_el == pct_el), ]
  bs_top_cluster <- unique(bs_filter[, c('tf', 'index', 'start', 'end', 'middle', 'pct_el')])
  bs_top_cluster[c('meanEFV', 'stdEFV', 'meanAE', 'stdAE', 'count')] <- 0.0
  for (idx in 1:length(bs_top_cluster[[1]])) {
    bs <- bs_top_cluster[idx, ]
    bs_top_cluster['count'][idx, ] <- sum((bs_filter$tf == bs$tf) & (bs_filter$index == bs$index))
    bs_top_cluster['meanAE'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'AE'])
    bs_top_cluster['stdAE'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'AE'])
    bs_top_cluster['meanEFV'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf)&(bs_filter$index == bs$index), 'EFV'])
    bs_top_cluster['stdEFV'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index), 'EFV'])
  }
  bs_top_cluster[['construct']] <- const
  bs_db_top_cluster <- rbind(bs_db_top_cluster, bs_top_cluster)

### COACT
  bs_db_coact_cluster <- list()

  bs_filter <- unique(bs_db_coact_bm[(bs_db_coact_bm$construct == const) & (bs_db_coact_bm$pct_el == pct_el), ])
  if(nrow(bs_filter) > 0 ) {
    bs_coact_cluster <- unique(bs_filter[, c('tf', 'index', 'start', 'end', 'middle', 'pct_el', 'coact_tf', 'coact_idx', 'coact_middle', 'dist')])
    bs_coact_cluster[c('meangEf', 'stdgEf', 'meanEFV', 'stdEFV', 'count')] <- 0.0
    for (idx in 1:length(bs_coact_cluster[[1]])) {
      bs <- bs_coact_cluster[idx, ]
      bs_coact_cluster['count'][idx, ] <- sum((bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$coact_tf == bs$coact_tf) & (bs_filter$coact_idx == bs$coact_idx))
      bs_coact_cluster['meanEFV'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$coact_tf == bs$coact_tf) & (bs_filter$coact_idx == bs$coact_idx), 'EFV'])
      bs_coact_cluster['stdEFV'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$coact_tf == bs$coact_tf) & (bs_filter$coact_idx == bs$coact_idx), 'EFV'])
      bs_coact_cluster['meangEf'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf)&(bs_filter$index == bs$index) & (bs_filter$coact_tf == bs$coact_tf) & (bs_filter$coact_idx == bs$coact_idx), 'gEf'])
      bs_coact_cluster['stdgEf'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$coact_tf == bs$coact_tf) & (bs_filter$coact_idx == bs$coact_idx), 'gEf'])
    }
    bs_coact_cluster[['construct']] <- const
    bs_db_coact_cluster <- rbind(bs_db_coact_cluster, bs_coact_cluster)
  }
  bs_db_coact_cluster$coact_tf[bs_db_coact_cluster$coact_tf == 'hb'] <- 'hb1'

### REP OF TOP ACT
  bs_db_rep_tact_cluster <- list()

  bs_filter <- unique(bs_db_rep_tact_bm[(bs_db_rep_tact_bm$construct == const) & (bs_db_rep_tact_bm$pct_el == pct_el), ])
  bs_rep_tact_cluster <- unique(bs_filter[, c('tf', 'index', 'start', 'end', 'middle', 'pct_el', 'act_tf', 'act_idx', 'act_middle', 'dist')])
  bs_rep_tact_cluster[c('meangEf', 'stdgEf', 'meanEf', 'stdEf', 'count')] <- 0.0
  for (idx in 1:length(bs_rep_tact_cluster[[1]])) {
    bs <- bs_rep_tact_cluster[idx, ]
    bs_rep_tact_cluster['count'][idx, ] <- sum((bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$act_tf == bs$act_tf) & (bs_filter$act_idx == bs$act_idx))
    bs_rep_tact_cluster['meanEf'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$act_tf == bs$act_tf) & (bs_filter$act_idx == bs$act_idx), 'Ef'])
    bs_rep_tact_cluster['stdEf'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$act_tf == bs$act_tf) & (bs_filter$act_idx == bs$act_idx), 'Ef'])
    bs_rep_tact_cluster['meangEf'][idx, ] <- mean(bs_filter[(bs_filter$tf == bs$tf)&(bs_filter$index == bs$index) & (bs_filter$act_tf == bs$act_tf) & (bs_filter$act_idx == bs$act_idx), 'gEf'])
    bs_rep_tact_cluster['stdgEf'][idx, ] <- sd(bs_filter[(bs_filter$tf == bs$tf) & (bs_filter$index == bs$index) & (bs_filter$act_tf == bs$act_tf) & (bs_filter$act_idx == bs$act_idx), 'gEf'])
  }
  bs_rep_tact_cluster[['construct']] <- const
  bs_db_rep_tact_cluster <- rbind(bs_db_rep_tact_cluster, bs_rep_tact_cluster)
  bs_db_rep_tact_cluster$act_tf[bs_db_rep_tact_cluster$act_tf == 'hb'] <- 'hb1'


##### CLUSTER OF POPULAR SITES #####
  repressor_filter <- 0.05
  activators_filter_alpha <- 0.1
  repressors_filter_alpha <- 0.1
  coactivators_filter_alpha <- 0.1
  ## activators_filter_alpha <- 0.05
  ## repressors_filter_alpha <- 0.05
  ## coactivators_filter_alpha <- 0.005

  fit_lb <- fit_plot$genes$eve_mel$left_bound
  sequence <- fit_plot$genes$eve_mel$sequence

  bs_db_rep_cluster_aux <- bs_db_rep_cluster[(bs_db_rep_cluster$meanf >= repressor_filter) & (bs_db_rep_cluster$pct_el == pct_el),]
  ## bs_db_rep_cluster_aux[['F']] <- 0.0
  bs_db_rep_cluster_aux[['meanAE']] <- 0.0
  bs_db_rep_cluster_aux[['stdAE']] <- 0.0
  bs_db_rep_cluster_aux[['meanEFV']] <- 0.0
  bs_db_rep_cluster_aux[['stdEFV']] <- 0.0

  bs_db_top_cluster_aux <- bs_db_top_cluster[bs_db_top_cluster$pct_el == pct_el,]
  bs_db_top_cluster_aux[['meanEf']] <- 0.0
  bs_db_top_cluster_aux[['stdEf']] <- 0.0
  bs_db_top_cluster_aux[['meanf']] <- 0.0
  bs_db_top_cluster_aux[['stdf']] <- 0.0
  ## bs_db_top_cluster_aux[['gEf']] <- 0.0
  ## bs_db_top_cluster_aux[['dist']] <- 0.0
  bs_db_top_cluster_aux$tf[bs_db_top_cluster_aux$tf == 'hb'] <- 'hb1'

  bs_db_rep_tact_cluster_aux <- bs_db_rep_tact_cluster[(bs_db_rep_tact_cluster$pct_el == pct_el) & (bs_db_rep_tact_cluster$meangEf >= -1*repressor_filter),]

  alpha_par_act <- 'meanEFV'# 'F' # 'EFV' # 'f' #
  alpha_par_rep_coact <- 'meangEf' # 'f' #

  bs_db_top_cluster_aux[['alpha']] <- 1.0
  bs_db_rep_cluster_aux[['alpha']] <- 1.0
  bs_db_rep_tact_cluster_aux[['alpha']] <- 1.0

  bs_db_top_cluster_aux[c('sequence', 'label')] <- '-'
  bs_db_rep_cluster_aux[c('sequence', 'label')] <- '-'
  bs_db_rep_tact_cluster_aux[c('sequence', 'label')] <- '-'

  max_app <- max(bs_db_top_cluster_aux$count, bs_db_rep_tact_cluster_aux$count)

  for (idx in seq(dim(bs_db_top_cluster_aux)[1])) {
    bs <- bs_db_top_cluster_aux[idx, ]
    bs_db_top_cluster_aux['alpha'][idx, ] <- (bs$count / max_app ) * bs[alpha_par_act]
    ## bs_db_top_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)-abs(bs$start))+1,(abs(fit_lb)-abs(bs$end))+1)], collapse = '')
    bs_db_top_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)+bs$start)+1,(abs(fit_lb)+bs$end)+1)], collapse = '')
  }

  bs_db_top_cluster_aux['alpha'] <- bs_db_top_cluster_aux['alpha'] / max(bs_db_top_cluster_aux['alpha'])
  bs_db_top_cluster_aux <- bs_db_top_cluster_aux[bs_db_top_cluster_aux$alpha >= activators_filter_alpha, ]
  bs_db_top_cluster_aux <- bs_db_top_cluster_aux[order(bs_db_top_cluster_aux$alpha, decreasing = TRUE), ]

  ## cat('<><><><><>\n  ', pct_el, sprintf(' | Stripe %s', stripe_pct_exp[as.character(pct_el)]),'\n<><><><><>\n')
  ## cat('\n<> Cluster region: ', min(bs_db_top_cluster_aux$start), max(bs_db_top_cluster_aux$end), '\n')

  ## cat('\n## ACTIVATORS ##\n')
  act_cluster_aux <-  bs_db_top_cluster_aux[, c('tf','index' ,'start', 'end', 'count', 'pct_el', 'alpha', 'sequence')]
  ## print(sprintf('%d sites; TFs: %s;', dim(act_cluster_aux)[1], paste(unique(act_cluster_aux$tf), collapse = ', ')))
  ## print(act_cluster_aux)
  top_bs_id <- paste(bs_db_top_cluster_aux$tf, bs_db_top_cluster_aux$index, sep = '')

  rep_act_id <- paste(bs_db_rep_tact_cluster_aux$act_tf, bs_db_rep_tact_cluster_aux$act_idx, sep = '')
  bs_db_rep_tact_cluster_aux <- bs_db_rep_tact_cluster_aux[rep_act_id %in% top_bs_id,]

  for (idx in seq(dim(bs_db_rep_tact_cluster_aux)[1])) {
    bs <- bs_db_rep_tact_cluster_aux[idx, ]
    bs_db_rep_tact_cluster_aux['alpha'][idx, ] <- (bs$count / max_app ) * (bs[alpha_par_rep_coact] * -1)
    ## bs_db_rep_tact_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)-abs(bs$start))+1,(abs(fit_lb)-abs(bs$end))+1)], collapse = '')
    bs_db_rep_tact_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)+bs$start)+1,(abs(fit_lb)+bs$end)+1)], collapse = '')
  }
  bs_db_rep_tact_cluster_aux['alpha'] <- bs_db_rep_tact_cluster_aux['alpha'] / max(bs_db_rep_tact_cluster_aux['alpha'])
  bs_db_rep_tact_cluster_aux <- bs_db_rep_tact_cluster_aux[bs_db_rep_tact_cluster_aux$alpha >= repressors_filter_alpha, ]
  bs_db_rep_tact_cluster_aux <- bs_db_rep_tact_cluster_aux[order(bs_db_rep_tact_cluster_aux$alpha, decreasing = TRUE), ]

  ## cat('\n## REPRESSORS ##\n')
  rep_tact_cluster_aux <-  unique(bs_db_rep_tact_cluster_aux[, c('tf','index','start','end', 'pct_el', 'sequence')])
  rep_tact_cluster_aux <- rep_tact_cluster_aux[order(rep_tact_cluster_aux$tf),]
  ## print(sprintf('%d sites; TFs: %s;', dim(rep_tact_cluster_aux)[1], paste(unique(rep_tact_cluster_aux$tf), collapse = ', ')))
  ## print(rep_tact_cluster_aux)

  bs_db_coact_cluster_aux <- bs_db_coact_cluster[bs_db_coact_cluster$pct_el == pct_el,]
  coact_act_id <- paste(bs_db_coact_cluster_aux$coact_tf, bs_db_coact_cluster_aux$coact_idx, sep = '')
  bs_db_coact_cluster_aux <- bs_db_coact_cluster_aux[coact_act_id %in% top_bs_id,]

  is_coactivated <- ifelse(dim(bs_db_coact_cluster_aux)[1] > 0, TRUE, FALSE)
  if (is_coactivated) {
    bs_db_coact_cluster_aux[['alpha']] <- 1.0
    bs_db_coact_cluster_aux[c('sequence', 'label')] <- '-'

    for (idx in seq(dim(bs_db_coact_cluster_aux)[1])) {
      bs <- bs_db_coact_cluster_aux[idx, ]
      bs_db_coact_cluster_aux['alpha'][idx, ] <- (bs$count / max_app ) * bs[alpha_par_rep_coact]
      ## bs_db_coact_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)-abs(bs$start))+1,(abs(fit_lb)-abs(bs$end))+1)], collapse = '')
      bs_db_coact_cluster_aux['sequence'][idx, ] <- paste(sequence[seq((abs(fit_lb)+bs$start)+1,(abs(fit_lb)+bs$end)+1)], collapse = '')
    }
    bs_db_coact_cluster_aux['alpha'] <- bs_db_coact_cluster_aux['alpha'] / max(bs_db_coact_cluster_aux['alpha'])
    bs_db_coact_cluster_aux <- bs_db_coact_cluster_aux[bs_db_coact_cluster_aux$alpha >= coactivators_filter_alpha, ]
    bs_db_coact_cluster_aux <- bs_db_coact_cluster_aux[order(bs_db_coact_cluster_aux$alpha, decreasing = TRUE), ]

    ## cat('\n## COACTIVATION ##\n')
    coact_cluster_aux <-  unique(bs_db_coact_cluster_aux[, c('tf','index','start','end', 'pct_el', 'sequence')])
    coact_cluster_aux <- coact_cluster_aux[order(coact_cluster_aux$tf),]
    ## print(sprintf('%d sites; TFs: %s;', dim(coact_cluster_aux)[1], paste(unique(coact_cluster_aux$tf), collapse = ', ')))
    ## print(coact_cluster_aux)
  }


  x <- seq(fit_aux$genes$eve_mel$left_bound, fit_aux$genes$eve_mel$right_bound)
  y <- rep(0, times = fit_aux$genes$eve_mel$length)
  ## axis_min <- floor(min(bs_db_top_cluster_aux$middle) / 100) * 100 # -1650 #
  ## axis_max <- ceiling(max(bs_db_top_cluster_aux$middle) / 100) * 100 # -850 #
  axis_min <-  -1600 # 6500 #
  axis_max <-  -900 #8000 #
  step_x <- ifelse((axis_max - axis_min) < 1000, 100, floor((axis_max - axis_min)/1000)*250)

  bp_axis_size <- abs(axis_max-axis_min)
  coact_dist <- c(fit_aux$parameters[[8]]$value, 50)

  data  <- data.frame(x = x, y = y)
  data1 <- data.frame(x = x, y = y + 150)
  data2 <- data.frame(x = x, y = y - 150)

  bs_db_top_cluster_aux[['color']] <- tf_color_02[bs_db_top_cluster_aux$tf]
  bs_db_rep_tact_cluster_aux[['color']] <- tf_color_02[bs_db_rep_tact_cluster_aux$tf]
  bs_db_coact_cluster_aux[['color']] <- tf_color_02[bs_db_coact_cluster_aux$tf]

  cluster_plot <-  ggplot(data, aes(x = x, y = y)) +
    geom_line(color = "black") +
    geom_line(color = "#636363", data = data1, linetype = "dashed") +
    geom_line(color = "#636363", data = data2, linetype = "dashed") +
    annotate('rect', xmin = enhancers_coords_00$start, xmax = enhancers_coords_00$end,
             ymin = 230, ymax = 250, fill="#E69F00", alpha = 1.0) +
    annotate('rect', xmin = mask_coords$start, xmax = mask_coords$end, ymin = 230, ymax = 250,
             fill = "#7C3F00", alpha = 1.0, lwd = 1, lty = 1, col = '#000000')


  if (is_coactivated){
    cluster_plot <- cluster_plot +
      geom_point(data = data.frame(x = bs_db_coact_cluster_aux$coact_middle, y = bs_db_coact_cluster_aux$dist),
                 aes(x = x, y = y, stroke = 1.5, fill = bs_db_coact_cluster_aux$tf, color = bs_db_coact_cluster_aux$tf, shape = bs_db_coact_cluster_aux$tf),
                 size = 6, alpha = bs_db_coact_cluster_aux$alpha) +
      geom_point(data = data.frame(x = bs_db_coact_cluster_aux$coact_middle, y = bs_db_coact_cluster_aux$dist),
                 aes(x = x, y = y,  color = bs_db_coact_cluster_aux$tf, shape = bs_db_coact_cluster_aux$tf ),
                 size = 10, stroke = 1.5, alpha = 0.8)
  }
  cluster_plot <- cluster_plot +
    geom_point(data = data.frame(x = bs_db_rep_tact_cluster_aux$act_middle, y = bs_db_rep_tact_cluster_aux$dist),
               aes(x = x, y = y, stroke = 1.5, fill = bs_db_rep_tact_cluster_aux$tf, color = bs_db_rep_tact_cluster_aux$tf, shape = bs_db_rep_tact_cluster_aux$tf),
               size = 6, alpha = bs_db_rep_tact_cluster_aux$alpha) +
    geom_point(data = data.frame(x = bs_db_rep_tact_cluster_aux$act_middle, y = bs_db_rep_tact_cluster_aux$dist),
               aes(x = x, y = y,  color = bs_db_rep_tact_cluster_aux$tf , shape = bs_db_rep_tact_cluster_aux$tf),
               size = 10, stroke = 1.5, alpha = 0.8) +
    geom_point(data = data.frame(x = bs_db_top_cluster_aux$middle, y = 0),
               aes(x = x, y = y, stroke = 1.5, fill = bs_db_top_cluster_aux$tf, color = bs_db_top_cluster_aux$tf, shape = bs_db_top_cluster_aux$tf),
               size = 6, alpha = bs_db_top_cluster_aux$alpha) +
    geom_point(data = data.frame(x = bs_db_top_cluster_aux$middle, y = 0),
               aes(x = x, y = y,  color = bs_db_top_cluster_aux$tf , shape = bs_db_top_cluster_aux$tf),
               size = 10, stroke = 1.5, alpha = 1.0) +
    labs(
      title = bquote(bold("Functional cluster ensemble analysis at " *.(pct_el)*"%EL | k:"*.(k))),
      ## title = bquote(bold("Functional cluster ensemble analysis at " *.(pct_el)*"%EL")),
      y = 'Distance from activators (bp)', x = 'Position (bp)') +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 35, face = "bold"),
          panel.background = element_rect(fill = "white", color = 'black'),
          panel.border = element_rect(colour = "black", fill = NA, size = 3),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text = element_text(size = 25, hjust = 0.5, colour = '#000000'),
          axis.text.y = element_text(angle = 90, size = 30, vjust = 0.5, hjust = 0.5),
          ## axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.title.x = element_text(size = 25, face = "bold"),
          axis.title.y = element_text(size = 25, face = "bold"),
          axis.ticks.length = unit(-0.5, "cm"),
          axis.ticks = element_line(color = "#000000"),
          legend.title = element_text(size = 25, colour = 'black'),
          legend.text = element_text(size = 24, colour = 'black'),
          legend.position =  'right',
          legend.key.spacing.y = unit(0.6, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = "#FFFFFF"),
          legend.background = element_rect(fill = "transparent"),
          aspect.ratio = 3.09 / 5,
          plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc")
          ) +
    scale_x_continuous(breaks = seq(axis_min, axis_max, step_x), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-250, 250, 50), expand = c(0, 0)) +
    coord_cartesian(xlim = c(axis_min - bp_axis_size*0.04, axis_max + bp_axis_size*0.04), ylim = c(-250, 250)) +
    scale_color_manual(values = tf_color_02, guide = guide_legend(title = "TF", byrow = TRUE)) +
    scale_fill_manual(values = tf_color_02, guide = guide_legend(title = "TF", byrow = TRUE)) +
    scale_shape_manual(values = tf_symbol_02, guide = guide_legend(title = "TF", byrow = TRUE))
  ## plot(cluster_plot)


  ## ggsave(sprintf('%s_functional_pct%s_act-%s_rep-%s_k%sstr15.pdf', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep_coact, k), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')

  ggsave(sprintf('%s_functional_pct%s_act-%s_rep_coact-%s_k%s.pdf', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep_coact, k), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')
  ggsave(sprintf('%s_functional_pct%s_act-%s_rep_coact-%s_k%s.svg', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep_coact, k), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')
}

## ggsave(sprintf('%s_functional_pct%s_act-%s_rep_coact-%s.pdf', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep_coact), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')
## ggsave(sprintf('%s_functional_pct%s_act-%s_rep_coact-%s.svg', pattern_name_rw10, pct_el, alpha_par_act, alpha_par_rep_coact), scale = 2, dpi = 600, width = 8, height = 5.25, units = 'in')


###################################################str15#############################
###### CLUSTERING ######
####################################################str15############################


##### w1000 ####
#### DEFAULT BM ####
param_db_dw10str15 <- read.csv(sprintf("%s%s_thr%s_w1000_param_db.csv", path_dw10, pattern_name_dw10, RMS_THR), stringsAsFactors = FALSE)
param_db_dw10_scaled <- scale(param_db_dw10[, !names(param_db_dw10) %instr15% c('file')])
rownames(param_db_dw10_scaled) <- param_db_dw10[,'file'] %>% substr(8,11)
param_db_dw10['model'] <- param_db_dw10[,'file'] %>%str15 substr(8,11)

cluster_dw10_list <- list()
for (k in seq(3,8)) {
  str15cluster_dw10_list <- rbind(cluster_dw10_list, kmeans(param_db_dw10_scaled, centers=k, nstart=125, iter.max=30))
}

pdf(sprintf('%s%s_dist_violin_cluster.pdf', resultsstr15_path, pattern_name_dw10), width = 19, height = 9.09135017545)
par(family = "sans", mar = c( 0, 0, 1.7, 1.7),  oma = c(3.5, 4.5, 0.5, 0),
    mgp = c(1.5,0.2,0), cex.axis =str15 2.5, cex.lab = 2.5, cex.main = 2.82)

cluster_cols <- c('file', 'model')

for ( nk in seq(str151,6)) {
  kc <- cluster_dw10_list[nk, ]
  aux_k <- list()
  aux_k['cluster'] <- listr15st(unname(kc$cluster))
  aux_k['model'] <- list(names(kc$cluster))

  param_db_dw10['cluster'] <- 0.0
  for (k in unique(aux_k$cluster)) {
    models_sc <- aux_k$model[aux_k$cluster == k]
    param_db_dw10[param_db_dw10$model %in% models_sc, 'cluster'] <- as.character(k)
  }

  ## param_db_aux <- param_db_dw10 %>% select(!c('file', 'model', cluster_cols))
  param_db_aux <- param_db_dw10 %>% select(!cluster_cols)
  melted_df_h <-  param_db_aux %>%
    pivot_longer(cols = !c(cluster), names_to = "var", values_to = c("value"))
  melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

  k_plot <- ggplot(melted_df_h, aes(x = cluster, y = value)) +
    geom_violin() +
    labs(title = bquote(bold("Distribution of the Annealed parameters by cluster | # models by k: {"~ .(paste(kc$size, collapse = ', '))~ "}")),
         y = 'Values (AU)', x = 'Annealed variables') +
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
          panel.background = element_rect(fill = "white", color = 'black', size = 2),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16),
          axis.ticks.length.y = unit(-0.25, "cm"),
          axis.ticks.length.x = unit( 0.0, "cm"),
          plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
    facet_wrap(~ var, scales = "free", ncol = 9)
  plot(k_plot)
  cluster_col <- sprintf('k_%s', length(kc$size))
  colnames(param_db_dw10)[colnames(param_db_dw10) == 'cluster'] <- cluster_col
  cluster_cols <- append(cluster_cols, cluster_col)
}
dev.off()
write.csv(param_db_dw10, sprintf("%s%s_thr%s_w10_cluster_param_db.csv", results_path, pattern_name_dw10, RMS_THR), row.names=FALSE)

#### REDUCED BM ####
param_db_rw10 <- read.csv(sprintf("%s%s_thr%s_w1000_param_db.csv", path_rw10, pattern_name_rw10, RMS_THR), stringsAsFactors = FALSE)
param_db_rw10_scaled <- scale(param_db_rw10[, !names(param_db_rw10) %in% c('file')])
rownames(param_db_rw10_scaled) <- param_db_rw10[,'file'] %>% substr(11,14)
param_db_rw10['model'] <- param_db_rw10[,'file'] %>% substr(11,14)


cluster_rw10_list <- list()
for (k in seq(3,8)) {
  cluster_rw10_list <- rbind(cluster_rw10_list, kmeans(param_db_rw10_scaled, centers=k, nstart=125, iter.max=50))
}

pdf(sprintf('%s%s_dist_violin_cluster.pdf', results_path, pattern_name_rw10), width = 19, height = 9.09135017545)
par(family = "sans", mar = c( 0, 0, 1.7, 1.7),  oma = c(3.5, 4.5, 0.5, 0),
    mgp = c(1.5,0.2,0), cex.axis = 2.5, cex.lab = 2.5, cex.main = 2.82)

cluster_cols <- c('file', 'model')

for ( nk in seq(1,6)) {
  kc <- cluster_rw10_list[nk, ]
  aux_k <- list()
  aux_k['cluster'] <- list(unname(kc$cluster))
  aux_k['model'] <- list(names(kc$cluster))

  param_db_rw10['cluster'] <- 0.0
  for (k in unique(aux_k$cluster)) {
    models_sc <- aux_k$model[aux_k$cluster == k]
    param_db_rw10[param_db_rw10$model %in% models_sc, 'cluster'] <- as.character(k)
  }

  ## param_db_aux <- param_db_rw10 %>% select(!c('file', 'model', cluster_cols))
  param_db_aux <- param_db_rw10 %>% select(!cluster_cols)
  melted_df_h <-  param_db_aux %>%
    pivot_longer(cols = !c(cluster), names_to = "var", values_to = c("value"))
  melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

  k_plot <- ggplot(melted_df_h, aes(x = cluster, y = value)) +
    geom_violin() +
    labs(title = bquote(bold("Distribution of the Annealed parameters by cluster | # models by k: {"~ .(paste(kc$size, collapse = ', '))~ "}")),
         y = 'Values (AU)', x = 'Annealed variables') +
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
          panel.background = element_rect(fill = "white", color = 'black', size = 2),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16),
          axis.ticks.length.y = unit(-0.25, "cm"),
          axis.ticks.length.x = unit( 0.0, "cm"),
          plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
    facet_wrap(~ var, scales = "free", ncol = 9)
  plot(k_plot)
  cluster_col <- sprintf('k_%s', length(kc$size))
  colnames(param_db_rw10)[colnames(param_db_rw10) == 'cluster'] <- cluster_col
  cluster_cols <- append(cluster_cols, cluster_col)
}
dev.off()
write.csv(param_db_rw10, sprintf("%s%s_thr%s_w10_cluster_param_db.csv", results_path, pattern_name_rw10, RMS_THR), row.names=FALSE)

################################

##### w500 ####
#### DEFAULT BM ####
param_db_dw5 <- read.csv(sprintf("%s%s_thr%s_w500_param_db.csv", path_dw5, pattern_name_dw5, RMS_THR), stringsAsFactors = FALSE)
param_db_dw5_scaled <- scale(param_db_dw5[, !names(param_db_dw5) %in% c('file')])
rownames(param_db_dw5_scaled) <- param_db_dw5[,'file'] %>% substr(11,14)
param_db_dw5['model'] <- param_db_dw5[,'file'] %>% substr(11,14)

cluster_dw5_list <- list()
for (k in seq(3,8)) {
  cluster_dw5_list <- rbind(cluster_dw5_list, kmeans(param_db_dw5_scaled, centers=k, nstart=125, iter.max=30))
}

pdf(sprintf('%s%s_dist_violin_cluster.pdf', results_path, pattern_name_dw5), width = 19, height = 9.09135017545)
par(family = "sans", mar = c( 0, 0, 1.7, 1.7),  oma = c(3.5, 4.5, 0.5, 0),
    mgp = c(1.5,0.2,0), cex.axis = 2.5, cex.lab = 2.5, cex.main = 2.82)

cluster_cols <- c('file', 'model')

for ( nk in seq(1,6)) {
  kc <- cluster_dw5_list[nk, ]
  aux_k <- list()
  aux_k['cluster'] <- list(unname(kc$cluster))
  aux_k['model'] <- list(names(kc$cluster))

  param_db_dw5['cluster'] <- 0.0
  for (k in unique(aux_k$cluster)) {
    models_sc <- aux_k$model[aux_k$cluster == k]
    param_db_dw5[param_db_dw5$model %in% models_sc, 'cluster'] <- as.character(k)
  }

  ## param_db_aux <- param_db_dw5 %>% select(!c('file', 'model', cluster_cols))
  param_db_aux <- param_db_dw5 %>% select(!cluster_cols)
  melted_df_h <-  param_db_aux %>%
    pivot_longer(cols = !c(cluster), names_to = "var", values_to = c("value"))
  melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

  k_plot <- ggplot(melted_df_h, aes(x = cluster, y = value)) +
    geom_violin() +
    labs(title = bquote(bold("Distribution of the Annealed parameters by cluster | # models by k: {"~ .(paste(kc$size, collapse = ', '))~ "}")),
         y = 'Values (AU)', x = 'Annealed variables') +
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
          panel.background = element_rect(fill = "white", color = 'black', size = 2),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16),
          axis.ticks.length.y = unit(-0.25, "cm"),
          axis.ticks.length.x = unit( 0.0, "cm"),
          plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
    facet_wrap(~ var, scales = "free", ncol = 9)
  plot(k_plot)
  cluster_col <- sprintf('k_%s', length(kc$size))
  colnames(param_db_dw5)[colnames(param_db_dw5) == 'cluster'] <- cluster_col
  cluster_cols <- append(cluster_cols, cluster_col)
}
dev.off()
write.csv(param_db_dw5, sprintf("%s%s_thr%s_w500_cluster_param_db.csv", results_path, pattern_name_dw5, rms_thr), row.names=FALSE)


#### REDUCED BM ####
param_db_rw5 <- read.csv(sprintf("%s%s_thr%s_w500_param_db.csv", path_rw5, pattern_name_rw5, rms_thr), stringsAsFactors = FALSE)
param_db_rw5_scaled <- scale(param_db_rw5[, !names(param_db_rw5) %in% c('file')])
rownames(param_db_rw5_scaled) <- param_db_rw5[,'file'] %>% substr(12,15)
param_db_rw5['model'] <- param_db_rw5[,'file'] %>% substr(12,15)

cluster_rw5_list <- list()
for (k in seq(3,8)) {
  cluster_rw5_list <- rbind(cluster_rw5_list, kmeans(param_db_rw5_scaled, centers=k, nstart=125, iter.max=30))
}

pdf(sprintf('%s%s_dist_violin_cluster.pdf', results_path, pattern_name_rw5), width = 19, height = 9.09135017545)
par(family = "sans", mar = c( 0, 0, 1.7, 1.7),  oma = c(3.5, 4.5, 0.5, 0),
    mgp = c(1.5,0.2,0), cex.axis = 2.5, cex.lab = 2.5, cex.main = 2.82)

cluster_cols <- c('file', 'model')

for ( nk in seq(1,6)) {
  kc <- cluster_rw5_list[nk, ]
  aux_k <- list()
  aux_k['cluster'] <- list(unname(kc$cluster))
  aux_k['model'] <- list(names(kc$cluster))

  param_db_rw5['cluster'] <- 0.0
  for (k in unique(aux_k$cluster)) {
    models_sc <- aux_k$model[aux_k$cluster == k]
    param_db_rw5[param_db_rw5$model %in% models_sc, 'cluster'] <- as.character(k)
  }

  ## param_db_aux <- param_db_rw5 %>% select(!c('file', 'model', cluster_cols))
  param_db_aux <- param_db_rw5 %>% select(!cluster_cols)
  melted_df_h <-  param_db_aux %>%
    pivot_longer(cols = !c(cluster), names_to = "var", values_to = c("value"))
  melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

  k_plot <- ggplot(melted_df_h, aes(x = cluster, y = value)) +
    geom_violin() +
    labs(title = bquote(bold("Distribution of the Annealed parameters by cluster | # models by k: {"~ .(paste(kc$size, collapse = ', '))~ "}")),
         y = 'Values (AU)', x = 'Annealed variables') +
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
          panel.background = element_rect(fill = "white", color = 'black', size = 2),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16),
          axis.ticks.length.y = unit(-0.25, "cm"),
          axis.ticks.length.x = unit( 0.0, "cm"),
          plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
    facet_wrap(~ var, scales = "free", ncol = 9)
  plot(k_plot)
  cluster_col <- sprintf('k_%s', length(kc$size))
  colnames(param_db_rw5)[colnames(param_db_rw5) == 'cluster'] <- cluster_col
  cluster_cols <- append(cluster_cols, cluster_col)
}
dev.off()
write.csv(param_db_rw5, sprintf("%s%s_thr%s_w500_cluster_param_db.csv", results_path, pattern_name_rw5, rms_thr), row.names=FALSE)

################################################################################
################################################################################


################################################################################
###### VIOLIN/HISTOGRAM COMPARISON ######
################################################################################

##### Default: w500 x w1000 #####
param_db_dw1['window'] <- 'W1000'
param_db_dw5['window'] <- 'W500'
param_db_dw <- rbind(param_db_dw1, param_db_dw5)

no_pivot <- append(cluster_cols, 'window')

melted_df_h <-  param_db_dw %>%
  pivot_longer(cols = !no_pivot, names_to = "var", values_to = c("value"))

melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

## plot <-ggplot(melted_df_h, aes(x = value, fill = window)) +
  ## geom_histogram(alpha=0.75, position="identity", aes(y=..density..)) +
plot<- ggplot(melted_df_h, aes(x = window, y = value)) +
  geom_violin() +
  labs(title = bquote(bold("bp window: 1k x 500 | Default search space")),
       y = 'Values (AU)', x = 'Annealed variables') +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
        panel.background = element_rect(fill = "white", color = 'black', size = 2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 12, hjust = 0.5),
        ## axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16),
        axis.ticks.length.y = unit(-0.25, "cm"),
        axis.ticks.length.x = unit( 0.0, "cm"),
        plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
  facet_wrap(~ var, scales = "free", ncol = 9)

ggsave(sprintf('%sdw_w5_w1k_dist_violin.pdf', results_path), width = 20,  height = 8.09017545)
## ggsave(sprintf('%sdw_w5_w1k_dist_hist.pdf', results_path), width = 20,  height = 8.09017545)

##### Reduced: w500 x w1000 #####
param_db_rw1['window'] <- 'W1000'
param_db_rw5['window'] <- 'W500'
param_db_rw <- rbind(param_db_rw1, param_db_rw5)

## no_pivot <- append(cluster_cols, 'window')

melted_df_h <-  param_db_rw %>%
  pivot_longer(cols = !no_pivot, names_to = "var", values_to = c("value"))

melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

## plot <- ggplot(melted_df_h, aes(x = value, fill = window)) +
  ## geom_histogram(alpha=0.75, position="identity", aes(y=..density..)) +
plot<-ggplot(melted_df_h, aes(x = window, y = value)) +
  geom_violin() +
  labs(title = bquote(bold("bp window: 1k x 500 | Reduced search space")),
       y = 'Values (AU)', x = 'Annealed variables') +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
        panel.background = element_rect(fill = "white", color = 'black', size = 2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 12, hjust = 0.5),
        ## axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16),
        axis.ticks.length.y = unit(-0.25, "cm"),
        axis.ticks.length.x = unit( 0.0, "cm"),
        plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
  facet_wrap(~ var, scales = "free", ncol = 9)

ggsave(sprintf('%srw_w5_w1k_dist_violin.pdf', results_path), width = 20,  height = 8.09017545)
## ggsave(sprintf('%srw_w5_w1k_dist_hist.pdf', results_path), width = 20,  height = 8.09017545)


##### w1000 ####
#### DEFAULT x REDUCED (RD) ####
param_db_dw1['sspace'] <- 'Default'
param_db_rw1['sspace'] <- 'Reduced'
param_db_w1 <- rbind(param_db_dw1, param_db_rw1)

no_pivot <- append(no_pivot, 'sspace')

melted_df_h <-  param_db_w1 %>%
  pivot_longer(cols = !no_pivot, names_to = "var", values_to = c("value"))

melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

## plot <- ggplot(melted_df_h, aes(x = value, fill = sspace)) +
  ## geom_histogram(alpha=0.75, position="identity", aes(y=..density..)) +
plot <- ggplot(melted_df_h, aes(x = sspace, y = value)) +
  geom_violin() +
  labs(title = bquote(bold("bp window 1k | Default x Reduced search space")),
       y = 'Values (AU)', x = 'Annealed variables') +
       ## y = 'Density', x = 'Annealed variables') +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
        panel.background = element_rect(fill = "white", color = 'black', size = 2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 12, hjust = 0.5),
        ## axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16),
        axis.ticks.length.y = unit(-0.25, "cm"),
        axis.ticks.length.x = unit( 0.0, "cm"),
        plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
  facet_wrap(~ var, scales = "free", ncol = 9)

ggsave(sprintf('%sw1k_sspace_dist_violin.pdf', results_path), width = 20,  height = 8.09017545)
## ggsave(sprintf('%sw1k_sspace_dist_hist.pdf', results_path), width = 20,  height = 8.09017545)

#### [RD] BM x <12 ####

################################

##### w500 ####
#### DEFAULT x REDUCED (RD) ####
param_db_dw5['sspace'] <- 'Default'
param_db_rw5['sspace'] <- 'Reduced'
param_db_w5 <- rbind(param_db_dw5, param_db_rw5)

## no_pivot <- append(cluster_cols, 'sspace')

melted_df_h <-  param_db_w5 %>%
  pivot_longer(cols = !no_pivot, names_to = "var", values_to = c("value"))

melted_df_h$var <- factor(melted_df_h$var, levels = order_list_01)

plot <- ggplot(melted_df_h, aes(x = value, fill = sspace)) +
  geom_histogram(alpha=0.75, position="identity", aes(y=..density..)) +
## plot<- ggplot(melted_df_h, aes(x = sspace, y = value)) +
  ## geom_violin() +
  labs(title = bquote(bold("bp window 500| Default x Reduced search space")),
       y = 'Values (AU)', x = 'Annealed variables') +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 19, face = "bold"),
 panel.background = element_rect(fill = "white", color = 'black', size = 2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 12, hjust = 0.5),
        ## axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16),
        axis.ticks.length.y = unit(-0.25, "cm"),
        axis.ticks.length.x = unit( 0.0, "cm"),
        plot.margin = unit(c(0.0, 0.01, 0.0, 0.01), "npc") )+
  facet_wrap(~ var, scales = "free", ncol = 9)

## ggsave(sprintf('%sw5_sspace_dist_violin.pdf', results_path), width = 20,  height = 8.09017545)
ggsave(sprintf('%sw5_sspace_dist_hist.pdf', results_path), width = 20,  height = 8.09017545)

#### [RD] BM x <12 ####

################################################################################
################################################################################
