# 0. Dependencies ---------------------------------------------------------
library(here)
library(data.table)
library(ggplot2)
library(ggrepel) # For labels not to overlap
library(ggdist) # or ggforce::geom_link for gradient in ggplot2
library(khroma)
library(patchwork)

dt_multiverse <- readRDS(here::here("input", "dt_multiverse.RDS"))

if (exists('theme_tvp')) { # Custom ggplot theme
  ggplot2::theme_set(theme_tvp())
}

# 1. Process --------------------------------------------------------------


# 1.1 Compute Hamming distance --------------------------------------------

# Create five character string identifier for each universe

choice_names <- c("exclusion", "outcome", "treatment", "adjustment", "missingness")

vector_strings <- apply(t(dt_multiverse [, .SD, .SDcols = c(choice_names)]),
                        FUN = function(x) paste(substr(x, 1, 1), collapse = ""), MARGIN = 2) 

dt_multiverse[, string_id := vector_strings]

# From KHF study (KKKKC) as reference
dt_multiverse [, distance_from_KHF := sapply(string_id, function(x) {
  sum(strsplit(x, split = "")[[1]] != "K" & strsplit(x, split = "")[[1]] != "C") # C stands for Complete Case
})]

# From VØFB (VVVVI) study as reference
dt_multiverse[, distance_from_VØFB := sapply(string_id, function(x) {
  sum(strsplit(x, split = "")[[1]] != "V" & strsplit(x, split = "")[[1]] != "I")
})]

dt_multiverse




# 2. Plotting -------------------------------------------------------------
# 
# # Fix patchwork 1.3 failure with guides = 'collect':
# 
# guides_build_mod <- function (guides, theme){
#   legend.spacing.y <- calc_element("legend.spacing.y", theme)  # modified by me
#   legend.spacing.x <- calc_element("legend.spacing.x", theme)  # modified by me
#   legend.box.margin <- calc_element("legend.box.margin", theme) %||%
#     margin()
#   widths <- exec(unit.c, !!!lapply(guides, gtable_width))
#   heights <- exec(unit.c, !!!lapply(guides, gtable_height))
#   just <- valid.just(calc_element("legend.box.just", theme))
#   xjust <- just[1]
#   yjust <- just[2]
#   vert <- identical(calc_element("legend.box", theme), "horizontal")
#   guides <- lapply(guides, function(g) {
#     editGrob(g, vp = viewport(x = xjust, y = yjust, just = c(xjust,
#                                                              yjust), height = if (vert)
#                                                                heightDetails(g)
#                               else 1, width = if (!vert)
#                                 widthDetails(g)
#                               else 1))
#   })
#   guide_ind <- seq(by = 2, length.out = length(guides))
#   sep_ind <- seq(2, by = 2, length.out = length(guides) - 1)
#   if (vert) {
#     heights <- max(heights)
#     if (length(widths) != 1) {
#       w <- unit(rep_len(0, length(widths) * 2 - 1), "mm")
#       w[guide_ind] <- widths
#       w[sep_ind] <- legend.spacing.x
#       widths <- w
#     }
#   }
#   else {
#     widths <- max(widths)
#     if (length(heights) != 1) {
#       h <- unit(rep_len(0, length(heights) * 2 - 1), "mm")
#       h[guide_ind] <- heights
#       h[sep_ind] <- legend.spacing.y
#       heights <- h
#     }
#   }
#   widths <- unit.c(legend.box.margin[4], widths, legend.box.margin[2])
#   heights <- unit.c(legend.box.margin[1], heights, legend.box.margin[3])
#   guides <- gtable_add_grob(gtable(widths, heights, name = "guide-box"),
#                             guides, t = 1 + if (!vert)
#                               guide_ind
#                             else 1, l = 1 + if (vert)
#                               guide_ind
#                             else 1, name = "guides")
#   gtable_add_grob(guides, element_render(theme, "legend.box.background"),
#                   t = 1, l = 1, b = -1, r = -1, z = -Inf, clip = "off",
#                   name = "legend.box.background")
# }
# 
# environment(guides_build_mod) <- asNamespace('patchwork')
# assignInNamespace("guides_build", guides_build_mod, ns = "patchwork")



# Hyperparameters
alpha_par <- 0.9
dodge_width <- 0.7
line_width   <- 0.6
point_size  <- 5L

# For readability, we will invert the estimates for discontinuation (x -1) so that
# they are more readily comparable with those for initiation

dt_multiverse[, `:=`(point_estimate_mirror = point_estimate,
                     lower_CI_mirror       = lower_CI,
                     upper_CI_mirror       = upper_CI)]

dt_multiverse[treatment == "KHF", `:=`(point_estimate_mirror = -1*point_estimate,
                                       lower_CI_mirror       = -1*lower_CI,
                                       upper_CI_mirror       = -1*upper_CI)]

# Add an indicator for universes corresponding to original studies:
dt_multiverse[, original := fcase(
  string_id == "VVVVI", TRUE,
  string_id == "KKKKC", TRUE,
  default = FALSE
)]

# Choose to plot mirrored or non-mirrored plots
mirrored <-  TRUE

# Difference hamming plot or just estimates
differences <- TRUE

# Store original KHF study estimate
KHF_point_estimate <- ifelse(mirrored == TRUE, 0.22, -0.22)

# This and coord_cartesian was needed when convergence issues gave
# too long of a compatibility interval for the imputation cases. But issue's fixed now.
# # To which respective quantile tail should we crop the y-axes:
# quantile_lim <- 0.4


# Universum index plot
p1 <- ggplot(data = dt_multiverse,
             aes(x     = .universe,
                 y = if(mirrored)  {point_estimate_mirror} else point_estimate,
                 ymin    = if(mirrored) {lower_CI_mirror} else lower_CI,
                 ymax    = if(mirrored) {upper_CI_mirror} else upper_CI, 
                 shape   = outcome,
                 fill    = treatment,
                 color = treatment,
                 label   = string_id)
) + 
  geom_point(aes(size = analytic_n)) +
  geom_point(data = dt_multiverse[original == TRUE],
             stroke = 2,
             color = 'gray8',
             size = point_size,
             shape = c(21, 24)) +
  geom_linerange(linewidth = line_width, alpha = alpha_par*(2/3)) +
  geom_text_repel(vjust = 2, direction = "y", nudge_y = 0.9, seed = 1234, segment.color = NA
                  #, force_pull = 2, force = 4, nudge_y = 3
                  #, point.padding = 10L, seed = 1234, segment.colour = NA
  ) +
  coord_cartesian(ylim = c(-1, 1) 
  )   +
  # coord_cartesian(ylim = c(quantile(dt_multiverse$lower_CI, 1 - quantile_lim),
  #                          quantile(dt_multiverse$upper_CI, quantile_lim)
  # )
  # ) +
  labs(x = "Universe Index",
       y = "Effect estimate",
       color = "Treatment Variable",
       size  = "Analytic\nSample Size",
       shape = "Outcome Variable") + {labs(caption = if(mirrored) {'Mirrored'} else element_blank())} +
  khroma::scale_fill_bright(guide = 'none') +
  khroma::scale_color_bright(labels = c("KHF\n(Discontinuation)", "VØFB\n(Initiation)")) +
  scale_size(range = range(dt_multiverse$analytic_n)/1000, breaks = c(min(dt_multiverse$analytic_n), max(dt_multiverse$analytic_n))) +
  scale_shape(labels = c("KHF\n(GPA)", "VØFB\n(National test)")) +
  scale_y_continuous(breaks = c(-0.75,-0.50, -0.25, 0.00, KHF_point_estimate , 0.25, 0.50, 0.75),
                     labels = c("-0.75","-0.50", "-0.25", "0.00", glue::glue("{KHF_point_estimate} (KHF)"), "0.25", "0.50", "0.75"),
                     minor_breaks = c(-0.625, -0.375, -0.125, 0.125, 0.375, 0.625)) +
  guides(shape = guide_legend(override.aes = list(size = 5)), # Symbols in legend too small
         color = guide_legend(override.aes = list(size = 5, label = ""))) +
  theme(panel.grid.minor.y = # To remove additional intermediate line while matching the theme
          element_line(color = c('gray92', 'gray92', 'gray92', NA, 'gray92')),
        panel.grid.major.y = 
          element_line(color = c('gray92', 'gray92', 'gray92', 'gray92')))

if (differences == TRUE) {
  dt_multiverse[, estimate_diff_KHF  := abs(point_estimate) -
                  abs(dt_multiverse[string_id == "KKKKC", point_estimate])]
  
  dt_multiverse[, estimate_diff_VØFB := abs(point_estimate) -
                  abs(dt_multiverse[string_id == "VVVVI", point_estimate])]
  
  # dt_multiverse[, estimate_diff_KHF  := abs(point_estimate) -
  #                 abs(dt_multiverse[string_id == "KKKKC", point_estimate])]
  # 
  # dt_multiverse[, estimate_diff_VØFB := point_estimate -
  #                 dt_multiverse[string_id == "VVVVI", point_estimate]]
  
  dodge_width = 0.2
  
  # KHF as reference plot
  p2 <- ggplot(data = dt_multiverse,
               aes(x = distance_from_KHF,
                   y       = estimate_diff_KHF,
                   # ymin    = 
                   # ymax    =  
                   shape   = outcome,
                   fill    = treatment,
                   color = treatment,
                   label = string_id,
                   group = string_id
               )
  ) + 
    geom_point(aes(size = analytic_n),
               # position = position_dodge(width = 0.5),
               position = position_dodge(preserve = "total", width = dodge_width),
               alpha = alpha_par
    ) +
    # geom_linerange(linewidth = line_width,
    #                position = position_dodge(preserve = "total", width = dodge_width),
    #                alpha = alpha_par) +
    geom_text_repel(aes(segment.color = 'gray6'),
                    # vjust = 2,
                    point.padding = 0.065,
                    box.padding = 0.3,
                    min.segment.length = 0.9,
                    position = position_dodge(preserve = "total", width = dodge_width),
                    direction = "both",
                    force = 3,
                    force_pull = -0.2,
                    size = 3,
                    seed = 12345, max.overlaps = 50
    ) +
    # coord_cartesian(ylim = c(quantile(dt_multiverse$lower_CI, 1 - quantile_lim),
    #                          quantile(dt_multiverse$upper_CI, quantile_lim)
    # )
    # ) +
    labs(x = "Number of choices deviating from KHF study design (KKKKC)",
         y = "Relative difference in absolute effect point estimates",
         color = "Treatment Variable",
         size  = "Analytic\nSample Size",
         shape = "Outcome Variable") +
    coord_cartesian(ylim = c(min(dt_multiverse[, .SD, .SDcols = c("estimate_diff_VØFB", "estimate_diff_KHF")]),
                             max(dt_multiverse[, .SD, .SDcols = c("estimate_diff_VØFB", "estimate_diff_KHF")])) |>
                      round(1)) +
    khroma::scale_color_bright() +
    khroma::scale_fill_bright(guide = 'none') +
    scale_size(range = range(dt_multiverse$analytic_n)/1000, breaks = c(min(dt_multiverse$analytic_n), max(dt_multiverse$analytic_n))) +
    guides(color = guide_legend(override.aes = list(size = 5, label = "")),
           shape = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = 0:5)
  
  # VØFB as reference plot
  p3 <- ggplot(data = dt_multiverse,
               aes(x = distance_from_VØFB,
                   y = estimate_diff_VØFB,
                   # ymin    = if(mirrored) {lower_CI_mirror} else lower_CI,
                   # ymax    = if(mirrored) {upper_CI_mirror} else upper_CI, 
                   shape   = outcome,
                   fill    = treatment,
                   color = treatment,
                   label = string_id,
                   group = string_id
               )
  ) + 
    geom_point(aes(size = analytic_n),
               # position = position_dodge(width = 0.5),
               position = position_dodge(preserve = "total", width = dodge_width),
               alpha = alpha_par
    ) +
    # geom_linerange(linewidth = line_width,
    #                position = position_dodge(preserve = "total", width = dodge_width),
    #                alpha = alpha_par) +
    geom_text_repel(aes(segment.color = 'gray6'),
                    # vjust = 2,
                    point.padding = 0.06,
                    box.padding = 0.28,
                    min.segment.length = 0.9,
                    position = position_dodge(preserve = "total", width = dodge_width),
                    direction = "both",
                    force = 2,
                    force_pull = -0.22,
                    size = 3,
                    seed = 12345, max.overlaps = 50
    ) +
    # coord_cartesian(ylim = c(quantile(dt_multiverse$lower_CI, 1 - quantile_lim),
    #                          quantile(dt_multiverse$upper_CI, quantile_lim)
    # )
    # ) +
    labs(x = "Number of choices deviating from VØFB study design (VVVVI)",
         y = "Relative difference in absolute effect point estimates",
         color = "Treatment Variable",
         size  = "Analytic\nSample Size",
         shape = "Outcome Variable") +
    coord_cartesian(ylim = c(min(dt_multiverse[, .SD, .SDcols = c("estimate_diff_VØFB", "estimate_diff_KHF")]),
                             max(dt_multiverse[, .SD, .SDcols = c("estimate_diff_VØFB", "estimate_diff_KHF")])) |>
                      round(1)) +
    khroma::scale_color_bright() +
    khroma::scale_fill_bright(guide = 'none') +
    scale_size(range = range(dt_multiverse$analytic_n)/1000, breaks = c(min(dt_multiverse$analytic_n), max(dt_multiverse$analytic_n))) +
    guides(color = guide_legend(override.aes = list(size = 5, label = "")),
           shape = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = 0:5) + theme(axis.line.y  = element_blank(),
                                             axis.title.y = element_blank(),
                                             axis.ticks.y = element_blank(),
                                             axis.text.y  = element_blank())
} else if (differences == FALSE) {
  # KHF as reference plot
  p2 <- ggplot(data = dt_multiverse,
               aes(x = distance_from_KHF,
                   y = if(mirrored)  {point_estimate_mirror} else point_estimate,
                   ymin    = if(mirrored) {lower_CI_mirror} else lower_CI,
                   ymax    = if(mirrored) {upper_CI_mirror} else upper_CI, 
                   color = missingness,
                   shape = treatment,
                   label = string_id,
                   group = string_id
               )
  ) + 
    geom_point(           size = point_size, 
                          # position = position_dodge(width = 0.5),
                          position = position_dodge(preserve = "total", width = dodge_width),
    ) +
    geom_linerange(linewidth = line_width,
                   position = position_dodge(preserve = "total", width = dodge_width),
                   alpha = alpha_par) +
    geom_text_repel(aes(segment.color = 'gray6'), vjust = 2, position = position_dodge(preserve = "total", width = dodge_width),
                    direction = "both", force = 1.5, seed = 1234) +
    # coord_cartesian(ylim = c(quantile(dt_multiverse$lower_CI, 1 - quantile_lim),
    #                          quantile(dt_multiverse$upper_CI, quantile_lim)
    # )
    # ) +
    labs(x = "Number of choices deviating from KHF study (KKKKC)",
         y = "Effect estimate",
         color = "Missingness",
         shape = "Treatment Variable") +
    khroma::scale_color_bright() +
    scale_x_continuous(breaks = 0:5)
  
  # VØFB as reference plot
  p3 <- ggplot(data = dt_multiverse,
               aes(x = factor(distance_from_VØFB),
                   y = if(mirrored)  {point_estimate_mirror} else point_estimate,
                   ymin    = if(mirrored) {lower_CI_mirror} else lower_CI,
                   ymax    = if(mirrored) {upper_CI_mirror} else upper_CI, 
                   color = missingness,
                   shape = treatment,
                   label = string_id,
                   group = string_id
               )
  ) + 
    geom_point(           size = point_size, 
                          # position = position_dodge(width = 0.5),
                          position = position_dodge(preserve = "total", width = dodge_width),
    ) +
    geom_linerange(linewidth = line_width,
                   position = position_dodge(preserve = "total", width = dodge_width),
                   alpha = alpha_par) +
    geom_text_repel(aes(segment.color = 'gray6'), vjust = 2, position = position_dodge(preserve = "total", width = dodge_width),
                    direction = "x", force = 1.5, seed = 1234) +
    # coord_cartesian(ylim = c(quantile(dt_multiverse$lower_CI, 1 - quantile_lim),
    #                          quantile(dt_multiverse$upper_CI, quantile_lim)
    # )
    # ) +
    labs(x = "Number of choices deviating from VØFB study (VVVI)",
         y = "Effect estimate",
         color = "Missingness",
         shape = "Treatment Variable") + {labs(caption = if(mirrored) {'Mirrored'} else element_blank())} +
    khroma::scale_color_bright() +
    scale_x_discrete(breaks = 0:5) + theme(axis.line.y  = element_blank(),
                                           axis.title.y = element_blank(),
                                           axis.ticks.y = element_blank(),
                                           axis.text.y  = element_blank())
}

hamming_plot <- p2 + p3 + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')

# windows(height = 9, width = 16)
hamming_plot


# :: 2.1 Save plot objects and pdfs ----------------------------------------------------------------

suffix <- paste0(ifelse(mirrored, "_mirrored", ""))

saveRDS(p1,
        here::here("output", "plots", glue::glue("universes_plot{suffix}.RDS")))

ggsave(plot = p1,
       filename = glue::glue("universes_plot{suffix}.pdf"),
       device = 'pdf',
       path = here::here("output", "plots"),
       width = 12L,
       height = 6L)

if (differences == TRUE) {
  suffix <- paste0(ifelse(differences, "_diffs", ""))
}

saveRDS(hamming_plot,
        here::here("output", "plots", glue::glue("hamming_plot{suffix}.RDS")))

ggsave(plot = hamming_plot,
       filename = glue::glue("hamming_plot{suffix}.pdf"),
       device = 'pdf',
       path = here::here("output", "plots"),
       # width = 18.5,
       # height = 9.6
       width = 16,
       height = 9
)

# 3. Table ----------------------------------------------------------------

# Collapse the different imputation branches into one
dt_multiverse[, missingness := fcase(
  missingness == 'Imputation1', 'Imputation',
  missingness == 'Imputation2', 'Imputation',
  default = missingness
)]


dt_tables <- copy(dt_multiverse)

# Collapse the different imputation branches into one
dt_tables[, missingness := fcase(
  missingness == 'Imputation1', 'Imputation',
  missingness == 'Imputation2', 'Imputation',
  default = missingness
)]

# Exclude irrelevant variables
dt_tables <- dt_tables[, .SD, .SDcols = c(".universe", "exclusion", "outcome",
                                          "treatment", "adjustment", "missingness",
                                          "string_id", "distance_from_KHF", "distance_from_VØFB",
                                          "point_estimate", "lower_CI", "upper_CI",
                                          "analytic_n", "excluded_n")]

# Round numeric variables
dt_tables[, (c("point_estimate", "lower_CI", "upper_CI")) := lapply(.SD, function(x) round(x, 3)), .SDcols = c("point_estimate", "lower_CI", "upper_CI")]

# Create string for CI interval
dt_tables[, CI := glue::glue_data(.SD, ("[{lower_CI} ; {upper_CI}]"))]
dt_tables[, `:=`(lower_CI = NULL, upper_CI = NULL)] # No longer needed

# Re-order columns
setcolorder(dt_tables, c(".universe", "exclusion", "outcome", "treatment", "adjustment",
                         "missingness", "string_id", "distance_from_KHF", "distance_from_VØFB",
                         "point_estimate", "CI", "analytic_n", "excluded_n"))

stargazer_latex <- stargazer::stargazer(dt_tables, summary = FALSE, rownames = FALSE)

# Create a vector of string replacements 
replacements <- c(
  "CompleteCase" = "Complete Case",
  ".universe" = "Universe ID",
  "exclusion" = "Exclusion",
  "outcome" = "Outcome",
  "treatment" = "Treatment",
  "adjustment" = "Adjustment",
  "missingness" = "Missingness",
  "string\\_id" = "String ID",
  "distance\\_from\\_KHF" = "From KHF",
  "distance\\_from\\_VØFB" = "From V\\OFB",
  "point\\_estimate" = "Point Estimate",
  "CI"  = "CI_{95}",
  "VØFB" = "V\\OFB"
)

# Claude solution:
replaced_latex <- Reduce(function(text, i) {
  gsub(names(replacements)[i], replacements[i], text, fixed = TRUE)
}, seq_along(replacements), init = stargazer_latex)

writeLines(replaced_latex)
# Save output
writeLines(replaced_latex, con = here::here("output", "tables", "table_multiverse.txt"))


# 4. Meta-regression of estimates on choices ---------------------------------
marginalfx_output <- list()
summary_metafit   <- list()

metafit <- lm(point_estimate ~ exclusion + outcome + treatment + adjustment + missingness,
              data = as.data.frame(dt_multiverse))

summary_metafit$linear <- summary(metafit)

# This does not work in R 4.5.1 with marginaleffects 0.29.0 and data.table 1.17.8
# Error in bmerge(i, x, leftcols, rightcols, roll, rollends, nomatch, mult,  : 
#                   x..parameter_assignment is type list which is not supported by data.table join

# It does work in R 4.3.2 with marginaleffects 0.27.0 and data.table 1.17.4
marginalfx_output$linear <- marginaleffects::avg_comparisons(metafit)

summary(metafit)

# With first order interactions
metafit_inter1 <- lm(point_estimate ~ (exclusion + outcome + treatment + adjustment + missingness)^2,
                     data = dt_multiverse)

summary_metafit$inter1 <- summary(metafit_inter1)

marginalfx_output$inter1 <- marginaleffects::avg_comparisons(metafit_inter1)

# With second order interactions
metafit_inter2 <- lm(point_estimate ~ (exclusion + outcome + treatment + adjustment + missingness)^3,
                     data = dt_multiverse)

summary_metafit$inter2 <- summary(metafit_inter2)

marginalfx_output$inter2 <- marginaleffects::avg_comparisons(metafit_inter2)

# Ugly quick code:
digits <- 2L

# R squared and degrees of freedom
R_df <- list()
R_df$linear <- glue::glue("{summary_metafit$linear$r.squared |> round(x = _, digits)} ({summary_metafit$linear$df[[2]]})")
R_df$inter1 <- glue::glue("{summary_metafit$inter1$r.squared |> round(x = _, digits)} ({summary_metafit$inter1$df[[2]]})")
R_df$inter2 <- glue::glue("{summary_metafit$inter2$r.squared |> round(x = _, digits)} ({summary_metafit$inter2$df[[2]]})")


# For the manuscript, options and choices were split into two columns
meta_dt <- list(
  "Option and Choice Contrast" = c(glue::glue("{marginalfx_output$linear$term} {marginalfx_output$linear$contrast}"), "$R^2$ (degrees of freedom)" ),
  "Linear"                     = c(glue::glue("{marginalfx_output$linear$estimate |> round(x = _, digits)} [{marginalfx_output$linear$conf.low |> round(x = _, digits)} ; {marginalfx_output$linear$conf.high |> round(x = _, digits)}]"), R_df$linear),
  "Two-way interactions"  = c(glue::glue("{marginalfx_output$inter1$estimate |> round(x = _, digits)} [{marginalfx_output$inter1$conf.low |> round(x = _, digits)} ; {marginalfx_output$inter1$conf.high |> round(x = _, digits)}]"), R_df$inter1),
  "Three-way interactions"   = c(glue::glue("{marginalfx_output$inter2$estimate |> round(x = _, digits)} [{marginalfx_output$inter2$conf.low |> round(x = _, digits)} ; {marginalfx_output$inter2$conf.high |> round(x = _, digits)}]"), R_df$inter2)
) |> 
  data.table::as.data.table()

# Re-order to follow same order as in string ID
meta_dt <- meta_dt[, ordertemp := fcase(
  grepl("exclusion", `Option and Choice Contrast`),   1L, 
  grepl("outcome", `Option and Choice Contrast`),     2L, 
  grepl("treatment", `Option and Choice Contrast`),   3L, 
  grepl("adjustment", `Option and Choice Contrast`),  4L, 
  grepl("missingness", `Option and Choice Contrast`), 5L
)][order(ordertemp)][, ordertemp := NULL]

meta_dt[]

stargazer_latex <- stargazer::stargazer(meta_dt, summary = FALSE, rownames = FALSE)

# Create a vector of string replacements 
replacements <- c(
  "CompleteCase" = "Complete Case",
  "-" = "$-$",
  "exclusion" = "Exclusion:",
  "outcome" = "Outcome:",
  "treatment" = "Treatment:",
  "adjustment" = "Adjustment:",
  "missingness" = "Missingness:",
  "\\$R$\\hat{\\mkern6mu}$2\\$" = "$R^2$",
  "VØFB" = "V\\O FB"
)

# Claude solution:
replaced_latex <- Reduce(function(text, i) {
  gsub(names(replacements)[i], replacements[i], text, fixed = TRUE)
}, seq_along(replacements), init = stargazer_latex)


writeLines(replaced_latex)

# Save output
writeLines(replaced_latex, con = here::here("output", "tables", "table_metaregression.txt"))


# Session -----------------------------------------------------------------

# sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=Spanish_Chile.utf8  LC_CTYPE=Spanish_Chile.utf8    LC_MONETARY=Spanish_Chile.utf8 LC_NUMERIC=C                   LC_TIME=Spanish_Chile.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] patchwork_1.3.0   khroma_1.11.0     ggdist_3.3.3      ggrepel_0.9.6     ggplot2_3.5.2     data.table_1.17.4 here_1.0.1       
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.4           dplyr_1.1.3            compiler_4.3.2         tidyselect_1.2.1       Rcpp_1.0.12            systemfonts_1.0.5      scales_1.4.0          
# [8] marginaleffects_0.27.0 textshaping_0.3.7      R6_2.5.1               labeling_0.4.3         generics_0.1.3         distributional_0.3.2   backports_1.4.1       
# [15] checkmate_2.3.0        tibble_3.2.1           insight_1.3.0          rprojroot_2.0.4        pillar_1.9.0           RColorBrewer_1.1-3     rlang_1.1.2           
# [22] utf8_1.2.4             cli_3.6.1              withr_3.0.0            magrittr_2.0.3         grid_4.3.2             rstudioapi_0.15.0      lifecycle_1.0.4       
# [29] vctrs_0.6.4            glue_1.6.2             farver_2.1.1           ragg_1.2.6             fansi_1.0.5            stargazer_5.2.3        tools_4.3.2           
# [36] pkgconfig_2.0.3  