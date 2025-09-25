library(ggplot2)
library(data.table)
library(here)
library(khroma)

# Set custom theme
if ("theme_tvp" |> exists()) {
  ggplot2::theme_set(theme_tvp())
}


# Load resulting treatment pattern distribution from dt_keilow computed
# in main script in the TSD virtual machine

treat_groups <- readRDS(here::here("input", "treatment_distribution.RDS"))

# Create data table
dt_patterns <- as.data.table(list(
  "Percentage"             = c(11, 70, 19,
                               {treat_groups["Proportions", c("DPT", "APT", "CPT")]*100} |> round(3)),
  "Treatment Pattern"      = c("Discontinuous", "Ambiguous", "Continuous",
                               "Discontinuous", "Ambiguous", "Continuous") |> as.factor(),
  "Study Population"                  = c("KHF", "KHF", "KHF",
                   "VØFB", "VØFB", "VØFB") |> as.factor()
))

# Bar plot
barplot <- ggplot(data = dt_patterns, aes(x = `Treatment Pattern`, fill = `Study Population`, y  = Percentage)) + 
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_text(aes(label = Percentage),
            vjust    = -0.5,
            color    = "gray8",
            position = position_dodge(0.9)) +
  khroma::scale_fill_bright()


ggsave(filename = "fig_treatment_patterns.pdf",
       device   = "pdf",
       plot     = barplot,
       width    = 6L,
       height   = 6L,
       units    = 'in',
       path     = here::here("output", "plots")
)

