# About

`R` code for manuscript _A mixture multiverse replication approach to explore estimand differences between studies: Application to the effects of discontinuing versus initiating ADHD pharmaceutical treatment_ (Varnet Pérez & Biele, 2025), included as third article in my PhD dissertation.

Consists of three scripts:

- `main_mixture_multiverse.R`: This is the main script that processes the data and runs the multiverses. It thus cannot be run by the user, as it requires access to sensitive data which is not shared in the present repository. This script outputs a `dt_multiverse` RDS object which is a `data.table` including the results from the mixture multiverse analysis. Additionally, it also produces a `treatment_distribution.RDS` object that has the frequency table of the KHF study treatment operationalization applied to the Norwegian sample. Both files are stored in the `input` folder in the present repository.
- `process_dt_multiverse.R`: This script processes the multiverse output `dt_multiverse` to produce all Figures and Tables included in the manuscript. Since it works only with aggregated results, this script can be run by the user as all necessary files are included.
- `treatment_pattern_distribution.R`: This script processes the output `treatment_distribution.RDS` to produce Figure 3 in the manuscript, the bar chart comparing treatment group distributions between the original KHF study and its replication in the Norwegian sample.
