

source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")

df <- create_home_data()

source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_table_1_function.R")

table_1 <- create_table_1()

source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_table_2_function.R")

table_2 <- create_table_2()


source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_figure_1_function.R")

figure_1 <- create_figure_1()



source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_figure_2_function.R")

figure_2 <- create_figure_2()


source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_figure_3_function.R")

figure_3 <- create_figure_3()


source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_figure_4_function.R")

figure_4 <- create_figure_4()

renv::snapshot()


#write.csv(df, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Final Data/final_data.csv")
#gtsave(table_1, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table_1_revised.html")
#save_as_docx(table_2, path = "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Tables/Table_Bayesian_Regressions_revision.docx")
#ggsave(plot=figure_1, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1_revised.jpeg", width=9.5, height=7, dpi=600)
#ggsave(plot=figure_2, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2_revised.jpeg", width=11, height=6, dpi=600)
#ggsave(plot=figure_3, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_3_revised.jpeg", width=12, height=11, dpi=600)
#ggsave(plot=figure_4, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_4_revised.jpeg", width=7, height=16, dpi=600)

