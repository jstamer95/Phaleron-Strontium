
#R Script for “Investigating the Residential History of the Esplanada 
#Mass Graves at Phaleron, Greece”

#by Julianne Stamer
#julianne.stamer@gmail.com

##Updated 2/5/2026

####REQUIRED PACKAGES###########################################################
library(tidyverse)
library(ggstatsplot) #a package that improves on ggplot
library(glue)
library(gridExtra)
library(rstatix) #makes the stats tests easier to do
library(plotly)

####IMPORT DATA#################################################################
Phaleron_diagenesis <- read.csv("https://raw.githubusercontent.com/jstamer95/Phaleron-Strontium/main/Phaleron_diagenesis.csv",
                                check.names = FALSE)

phaleron_Sr <- read.csv("https://raw.githubusercontent.com/jstamer95/Phaleron-Strontium/refs/heads/main/phaleron_Sr.csv",
                        check.names = FALSE) %>%
  select(-which(names(.) == ""))

Phaleron_map <- read.csv("https://raw.githubusercontent.com/jstamer95/Phaleron-Strontium/refs/heads/main/Phaleron_map.csv",
                         check.names = FALSE) %>%
  select(-which(names(.) == ""))
####DIAGENESIS##################################################################

ked_long <- Phaleron_diagenesis %>%
  pivot_longer(
    cols = c(2:8),
    names_to = "Element",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>%
  select(`Burial Number`, 
         `ACL Number`, 
         `Sample Category (bone, tooth, or calculus)`,
         Element, 
         Value)



ked_long_tooth <- ked_long %>%
  filter(`Sample Category (bone, tooth, or calculus)` == "tooth")

#there are 192 teeth from Phaleron
Phaleron_diagenesis_tooth <- Phaleron_diagenesis %>%
  filter(`Sample Category (bone, tooth, or calculus)` == "tooth")


#there are 175 bones from Phaleron
Phaleron_diagenesis_bone <- Phaleron_diagenesis %>%
  filter(`Sample Category (bone, tooth, or calculus)` == "bone")

  ##C/MTCs!!!###################################################################

##Violin plot for Phaleron

##will lock the y axes 
c.mtc.max <- max(ked_long$Value)
c.mtc.min <- min(ked_long$Value)

#this is teeth and bone
kamenov.plot.2 <- ggbetweenstats(
  data= ked_long, 
  x = Element,
  y= Value, #plotted in log10 to make the graph readable becuase of U
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
) +
  #adding the Kamenov thresholds
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.5,      # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 3,        # between 1 and 10 (on log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, size = 6, color = "darkred"
  ) +
  #labels and color
  labs(x = "Element", 
       y = "C/MTC Value", 
       fill = "Sample Category", 
       title = "C/MTC values for Phaleron") +
  scale_color_viridis_d(option = "inferno", direction = -1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))+
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

plot(kamenov.plot.2)


#this is only teeth:
kamenov.plot.tooth <- ggbetweenstats(
  data= ked_long_tooth, 
  x = Element,
  y= Value, #plotted in log10 to make the graph readable becuase of U
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
) +
  #adding the Kamenov thresholds
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.5,      # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 3,        # between 1 and 10 (on log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, size = 6, color = "darkred"
  ) +
  #labels and color
  labs(x = "Element", 
       y = "C/MTC Value", 
       fill = "Sample Category", 
       title = "C/MTC values for Phaleron") +
  scale_color_viridis_d(option = "inferno", direction = -1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))+
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

plot(kamenov.plot.tooth)

#how many samples have been altered and significantly altered?
##everyone first
ked_long_altered <- ked_long %>%
  filter(Value > 1)

ked_long_sig_altered <- ked_long %>%
  filter(Value > 10)

altered_counts <- ked_long_altered %>%
  count(`ACL Number`, name = "n_over_1") %>%
  left_join(phaleron_Sr, "ACL Number") %>%
  select(c(`ACL Number`, "n_over_1",`87Sr/86Sr`, material.type))


sig_altered_counts <- ked_long_sig_altered %>%
  count(`ACL Number`, name = "n_over_10") %>%
  left_join(phaleron_Sr, "ACL Number") %>%
  select(c(`ACL Number`, "n_over_10",`87Sr/86Sr`))

perc.Phaleron.altered <- round((nrow(altered_counts) / nrow(Phaleron_diagenesis)) * 100, 2)
perc.Phaleron.altered # 97%

perc.Phaleron.sig.altered <- round((nrow(sig_altered_counts) / nrow(Phaleron_diagenesis)) * 100, 2)
perc.Phaleron.sig.altered # 76.3%

##teeth only
ked_long_tooth_altered <- ked_long_tooth %>%
  filter(Value > 1)

ked_long_tooth_sig_altered <- ked_long_tooth %>%
  filter(Value > 10)

altered_counts_tooth <- ked_long_tooth_altered %>%
  count(`ACL Number`, name = "n_over_1") %>%
  left_join(phaleron_Sr, "ACL Number") %>%
  select(c(`ACL Number`, "n_over_1",`87Sr/86Sr`))



sig_altered_counts <- ked_long_tooth_sig_altered %>%
  count(`ACL Number`, name = "n_over_10") %>%
  left_join(phaleron_Sr, "ACL Number") %>%
  select(c(`ACL Number`, "n_over_10",`87Sr/86Sr`))

    ##Significant differences between bone and tooth, wilcox tests##############

C.MTC.pairwise.list <- ked_long %>%
  group_by(Element) %>%
  group_split() %>%
  set_names(purrr::map_chr(., ~ unique(.x$Element))) %>%
  purrr::map(~ pairwise.wilcox.test(
    x = .x$Value,
    g = .x$`Sample Category (bone, tooth, or calculus)`,
    p.adjust.method = "holm"
  ))

C.MTC.pairwise.df <- purrr::map_dfr(
  names(C.MTC.pairwise.list),
  ~ broom::tidy(C.MTC.pairwise.list[[.x]]) %>%
    mutate(Element = .x),
  .id = "element"
)


      ##violin plots of significantly different C/MTCs##########################

        ##238U####
ked_long_U <- ked_long %>%
  filter(Element == "238U C/MTC")

U.wilcox <- wilcox.test(Value ~ `Sample Category (bone, tooth, or calculus)`, 
                        data = ked_long_U) #storing the wilcox test value for U, they are significantly different between bone and tooth
U.wilcox$p.value
U.wilcox$statistic

kamenov.plot.U <- ggbetweenstats(
  data = ked_long_U, 
  x = `Sample Category (bone, tooth, or calculus)`,
  y = Value, 
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.3,          # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 8,            # between 1 and 10 (log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  labs(
    x = "", 
    y = "", 
    fill = "Sample Category", 
    title = expression({}^238*U ~ "at Phaleron"),
    subtitle = glue("Wilcoxon test: p = {format(U.wilcox$p.value, scientific = TRUE, digits = 3)}, W = {format(U.wilcox$statistic, scientific = FALSE, digits = 3)}")
  ) +
  scale_color_manual(
    values = c("bone" = "#110a31ff", "tooth" ="#450a49ff")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )+
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

kamenov.plot.U


        ##172Yb####

ked_long_Yb <- ked_long %>%
  filter(Element == "172Yb C/MTC")

Yb.wilcox <- wilcox.test(Value ~ `Sample Category (bone, tooth, or calculus)`, 
                         data = ked_long_Yb) #storing the wilcox test value for U, they are significantly different between bone and tooth
Yb.wilcox$p.value

kamenov.plot.Yb <- ggbetweenstats(
  data= ked_long_Yb, 
  x = `Sample Category (bone, tooth, or calculus)`, 
  y = Value, 
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.3,          # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 8,            # between 1 and 10 (log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  labs(
    x = "", 
    y = "", 
    fill = "Sample Category", 
    title = expression({}^172*Yb ~ "at Phaleron"),
    subtitle = glue("Wilcoxon test: p = {format(Yb.wilcox$p.value, scientific = TRUE, digits = 3)}, W = {format(Yb.wilcox$statistic, scientific = FALSE, digits = 3)}")
  ) +
  scale_color_manual(
    values = c("bone" = "#89224aff", "tooth" ="#ab2f5eff")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  ) +
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

plot(kamenov.plot.Yb)

        ##163Dy#### 
ked_long_Dy <- ked_long %>%
  filter(Element == "163Dy C/MTC")

Dy.wilcox <- wilcox.test(Value ~ `Sample Category (bone, tooth, or calculus)`, 
                         data = ked_long_Dy) #storing the wilcox test value for U, they are significantly different between bone and tooth
Dy.wilcox$p.value

kamenov.plot.Dy <- ggbetweenstats(
  data = ked_long_Dy, 
  x = `Sample Category (bone, tooth, or calculus)`,
  y = Value,
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
)  +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.3,          # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 8,            # between 1 and 10 (log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  labs(
    x = "", 
    y = "", 
    fill = "Sample Category", 
    title = expression({}^143*Dy ~ "at Phaleron"),
    subtitle = glue(
      "Wilcoxon test: p = {format(Dy.wilcox$p.value, scientific = TRUE, digits = 3)}, W = {format(Dy.wilcox$statistic, scientific = FALSE, digits = 3)}"
    )
  ) +
  scale_color_manual(
    values = c("bone" = "#ed4925ff", "tooth" = "#F98c0aff")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  ) +
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

plot(kamenov.plot.Dy)

        ##139La#### 

ked_long_La <- ked_long %>%
  filter(Element == "139La C/MTC")

La.wilcox <- wilcox.test(Value ~ `Sample Category (bone, tooth, or calculus)`, 
                         data = ked_long_La) #storing the wilcox test value for U, they are significantly different between bone and tooth
La.wilcox$p.value

kamenov.plot.La <- ggbetweenstats(
  data= ked_long_La, 
  x = `Sample Category (bone, tooth, or calculus)`,
  y= Value, #plotted in log10 to make the graph readable becuase of U
  pairwise.display = "none",
  results.subtitle = FALSE,
  centrality.plotting = FALSE
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred", linewidth = 1.5) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 1.5) +
  annotate(
    "text",
    x = -Inf, 
    y = 0.3,          # below y = 1 threshold
    label = "Altered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 8,            # between 1 and 10 (log10 scale)
    label = "Significantly\nAltered",
    hjust = -0.1, vjust = -0.5, 
    size = 6, color = "darkred"
  ) +
  labs(
    x = "", 
    y = "", 
    fill = "Sample Category", 
    title = expression({}^139*La ~ "at Phaleron"),
    subtitle = glue("Wilcoxon test: p = {format(La.wilcox$p.value, scientific = TRUE, digits = 3)}, W = {format(La.wilcox$statistic, scientific = FALSE, digits = 3)}")
  ) +
  
  scale_color_manual(
    values = c("bone" = "#f4de52ff", "tooth" ="#f1f17aff")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  ) +
  scale_y_log10(
    limits = c(0.1, 15000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1", "10", "100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.05))   # optional: small top padding
  )

plot(kamenov.plot.La)

        ##All sig different together####

bone.v.tooth <- grid.arrange(kamenov.plot.La, 
                             kamenov.plot.Dy, 
                             kamenov.plot.Yb, 
                             kamenov.plot.U, ncol = 2)  # 1 column = vertical stack



    ##is the esplanada differently altered than everyone else####
      ##first, group the Esplanada####
ked_long <- ked_long %>%
  left_join(
    phaleron_Sr %>%
      select(burial.number, grave.type),
    by = c("Burial Number" = "burial.number")
  ) 


ked_long <- ked_long %>%
  mutate(
    Esplanada = case_when(
      grave.type %in% c("Esplanada Row 1",
                        "Esplanada Row 2",
                        "Esplanada Row 3") ~ grave.type,
      TRUE ~ "General Burial Population"
    ),
    Esplanada.cond = if_else(
      Esplanada == "General Burial Population",
      "General Burial Population",
      "Esplanada"
    )
  ) %>%
  mutate(Esplanada = factor(
    Esplanada, levels = c(
      "General Burial Population",
      "Esplanada Row 1",
      "Esplanada Row 2",
      "Esplanada Row 3"
    )
  )) %>%
  mutate(Esplanada.cond = factor(
    Esplanada.cond, levels = c(
      "General Burial Population",
      "Esplanada"
    )
  ))
        ##teeth first####
ked_long_tooth <- ked_long %>%
  filter(`Sample Category (bone, tooth, or calculus)` == "tooth")
##then, do pairwise tests
C.MTC.pairwise.Esplanada.tooth.df <- ked_long_tooth %>%
  group_by(Element) %>%
  wilcox_test(
    Value ~ Esplanada.cond,
    p.adjust.method = "holm",
    exact = FALSE
  )


tooth_C_MTC_esplanada <- ked_long_tooth %>%
  group_by(Element, Esplanada.cond) %>%
  summarise(median = median(Value, na.rm = TRUE))


        ##bones next####

ked_long_bone <- ked_long %>%
  filter(`Sample Category (bone, tooth, or calculus)` == "bone")
##then, do pairwise tests
C.MTC.pairwise.Esplanada.bone.df <- ked_long_bone %>%
  group_by(Element) %>%
  wilcox_test(
    Value ~ Esplanada.cond,
    p.adjust.method = "holm",
    exact = FALSE
  )


bone_C_MTC_esplanada <- ked_long_bone %>%
  group_by(Element, Esplanada.cond) %>%
  summarise(median = median(Value, na.rm = TRUE))
#looking at the medians for context, the Esplanada has higher medians for all except
#U, the general population has slightly higher U



################################################################################
################################################################################

####RADIOGENIC##################################################################
phaleron_Sr_tooth <- phaleron_Sr %>%
  filter(material.type == "tooth") %>%
  mutate(grave.type = if_else(is.na(grave.type), "pit grave", grave.type)) 

phaleron_Sr_tooth <- phaleron_Sr_tooth %>%
  mutate(
    grave.type.grouped = case_when(
      str_starts(grave.type, "CMB") ~ "central sector mass grave",
      str_starts(grave.type, "Esplanada Row") ~ "Esplanada",
      TRUE ~ grave.type  # keep original if no match
    )
  )


length(unique(phaleron_Sr$burial.number)) #there are 176 individuals represented by a bone or tooth
length(unique(phaleron_Sr_tooth$burial.number)) #there are 163 individuals represented by a tooth
#13 individuals have a bone sample but no tooth sample

missing_a_tooth <- setdiff(
  phaleron_Sr$burial.number,   
  phaleron_Sr_tooth$burial.number 
)

missing_a_tooth

  ##data summary####

Sr_summary_all <- phaleron_Sr %>%
  summarise(
    Count = n(),
    Mean_87Sr_86Sr = mean(`87Sr/86Sr`),
    SD_87Sr_86Sr = sd(`87Sr/86Sr`),
    Min_87Sr_86Sr = min(`87Sr/86Sr`),
    Max_87Sr_86Sr = max(`87Sr/86Sr`))

#setting the ylim for all Sr charts
Sr.min <- Sr_summary_all$Min_87Sr_86Sr
Sr.max <- Sr_summary_all$Max_87Sr_86Sr

Sr_summary <- phaleron_Sr %>%
  summarise(
    Count = n(),
    Mean_87Sr_86Sr = mean(`87Sr/86Sr`),
    SD_87Sr_86Sr = sd(`87Sr/86Sr`),
    Min_87Sr_86Sr = min(`87Sr/86Sr`),
    Max_87Sr_86Sr = max(`87Sr/86Sr`),
    .groups = "drop"
  )
Sr_summary_type <- phaleron_Sr %>%
  group_by(material.type) %>%
  summarise(
    Count = n(),
    Mean_87Sr_86Sr = mean(`87Sr/86Sr`),
    SD_87Sr_86Sr = sd(`87Sr/86Sr`),
    Min_87Sr_86Sr = min(`87Sr/86Sr`),
    Max_87Sr_86Sr = max(`87Sr/86Sr`),
    .groups = "drop"
  )



phaleron.sr <- ggbetweenstats(
  data = phaleron_Sr,
  x = material.type,
  y = `87Sr/86Sr`,
  pairwise.display = "significant",
  results.subtitle = FALSE,
  centrality.plotting = TRUE, 
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.7,
    size = 5
  )
) +
  labs(
    x = "Sample Type",
    y = expression({}^87*Sr/{}^86*Sr),
    title = "Phaleron Radiogenic Values"
  )  +
  scale_color_viridis_d(option = "inferno", direction = -1) +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "solid"),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )+
  coord_cartesian(ylim = c(Sr.min, Sr.max))

phaleron.sr

 


################################################################################
##Determining Locality####
  ##are the data normally distributed? (no)####
  ##everyone####
qqnorm(phaleron_Sr$`87Sr/86Sr`)
qqline(phaleron_Sr$`87Sr/86Sr`, col = "red") 
shapiro.test(phaleron_Sr$`87Sr/86Sr`) #not normally distributed
#W = 0.51661, p-value < 2.2e-16

  ##teeth only####
qqnorm(phaleron_Sr_tooth$`87Sr/86Sr`)
qqline(phaleron_Sr_tooth$`87Sr/86Sr`, col = "red") 
shapiro.test(phaleron_Sr_tooth$`87Sr/86Sr`) #not normally distributed
#W = 0.63067, p-value < 2.2e-16

  ##Tried the Trimmed dataset and couldnt get it to be normal####
x <- phaleron_Sr_tooth$`87Sr/86Sr`   # replace with your variable

Q1 <- quantile(x, 0.25, na.rm = TRUE)
Q3 <- quantile(x, 0.75, na.rm = TRUE)
IQR_val <- IQR(x, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- x < lower_bound | x > upper_bound

phaleron_Sr_tooth <- phaleron_Sr_tooth %>%
  mutate(
    outlier_IQR = `87Sr/86Sr` < lower_bound | `87Sr/86Sr` > upper_bound
  )

trimmed_data <- phaleron_Sr_tooth %>%  
  filter(outlier_IQR == "FALSE")

qqnorm(trimmed_data$`87Sr/86Sr`)
qqline(trimmed_data$`87Sr/86Sr`, col = "red") 
shapiro.test(trimmed_data$`87Sr/86Sr`) #NOT NORMALLY DISTRIBUTED!!


  ##doing the mean +/- 2 SD method####

Phaleron_tooth_mean <- Sr_summary_type %>%
  filter(material.type == "tooth") %>%
  pull(Mean_87Sr_86Sr)
Phaleron_tooth_sd <- Sr_summary_type %>%
  filter(material.type == "tooth") %>%
  pull(SD_87Sr_86Sr)
Phaleron_2SD <- 2*Phaleron_tooth_sd
Sr.lower <- Phaleron_tooth_mean - Phaleron_2SD
Sr.upper <- Phaleron_tooth_mean  + Phaleron_2SD

  ##comparative data####

Frank.lower <- 0.70859 - 0.00066 ##two standard deviations from the mean
Frank.upper <- 0.70859 + 0.00066

  ##division rows####
##need to have one tooth per person (Littleton and Smith 2024)
phaleron_Sr_tooth_dups <- phaleron_Sr_tooth %>%
  group_by(`burial.number`) %>%
  filter(n() > 1) %>%
  ungroup()

phaleron_dups_remove <- phaleron_Sr_tooth_dups %>%
  filter(sector %in% c("IV", "5", "IX")) %>%
  filter(`Tooth Category` == "M3")

esplanada_dups_remove <- phaleron_Sr_tooth_dups %>%
  filter(sector == "XI") %>%
  filter(`Specimen Number` %in% c(
    "PBP-014",
    "PBP-020",
    "PBP-022",
    "PBP-023",
    "PBP-002",
    "PBP-026",
    "PBP-388",
    "PBP-011",
    "PBP-018"
  ))

phaleron_Sr_tooth_one <- phaleron_Sr_tooth %>%
  anti_join(phaleron_dups_remove, by = "ACL Number") %>%
  anti_join(esplanada_dups_remove, by = "ACL Number")

phaleron_Sr_tooth_one %>% ##there are no more duplicate burial numbers
  group_by(`burial.number`) %>%
  filter(n() > 1) %>%
  select(`ACL Number`, burial.number, `Tooth Category`) %>%
  ungroup()

#now we can sort by Sr value and describe the "jumps"
phaleron_Sr_tooth_one <- phaleron_Sr_tooth_one %>%
  arrange(`87Sr/86Sr`) %>%   # make sure it's sorted
  mutate(
    sr_diff = `87Sr/86Sr` - lag(`87Sr/86Sr`)
  ) %>%
  mutate(id_ordered = factor(burial.number, levels = burial.number)) %>%
  select(c(burial.number, `87Sr/86Sr`, sr_diff, everything()))

#lets plot the "jumps"
ggplot(phaleron_Sr_tooth_one, aes(x = sr_diff)) +
  geom_histogram(binwidth = 0.000001) +
  theme_classic() +
  labs(
    x = expression(paste(Delta, " ", {}^{87}, "Sr/", {}^{86}, "Sr")),
    y = "Count"
  )

division_row_summary <- phaleron_Sr_tooth_one %>%
  summarise(
    mean_sr_diff   = mean(sr_diff, na.rm = TRUE),
    median_sr_diff = median(sr_diff, na.rm = TRUE),
    sd_sr_diff     = sd(sr_diff, na.rm = TRUE),
    min_sr_diff    = min(sr_diff, na.rm = TRUE),
    max_sr_diff    = max(sr_diff, na.rm = TRUE),
    n              = sum(!is.na(sr_diff))
  )

qqnorm(phaleron_Sr_tooth_one$sr_diff)
qqline(phaleron_Sr_tooth_one$sr_diff, col = "red") 
shapiro.test(phaleron_Sr_tooth_one$sr_diff)

#lets plot the column chart and look for divisions#
division.rows.phaleron <- ggplot(phaleron_Sr_tooth_one,
                                 aes(
                                   x = id_ordered,
                                   y = `87Sr/86Sr`,
                                   text = paste(
                                     "Burial:", burial.number,
                                     "<br>Sr:", round(`87Sr/86Sr`, 5),
                                     "<br>Delta 87Sr/86Sr:", sr_diff
                                   )
                                 )) +
  geom_col(fill = "blue") +
  coord_cartesian(ylim = c(Sr.min, Sr.max)) +
  annotate(
    "segment",
    x = which(levels(phaleron_Sr_tooth_one$id_ordered) == "IV_174"),
    xend = which(levels(phaleron_Sr_tooth_one$id_ordered) == "IV_174"),
    y = 0.7086,
    yend = 0.7102,
    color = "red",
    linewidth = 0.5,
    arrow = arrow(length = unit(0.1, "npc"), type = "closed"))+
  annotate(
    "segment",
    x = which(levels(phaleron_Sr_tooth_one$id_ordered) == "IV_556"),
    xend = which(levels(phaleron_Sr_tooth_one$id_ordered) == "IV_556"),
    y = 0.7097,
    yend = 0.7112,
    color = "red",
    linewidth = 0.5,
    arrow = arrow(length = unit(0.1, "npc"), type = "closed")) +
  labs(
    title = "Division Rows for Phaleron",
    x = "Individuals (ordered by Sr)",
    y =  "87Sr/86Sr"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.text.x = element_blank()) +
  theme_minimal() 


ggplotly(division.rows.phaleron, tooltip = "text")

jump.upper <- 0.70955 #updated on 1/22/2026
#the division here is between 0.70853 and 0.70869 
#(and the change is 0.00016 which is greater than uncertainty)
jump.lower <- 0.70860 #updated on 1/22/2026
#the division here is between 0.70950 and 0.70965 
#(and the change is 0.00015 which is greater than uncertainty)
  ##who is local?####
phaleron_Sr_tooth <- phaleron_Sr_tooth %>%
  mutate(SD2.local = case_when(
    `87Sr/86Sr` > Sr.upper ~ "non-local",
    `87Sr/86Sr` < Sr.lower ~ "non-local",
    TRUE ~ "local"
  )) %>%
  mutate(jump.local = case_when(
    `87Sr/86Sr` > jump.upper ~ "probable local",
    `87Sr/86Sr` < jump.lower ~ "probable local",
    TRUE ~ "local"
  )) %>%
  mutate(
    local = case_when(
      SD2.local == "non-local" ~ "non-local",
      jump.local == "probable local" ~ "probable local",
      TRUE ~ "local"
    ),
    local = factor(local, levels = c(
      "local",
      "probable local",
      "non-local"
    ))
  ) %>%
  select(c(
    burial.number,
    `87Sr/86Sr`, 
    local,
    everything())) %>%
  mutate(
    Sr_bin = case_when(
      `87Sr/86Sr` > Sr.upper                               ~ "> 0.7101",
      `87Sr/86Sr` > jump.upper & `87Sr/86Sr` <= Sr.upper   ~ "0.7101 - 0.7096",
      `87Sr/86Sr` > jump.lower & `87Sr/86Sr` <= jump.upper ~ "0.7095 - 0.7086",
      `87Sr/86Sr` >= Sr.lower & `87Sr/86Sr` <= jump.lower  ~ "0.7086 - 0.7082",
      `87Sr/86Sr` < Sr.lower                               ~ "< 0.7082"
    ),
    Sr_bin = factor(
      Sr_bin,
      levels = c("< 0.7082",
                 "0.7086 - 0.7082",
                 "0.7095 - 0.7086",
                 "0.7101 - 0.7096",
                 "> 0.7101")
    )
  )

phaleron_Sr_tooth_one <- phaleron_Sr_tooth_one %>%
  mutate(SD2.local = case_when(
    `87Sr/86Sr` > Sr.upper ~ "non-local",
    `87Sr/86Sr` < Sr.lower ~ "non-local",
    TRUE ~ "local"
  )) %>%
  mutate(jump.local = case_when(
    `87Sr/86Sr` > jump.upper ~ "probable local",
    `87Sr/86Sr` < jump.lower ~ "probable local",
    TRUE ~ "local"
  )) %>%
  mutate(
    local = case_when(
      SD2.local == "non-local" ~ "non-local",
      jump.local == "probable local" ~ "probable local",
      TRUE ~ "local"
    ),
    local = factor(local, levels = c(
      "local",
      "probable local",
      "non-local"
    ))
  ) %>%
  select(c(
    burial.number,
    `87Sr/86Sr`, 
    local,
    everything())) %>% 
  mutate() %>%
  mutate(
    Sr_bin = case_when(
      `87Sr/86Sr` > Sr.upper                               ~ "> 0.7101",
      `87Sr/86Sr` > jump.upper & `87Sr/86Sr` <= Sr.upper   ~ "0.7101 - 0.7096",
      `87Sr/86Sr` > jump.lower & `87Sr/86Sr` <= jump.upper ~ "0.7095 - 0.7086",
      `87Sr/86Sr` >= Sr.lower & `87Sr/86Sr` <= jump.lower  ~ "0.7086 - 0.7082",
      `87Sr/86Sr` < Sr.lower                               ~ "< 0.7082"
    ),
    Sr_bin = factor(
      Sr_bin,
      levels = c("< 0.7082",
                 "0.7086 - 0.7082",
                 "0.7095 - 0.7086",
                 "0.7101 - 0.7096",
                 "> 0.7101")
    )
  )

#these are the colors for the rest of the plots
Sr_colors <- c(
  "< 0.7082"          = "#000004FF",
  "0.7086 - 0.7082"   = "#56106EFF",
  "0.7095 - 0.7086"   = "#BB3754FF",
  "0.7101 - 0.7096"   = "#F98C0AFF",
  "> 0.7101"          = "#FCFFA4FF"
)

##Numbers of Locals and Non-locals
Sr_summary_locality <- phaleron_Sr_tooth %>%
  group_by(local) %>%
  summarise(
    count = n(),
    mean_Sr = mean(`87Sr/86Sr`, na.rm = TRUE),  
    sd_Sr = sd(`87Sr/86Sr`, na.rm = TRUE),      
    .groups = "drop_last"
  ) %>%
  mutate(percent = count / sum(count) * 100) %>%
  select(local, mean_Sr, sd_Sr, count, percent)
#there are 12 probable locals and 7 definite non-local TEETH
#there are 173 local TEETH

Sr_summary_locality_individual <- phaleron_Sr_tooth_one %>%
  group_by(local) %>%
  summarise(
    count = n(),
    mean_Sr = mean(`87Sr/86Sr`, na.rm = TRUE),  
    sd_Sr = sd(`87Sr/86Sr`, na.rm = TRUE),      
    .groups = "drop_last"
  ) %>%
  mutate(percent = count / sum(count) * 100) %>%
  select(local, mean_Sr, sd_Sr, count, percent)
##there are 12 probable locals and 6 definite non-local INDIVIDUALS
local_percent <- Sr_summary_locality_individual %>%
  filter(local == "local") %>%
  pull(percent) # 88.96%
perc_non_locals <- 100 - local_percent # 11.04%


phaleron_Sr_tooth_one %>%
  filter(grave.type.grouped == "Esplanada") %>%
  summarise(n_burials = n_distinct(burial.number))



non_local_list <- phaleron_Sr_tooth %>%
  filter(local %in% c("non-local", "probable local"))

#need to add IV_555's other "local" tooth
IV_555 <- phaleron_Sr_tooth %>%
  filter(burial.number == "IV_555")

non_local_list <- non_local_list %>%
  full_join(IV_555)


  ##Visualization####
Sr.non.locals.Phaleron <- ggplot(non_local_list, aes(
  x = burial.number, 
  y = `87Sr/86Sr`
)) +
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = jump.lower, ymax = jump.upper,
  #alpha = 0.2, fill = "gray50") +
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = Sr.lower, ymax = Sr.upper,
  #alpha = 0.2, fill = "lightgray") +
  #instead I'm going to annotate the faunal baseline from Frank et al 2021 and Prevedorou 2015
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = Frank.lower, ymax = Frank.upper,
           alpha = 0.2, fill = "gray50") +
  geom_line(aes(group = burial.number), color = "black", linewidth = 0.7) +
  geom_point(
    aes(fill = Sr_bin),
    color = "black",
    shape = 21,
    size = 5,
    stroke = 0.7
  ) +
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(
    values = Sr_colors,
    name = "87Sr/86Sr Bin",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = expression("Non-local and Probable Local " * {}^87*Sr/{}^86*Sr * " Values for Phaleron"),
    x = "Burial Number",
    y = expression({}^87*Sr/{}^86*Sr)
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.0001)) +
  coord_cartesian(ylim = c(Sr.min, Sr.max)) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

Sr.non.locals.Phaleron



################################################################################
##Inter-individual Variation####

  #teeth only####
##from the london atlas:
##I1 enamel 0.5 - 6, early
##I2 enamel 0.5 - 6, early
## C enamel 0.5 - 6, early
##P2 enamel 2.5 - 8, middle
##M1 enamel 0-2.8, early
##M2 enamel 2 - 8, middle
##M3 enamel 8.5-14, late
phaleron_Sr_inter_tooth <- phaleron_Sr_tooth %>%
  group_by(burial.number) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(tooth.age = factor(
    case_when(
      `Tooth Category` %in% c("M1", "I1", "I2", "C") ~ "Early Forming Enamel", 
      `Tooth Category` %in% c("P2", "M2") ~ "Middle Forming Enamel",
      `Tooth Category` %in% c("M3", "C")  ~ "Late Forming Enamel",
      TRUE ~ NA_character_
    ),
    levels = c("Early Forming Enamel", "Middle Forming Enamel", "Late Forming Enamel")
  ))

#how many individuals with more than one tooth are there? 
##Phaleron = 28
phaleron_Sr_inter_tooth %>%
  summarise(n_burials = n_distinct(burial.number))



Sr.inter.Phaleron.tooth <- ggplot(phaleron_Sr_inter_tooth, aes(
  x = burial.number, 
  y = `87Sr/86Sr`,
  shape = tooth.age
)) +
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = jump.lower, ymax = jump.upper,
  # alpha = 0.2, fill = "gray50") +
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = Sr.lower, ymax = Sr.upper,
  # alpha = 0.2, fill = "lightgray") +
  geom_line(aes(group = burial.number), color = "black", linewidth = 0.7) +
  geom_point(
    aes(fill = Sr_bin, 
        shape = tooth.age),
    color = "black",
    size = 5,
    stroke = 0.7
  ) +
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(
    values = Sr_colors,
    #drop = FALSE,
    name = "87Sr/86Sr Bin",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = expression(paste({}^87, "Sr/", {}^86, "Sr Values for Phaleron")),
    x = "Burial Number",
    y = expression({}^87*Sr/{}^86*Sr),
    shape = "Age of Enamel Sample"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.0001)) +
  guides(
    fill = guide_legend(
      override.aes = list(
        shape = 21,  # filled circle
        size = 5,
        color = "black"
      ),
      reverse = TRUE
    ),
    shape = guide_legend(order = 1)
  ) +
  coord_cartesian(ylim = c(Sr.min, Sr.max)) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

Sr.inter.Phaleron.tooth




################################################################################
##Esplanada####
Esplanada_one <- phaleron_Sr_tooth_one %>%
  filter(sector == "XI") %>%
  filter(burial.number != "XI_1285")
Esplanada  <- phaleron_Sr_tooth %>%
  filter(sector == "XI")
XI_1285 <- Esplanada %>% ##XI_1285's earliest forming tooth is prob local but
  # the later forming tooth is non-local and I want 
  # it to be counted as a non-local
  filter(burial.number == "XI_1285")
XI_1285_non_local <- XI_1285 %>%
  filter(local == "non-local")
Esplanada_one <- Esplanada_one %>%
  full_join(XI_1285_non_local)
Esplanada_one <- Esplanada_one %>%
  arrange(burial.number) %>%
  arrange(grave.type) %>%
  select(burial.number, Sr_bin, grave.type, everything())

Esplanada %>% 
  summarise(n_burials = n_distinct(burial.number)) ##there are 66 Esplanada indiviudals with 
##at least one tooth (some of the esplanada have more than one tooth. some are only represented
##by a bone)

##are there any differences based on row??

unique(Esplanada_one$grave.type)

Esplanada.ks <- kruskal.test(`87Sr/86Sr` ~ grave.type, data = Esplanada_one)
#no significance, Kruskal-Wallis chi-squared = 1.177, df = 2, p-value = 0.5552

Esplanada.ks$statistic #1.14

Esplanada_one_summary <- Esplanada_one %>%
  summarise(
    count = n(),
    mean_Sr = mean(`87Sr/86Sr`, na.rm = TRUE),  
    sd_Sr = sd(`87Sr/86Sr`, na.rm = TRUE),      
    .groups = "drop_last"
  ) %>%
  mutate(percent = count / sum(count) * 100) %>%
  select(mean_Sr, sd_Sr, count, percent)

Esplanada_local_summary <- Esplanada_one %>%
  group_by(local) %>%
  summarise(
    count = n(),
    mean_Sr = mean(`87Sr/86Sr`, na.rm = TRUE),  
    sd_Sr = sd(`87Sr/86Sr`, na.rm = TRUE),      
    .groups = "drop_last"
  ) %>%
  mutate(percent = count / sum(count) * 100) %>%
  select(local, mean_Sr, sd_Sr, count, percent)
  ##Comparison between Esplanada and everyone else####
phaleron_Sr_tooth_one <- phaleron_Sr_tooth_one %>%
  filter(burial.number != "XI_1285") %>%
  full_join(XI_1285_non_local)

phaleron_Sr_tooth_one <- phaleron_Sr_tooth_one %>%
  mutate(
    Esplanada = case_when(
      grave.type %in% c("Esplanada Row 1",
                        "Esplanada Row 2",
                        "Esplanada Row 3") ~ grave.type,
      TRUE ~ "General Burial Population"
    ),
    Esplanada.cond = if_else(
      Esplanada == "General Burial Population",
      "General Burial Population",
      "Esplanada"
    )
  ) %>%
  mutate(Esplanada = factor(
    Esplanada, levels = c(
      "General Burial Population",
      "Esplanada Row 1",
      "Esplanada Row 2",
      "Esplanada Row 3"
    )
  )) %>%
  mutate(Esplanada.cond = factor(
    Esplanada.cond, levels = c(
      "General Burial Population",
      "Esplanada"
    )
  ))



Esplanada.vs.ks <- kruskal.test(`87Sr/86Sr` ~ Esplanada, data = phaleron_Sr_tooth_one)
#not significant; Kruskal-Wallis chi-squared = 1.1267, df = 3, p-value = 0.7706

Esplanada.all.ks <- kruskal.test(`87Sr/86Sr` ~ Esplanada.cond, data = phaleron_Sr_tooth_one)
#not significant; Kruskal-Wallis chi-squared = 0.12362, df = 1, p-value = 0.7251

phaleron_Sr_tooth_one %>% 
  filter(Esplanada.cond == "General Burial Population") %>%
  summarise(n_burials = n_distinct(burial.number))
#there are 97 individuals included in the "General Burial Population"
phaleron_Sr_tooth_one %>% 
  filter(Esplanada.cond == "Esplanada") %>%
  summarise(n_burials = n_distinct(burial.number))
#there are 66 individuals from the Esplanada

  ##Visualization####
sample.size = phaleron_Sr_tooth_one %>%
  group_by(Esplanada.cond) %>%
  summarize(num = n())

phaleron.sr.esplanada <-  phaleron_Sr_tooth_one %>%
  left_join(sample.size) %>%
  mutate(myaxis = paste0(Esplanada.cond, "\n", "n=", num)) %>%
  ggplot(
    aes(
      x = myaxis, 
      y = `87Sr/86Sr`)) +
  geom_violin(width = 0.5, color = "grey50", fill = NA) +
  geom_boxplot(width = 0.1, color = "grey50", alpha = 0.2) +
  geom_point(aes(
    y = `87Sr/86Sr`,
    fill = Sr_bin),
    alpha = 0.7,
    shape = 21,
    color = "black",
    size = 4,
    position = position_jitter(width = 0.08, height = 0)) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = 1,
                       name = expression(paste({}^{87}, "Sr/", {}^{86}, "Sr")),
                       guide = guide_legend(reverse = TRUE)) +
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = jump.lower, ymax = jump.upper,
  #  alpha = 0.1, fill = "gray50") +
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = Sr.lower, ymax = Sr.upper,
  # alpha = 0.1, fill = "lightgray") +
  # geom_hline(yintercept = jump.upper, linetype = "dotted", color = "lightgray", linewidth = 1.5) +
  # geom_hline(yintercept = jump.lower, linetype = "dotted", color = "lightgray", linewidth = 1.5) +
  # geom_hline(yintercept = Sr.upper, linetype = "dotted", color = "lightgray", linewidth = 1.5) +
  # geom_hline(yintercept = Sr.lower, linetype = "dotted", color = "lightgray", linewidth = 1.5) +
  #annotate(
  # "text",
  # x = 0, 
  # y = Phaleron_tooth_mean,          
  # label = "local range",
  # hjust = -0.05, 
  # vjust = 0, 
  # size = 6, color = "black"
  #  ) +
  # annotate(
  #   "text",
  #  x = 0,
  #  y = Sr.lower,            
  #  label = "probably local range",
  #  hjust = -0.05, 
  #  vjust = -0.5, 
  #  size = 6, color = "black"
  # ) +
  labs(
    x = "",
    y = expression({}^87*Sr/{}^86*Sr),
    title = expression(paste({}^87, "Sr/", {}^86, "Sr Values of the Esplanada and General Burial Population at Phaleron")),
  )  +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "solid"),
    panel.grid.major.y = element_line(linetype = "solid"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    #legend.position = "none"
  ) +
  coord_cartesian(ylim = c(Sr.min, Sr.max))

phaleron.sr.esplanada


################################################################################
##CREATING THE GOOD MAP####

phaleron_Sr_map <- phaleron_Sr_tooth_one %>%
  left_join(Phaleron_map, by = "burial.number")

Phaleron.Sr.map <- ggplot() +
  
  # Layer 1: all burials without Sr values (background)
  geom_point(
    data = Phaleron_map,
    aes(x = Longitude, 
        y = Latitude),
    color = "gray40",
    alpha = 0.5,
    size = 1,
    shape = 16) +
  
  # Layer 2: burials with Sr values (locals + non-locals)
  geom_point(
    data = phaleron_Sr_map,
    aes(x = Longitude, 
        y = Latitude, 
        fill = Sr_bin),
    color = "black",   # outline
    shape = 21,
    size = 3,
    alpha = 0.9
  ) +
  
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(
    values = Sr_colors,
    name = "87Sr/86Sr Bin",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(title = "Location of Burials",
       x = "Longitude",
       y = "Latitude") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.line = element_line(colour = "grey50"),
    axis.text = element_blank(),           # keep axis labels removed
    panel.grid = element_blank(),          # removes all grid lines
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    #legend.position = c(0.02, 0.02),       # bottom-left corner, relative coordinates
    #legend.justification = c(0, 0),       # aligns legend box to bottom-left
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


Phaleron.Sr.map

################################################################################
