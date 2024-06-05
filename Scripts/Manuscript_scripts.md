## Libraries

library(ggVennDiagram)
library(VennDiagram)
library(ggplot2)
library(ggpubr)
library(ggvenn)
library(dplyr)
library(patchwork)

## Input data
# input data was stored at the directory 'Data_repository'

df_metaphlan_all <- read.csv(r'(LRT_microbiomes_critical_illness\Data_repository\Main_figures\microbiome_species-top_prevalent.csv)')
df_metaphlan_all[df_metaphlan_all$SamplingSite=='HospitalA',]$Name
df_input_list <- list(
  "Hospital A" = df_metaphlan_all[df_metaphlan_all$SamplingSite=='Hospital-A',]$taxonomy,
  "Hospital B" = df_metaphlan_all[df_metaphlan_all$SamplingSite=='Hospital-B',]$taxonomy,
  "Hospital C" = df_metaphlan_all[df_metaphlan_all$SamplingSite=='Hospital-C',]$taxonomy
)


```{r Fig 1b}

venn.diagram(
  x = df_input_list,
  category.names = c("Hospital A" , "Hospital B" , "Hospital C"),
  filename = r'()', # fill the filepath where you want to save the plot
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 1500 , 
  width = 1500 , 
  resolution = 900,
  compression = "lzw",
  
  # Circles
  lwd = 0.7,
  # col=c("#440154ff", '#509fd2', '#5da94e'),
  col=c("#d93526", "#f7a902", "#1a6196"),
  # fill = c(alpha("#440154ff",0.4), alpha('#509fd2',0.4), alpha('#5da94e',0.4)),
  fill = c(alpha("#d93526",0.3), alpha('#f7a902',0.3), alpha('#1a6196',0.3)),
  cex = 0.7,
  fontfamily = "sans",
  cat.cex = 0.6,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  # cat.col = c("#440154ff", '#509fd2', '#5da94e'),
  cat.col = c("#d93526", "#f7a902", "#1a6196"),
  rotation = 1
)

```

```{r Fig 1c}

df_prevalent <- read.csv(r"(LRT_microbiomes_critical_illness\Data_repository\Main_figures\microbiome_species-top_prevalent.csv)")
df_order <- read.csv(r"(LRT_microbiomes_critical_illness\Data_repository\Main_figures\microbiome_species-top_prevalent-sorted.csv)")
df_sorted_desc <- arrange(df_order, desc(prevalency_percentage_all))

order_species <- df_sorted_desc[1:10,'taxonomy']
df_prevalent$taxonomy <- factor(df_prevalent$taxonomy, levels = df_sorted_desc[1:10,'taxonomy'])

#transform species name into human-readable format
df_prevalent$taxonomy <- gsub("^s__", "", df_prevalent$taxonomy) # Remove the prefix
df_prevalent$taxonomy <- gsub("_", " ", df_prevalent$taxonomy)
plot_df <- ggplot(na.omit(df_prevalent), aes(fill=SamplingSite, x=reorder(taxonomy,prevalency_percentage,sum,decreasing=T), y=prevalency_percentage))+  #'x=reorder(species,prevalency_percentage,sum,decreasing=T)' --> order stacked barplot with sum of values
  geom_bar(position = "dodge",stat = "identity",) +
  scale_fill_manual(values = c('Hospital-A'='#d93526','Hospital-B'='#f7a902','Hospital-C'='#1a6196'))+
  theme_pubr(x.text.angle = 35)

plot_df
ggpar(plot_df,
      ylab = "Percentage of samples with\ndetected species (%)",
      xlab = "Species",
      font.xtickslab=c(18),
      font.ytickslab=c(18),
      font.x = c(18,"bold"),
      font.y = c(18,"bold"),
      font.legend = c(18,'bold'),
      x.text.angle =50) + theme_pubr(base_size = 24,x.text.angle =35,legend = "none",margin = FALSE)

```

```{r Fig 1d}

```

```{r Fig 1e}

```

```{r Fig S1a}

#most prevalent species for hospitalA
df_species_hospitalA <- read.csv(r'(LRT_microbiomes_critical_illness\Data_repository\Supplementary_figures\microbiome_species-top_prevalent.csv\microbiome_species-statistics_count-Hospital-A.csv)',header = TRUE) # save in the same file 'microbiome_species-statistics_count-Hospitals.txt'
df_species_hospitalA$taxonomy <- gsub("^s__", "", df_species_hospitalA$taxonomy) # Remove the prefix
df_species_hospitalA$taxonomy <- gsub("_", " ", df_species_hospitalA$taxonomy)

plot_species_hospitalA <- ggbarplot(df_species_hospitalA[1:5,],x = "taxonomy", y = "prevalency_percentage",orientation = "vertical",alpha=1,
                                    fill = "#d93526", color = "#d93526",sort.val = 'desc',ggtheme = theme_bw(base_size = 18)) + 
  labs(x=NULL,y='Percentage of\ndetected samples (%)') +  theme(aspect.ratio =1/1.5 )  #+ #+ ylim(0,15) 
  #scale_y_continuous(breaks = c(0,2,4,6,8,10))

plot_species_hospitalA <-ggpar(plot_species_hospitalA,x.text.angle = 55)

#-------------------------------------------------------------------------------------------------------------------------------

#most prevalent species for hospitalB
df_species_hospitalB <- read.csv(r'(LRT_microbiomes_critical_illness\Data_repository\Supplementary_figures\microbiome_species-top_prevalent.csv\microbiome_species-statistics_count-Hospital-B.csv)',header = TRUE # save in the same file 'microbiome_species-statistics_count-Hospitals.txt'
df_species_hospitalB$taxonomy <- gsub("^s__", "", df_species_hospitalB$taxonomy) # Remove the prefix
df_species_hospitalB$taxonomy <- gsub("_", " ", df_species_hospitalB$taxonomy)

plot_species_hospitalB <- ggbarplot(df_species_hospitalB[1:5,],x = "taxonomy", y = "prevalency_percentage",orientation = "vertical",alpha=1,
                                    fill = "#f7a902", color = "#f7a902",sort.val = 'desc',ggtheme = theme_bw(base_size = 18)) + 
  labs(x='Species')  +  theme(aspect.ratio =1/1.5,axis.title.y = element_blank())

plot_species_hospitalB <- ggpar(plot_species_hospitalB,x.text.angle = 55)

#-------------------------------------------------------------------------------------------------------------------------------

#most prevalent species for hospitalC
df_species_hospitalC <- read.csv(r'(LRT_microbiomes_critical_illness\Data_repository\Supplementary_figures\microbiome_species-top_prevalent.csv\microbiome_species-statistics_count-Hospital-C.csv)',header = TRUE) # save in the same file 'microbiome_species-statistics_count-Hospitals.txt'
df_species_hospitalC$taxonomy <- gsub("^s__", "", df_species_hospitalC$taxonomy) # Remove the prefix
df_species_hospitalC$taxonomy <- gsub("_", " ", df_species_hospitalC$taxonomy)
df_species_hospitalC <- df_species_hospitalC[-5,]

plot_species_hospitalC <- ggbarplot(df_species_hospitalC[1:5,],x = "taxonomy", y = "prevalency_percentage",orientation = "vertical",alpha=1,
                                    fill = "#1a6196", color = "#1a6196",sort.val = 'desc',ggtheme = theme_bw(base_size = 18)) + 
  labs(x=NULL)  +  theme(aspect.ratio =1/1.5,axis.title.y = element_blank() )

plot_species_hospitalC <-ggpar(plot_species_hospitalC,x.text.angle = 55)

plot_species_hospitalA|plot_species_hospitalB|plot_species_hospitalC

```

```{r Fig S1b}

df_too_low <- read.csv((r'(LRT_microbiomes_critical_illness\Data_repository\Supplementary_figures\microbiome_species-top_prevalent.csv\head-to-head_comparisons-DNA_concentration_too_low.csv)'))
df_too_low$Percentage_Failed <- df_too_low$Percentage_Failed *100
df_too_low$Percentage_successful <- 100 - df_too_low$Percentage_Failed
plot <- ggbarplot(df_too_low, "Method", "Percentage_successful",
                  color = '#1f78b4',fill='#1f78b4',palette = "#1f78b4",
                  label = F,
                  position = position_dodge(0.9)) + 
  scale_x_discrete(labels= c('CMEM','Power Water','AllPrep DNA')) + 
  labs(x=NULL,y='Percentage of samples with detectable\nDNA after extraction (%)') + theme_bw(base_size = 27)
ggpar(plot,legend = 'bottom',x.text.angle = 30)

```

```{r Fig S1c}
df_concentration <- read.csv(r'(LRT_microbiomes_critical_illness\Data_repository\Supplementary_figures\head-to-head_comparisons-DNA_concentration.csv)')
df_concentration$Concentration  <- log10(df_concentration$Concentration + 1)
df_concentration$Method <- factor(df_concentration$Method,levels =c('CMEM','Power Water','AllPrep DNA') )
my_comparisons <- list(c('CMEM','Power Water'),c('CMEM','AllPrep DNA'),c('Power Water','AllPrep DNA'))
p_concentration <- ggboxplot(df_concentration, "Method", "Concentration",
          color = "Method", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter",add.params = list(size = 1.5, alpha = 0.5))  +
  stat_boxplot(geom = "errorbar",width=0.05,color=c("#00AFBB", "#E7B800", "#FC4E07"))  +
  stat_compare_means(comparisons = my_comparisons,label = "p.label",size=8,vjust=0) +
  scale_x_discrete(labels= c('CMEM','Power Water','AllPrep DNA')) + 
  labs(x=NULL,y='DNA concentration (log10, ng/ul)',) + #ylim(-1,10) +
  theme_bw(base_size = 24)

```

```{r Fig S1d}
