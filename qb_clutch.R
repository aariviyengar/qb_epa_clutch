library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
pbp <- load_pbp(2013:2023)|>
  filter(pass==1)|>
  filter(!is.na(epa))|>
  group_by(id)|>
  summarize(name = first(name), passes = n(), epa_pass=mean(epa), team=first(posteam))|>
  filter(passes>=320)
clutch_pbp <- load_pbp(2013:2023)|>
  filter(pass==1)|>
  filter(!is.na(epa))|>
  filter(game_seconds_remaining <= 300)|>
  filter(wp>=25|wp<=75)|>
  group_by(id)|>
  summarize(name = first(name), clutch_passes = n(), clutch_epa_pass=mean(epa))|>
  filter(clutch_passes>=200)
merged_data <- left_join(pbp, clutch_pbp, by=c("id","name"))|>
  filter(!is.na(clutch_passes))
merged_data <- left_join(merged_data,teams_colors_logos,by=c("team"="team_abbr"))
merged_data|>
  ggplot(aes(x=epa_pass,y=clutch_epa_pass))+
  geom_point(aes(fill=team_color,color=team_color2),shape=21,alpha=0.9)+
  scale_color_identity(aesthetics = c("fill","color"))+
  ggrepel::geom_text_repel(aes(label=paste(name)),max.overlaps = 13)+
  theme_bw()+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  labs(x = "QB EPA Per Dropback",
       y= "QB Clutch EPA Per Dropback",
       title = "QB EPA Per Dropback and Clutch EPA Per Dropback, 2013-2023",
       subtitle = "Minimum of 320 passes and 200 clutch passes")+
  theme(panel.grid.major.y=element_blank(),
        plot.title = element_text(size=22,hjust=0.5,face="bold"),
        plot.subtitle = element_text(size=16,hjust=0.5))
ggsave('EPA_Pass_Clutch_uncaptioned.png',width=14,height=10,dpi="retina")  
merged_data$epa_difference <- merged_data$clutch_epa_pass-merged_data$epa_pass
merged_data <- merged_data|>
  arrange(-epa_difference)|>
  mutate(rank=row_number())
clutch_qbs <- merged_data|>
  slice_max(epa_difference,n=8)
unclutch_qbs <- merged_data|>
  slice_min(epa_difference,n=8)
clutch_gt <- rbind(clutch_qbs,unclutch_qbs)|>
  arrange(-epa_difference)|>
  dplyr::select(rank, name, team_wordmark, epa_pass, clutch_epa_pass, epa_difference)|>
  mutate(epa_pass = round(epa_pass,2),
         clutch_epa_pass = round(clutch_epa_pass,2),
         epa_difference=round(epa_difference,2))|>
  gt()|>
  cols_align(align="center")|>
  gt_img_rows(team_wordmark)|>
  cols_label(rank = "Rank",
             name = "Quarterback",
             epa_pass = "EPA Per Dropback",
             clutch_epa_pass = "Clutch EPA Per Dropback",
             epa_difference = "Clutch EPA Difference",
             team_wordmark = "")|>
  gt_theme_538()|>
  gt_hulk_col_numeric(columns=epa_difference)
gtsave(clutch_gt, "clutch_gt.png")
  