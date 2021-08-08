# -------------------------------------------------

cntry <- games %>% 
  arrange(cntry, year) %>% 
  group_by(cntry, year) %>%
  summarize(nwins = sum(victory, na.rm = T),
            ndraws = sum(draw, na.rm = T),
            ngames = n()) %>% 
  mutate(points = (nwins*3) + (ndraws*1)) %>% 
  mutate(ppg = points/ngames) %>% 
  ungroup()

# cntry <- cntry %>% 
#   mutate(max_games = max(ngames))
# 
# cntry <- cntry %>% 
#   mutate(weight = ngames/max_games)
# 
# cntry <- cntry %>% 
#   mutate(ppg_weighted = ppg * weight)

# cntry %>% filter(ngames == 33) %>% head()
# tt <- cntry %>% filter(ngames > 20)

cntry <- cntry %>% 
  filter(cntry == "Sweden" | cntry == "Denmark" | cntry == "Norfway")

cntry <- cntry %>% 
  group_by(cntry) %>% 
  mutate(roll_ppg = rollapply(ppg, 5, mean, fill = NA, align = 'right'))


ggplot(cntry, aes(x = year, y = roll_ppg)) +
  geom_line(aes(color = cntry)) +
  #scale_color_manual(values = c("red4", "royalblue4", "goldenrod2")) +
  scale_color_manual(values = c("red4", "goldenrod2")) +
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, .5),
                     labels = seq(0, 3, .5)) +
  #geom_vline(xintercept = 2016, linetype = "dashed") +
  theme_ipsum()


p <- ggplot(cntry, aes(x = year, y = roll_ppg)) +
  geom_line(aes(color = cntry)) +
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, .5),
                     labels = seq(0, 3, .5)) +
  theme_ipsum() +
  transition_reveal(year) +
  ease_aes('linear')

gif <- animate(plot = last_plot(), end_pause = 30, nframes = 150)
anim_save(gif, file = "jun04_gif_a.gif")
