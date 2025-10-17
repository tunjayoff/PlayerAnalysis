########## GEREKLİ KÜTÜPHANELERİN YÜKLENMESİ VE TÜRKÇE KARAKTER İÇİN AYARLAMALAR ##########
setwd("C:/Users/tuncay/Desktop/R") ### Çalışma dizinini ayarladık

library(tidyverse) # Temel paketler için tidyverse kullandık
library(corrplot)  # Korelasyon matrisi için
library(caret)     # Modelleme için
library(class)     # KNN için

Sys.setlocale("LC_ALL", "Turkish") # Türkçe karakter desteği için

########## VERİLERİN OKUNMASI VE DÜZENLENME İŞLEMLERİ ##########
# Veri setlerini okuyoruz ve Türkçe karakter desteği için encoding ayarını yapıyoruz
goalkeepers <- read.csv("goalkeepers_stats.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
players     <- read.csv("player_stats.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
View(goalkeepers)
View(players)

########## POZİSYONLARIN YENİDEN SINIFLANDIRILMASI ##########
# Pozisyonları yeniden sınıflandırma
players <- players %>%
  mutate(position_simplified = case_when(
      position %in% c("DF", "DF.MF") ~ "Defans",
      position %in% c("MF", "MF.DF", "MF.FW") ~ "Orta Saha",
      position %in% c("FW", "FW.MF") ~ "Forvet",
    )
  )

# poziyonlara göre oyuncu sayıları
table(players$position_simplified)

########## YAŞ GRUPLARINA GÖRE OYUNCULARIN SINIFLANDIRILMASI ##########
# Oyuncuları yaş gruplarına ayırıyoruz
players$age_group <- cut(
  players$age,
  breaks = c(0, 21, 25, 29, 33, 100),
  labels = c("21 Yaş ve Altı", "22-25 Yaş", "26-29 Yaş", "30-33 Yaş", "33 Yaş Üstü")
)

########## YAŞ VE POZİSYON BAZLI İSTATİSTİKLERİN HESAPLANMASI ##########
# Yeniden sınıflandırdığımız pozisyonları kullanarak yaş analizi
age_analysis <- players %>%
  group_by(age_group, position_simplified) %>%
  summarize(
    oyuncu_sayisi    = n(),
    ortalama_dakika  = mean(minutes_played, na.rm = TRUE),
    gol_90dk         = sum(goals, na.rm = TRUE) * 90 / sum(minutes_played, na.rm = TRUE),
    asist_90dk       = sum(assists, na.rm = TRUE) * 90 / sum(minutes_played, na.rm = TRUE)
  )

# Yaş gruplarına göre pozisyon bazlı oyuncu sayıları görselleştirmesi
ggplot(age_analysis, aes(x = age_group, y = oyuncu_sayisi, fill = position_simplified)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  scale_fill_manual(values = c("Defans" = "#1f77b4", "Orta Saha" = "#ff7f0e", "Forvet" = "#2ca02c")) +
  labs(
    title = "Yaş Gruplarına ve Pozisyonlara Göre Oyuncu Sayıları",
    x     = "Yaş Grubu",
    y     = "Oyuncu Sayısı",
    fill  = "Pozisyon"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y      = element_text(size = 12),
    axis.title       = element_text(size = 14),
    legend.title     = element_text(size = 14),
    legend.text      = element_text(size = 12),
    legend.position  = "top"
  ) + geom_text(
    aes(label = oyuncu_sayisi),
    position = position_dodge(width = 0.7),
    vjust    = -0.5,
    size     = 4,
    color    = "black"
  ) + coord_cartesian(ylim = c(0, max(age_analysis$oyuncu_sayisi) * 1.1))


########## PERFORMANS METRİKLERİ KORELASYON ANALİZİ ##########
# İlgili performans metriklerini seçiyoruz
performance_metrics <- players %>%
  select(
    "age", "minutes_played",
    "X90_mins_complated", "points_per_match",
    "red_card", "fouls_committed",
    "fouls_drawn", "interceptions", "tackles_won",
    "own_goals", "goals", "assists", "shots", "shots_on_target",
  )

normalized_performance_metrics <- scale(performance_metrics) # Z-score normalizasyonu yaptık

# Korelasyon matrisini oluşturup görselleştiriyoruz
correlation_matrix <- cor(normalized_performance_metrics, use = "complete.obs")

# Görselleştirme
corrplot(correlation_matrix,
  method        = "color",
  type          = "full",        # Sadece alt üçgeni göstererek daha temiz bir görünüm
  order         = "original",       # Değişkenleri benzerliklerine göre sıralar
  addCoef.col   = "black",        # Korelasyon değerlerini siyah renkte ekler
  number.cex    = 0.5,            # Korelasyon değerlerinin boyutunu artırdık
  tl.col        = "black",        # Değişken isimlerinin rengini ayarladık
  title         = "Performans Metrikleri Korelasyon Matrisi",
  mar           = c(0, 0, 1, 0),  # Grafiğin üst kısmına biraz boşluk ekledik
  col           = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)
)


########## POZİSYONLARA GÖRE PERFORMANS ANALİZİ ##########
# Yeniden sınıflandırılmış pozisyonlara göre performans analizi
position_performance <- players %>%
  group_by(position_simplified) %>%
  summarize(
    ortalama_gol        = mean(goals, na.rm = TRUE),
    ortalama_asist      = mean(assists, na.rm = TRUE),
    ortalama_sut        = mean(shots, na.rm = TRUE),
    sut_isabeti         = mean(shots_on_target / shots * 100, na.rm = TRUE),
    ortalama_mudahale   = mean(tackles_won, na.rm = TRUE),
    ortalama_top_kesme  = mean(interceptions, na.rm = TRUE)
  )

########## POZİSYONLARA GÖRE GOL VE ASİST KARŞILAŞTIRMASI ##########
# Pozisyonlara göre ortalama gol ve asist grafiği
# Veri setini uzun formata dönüştürüyoruz
position_performance_long <- position_performance %>%
  pivot_longer(
    cols      = c(ortalama_gol, ortalama_asist),
    names_to  = "Metrik",
    values_to = "Ortalama"
  )

# 'Metrik' sütunundaki değerleri daha anlaşılır hale getiriyoruz
position_performance_long$Metrik <- recode(position_performance_long$Metrik,
                                           ortalama_gol = "Goller",
                                           ortalama_asist = "Asistler")

# Grafiği oluşturuyoruz
ggplot(position_performance_long, aes(x = position_simplified, y = Ortalama, fill = Metrik)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  labs(
    title = "Pozisyonlara Göre Ortalama Gol ve Asist",
    x     = "Pozisyon",
    y     = "Ortalama Değer",
    fill  = "Metrik"
  ) +
  theme_classic() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12),
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 10),
    legend.position  = "top"
  ) +
  scale_fill_manual(values = c("Goller" = "steelblue", "Asistler" = "darkred")) +
  geom_text(
    aes(label = round(Ortalama, 2)),
    position = position_dodge(width = 0.7),
    vjust    = -0.5,
    size     = 4
  ) +
  coord_cartesian(ylim = c(0, max(position_performance_long$Ortalama) * 1.1))


########## MAÇ BAŞINA İSTATİSTİKLERİN ANALİZİ ##########

match_stats <- players %>%
  mutate(
    mac_basi_gol     = goals / match_played,
    mac_basi_asist   = assists / match_played,
    mac_basi_dakika  = minutes_played / match_played,
    mac_basi_sut     = shots / match_played
  ) %>%
  group_by(position_simplified) %>%
  summarize(
    ort_mac_basi_gol     = mean(mac_basi_gol, na.rm = TRUE),
    ort_mac_basi_asist   = mean(mac_basi_asist, na.rm = TRUE),
    ort_mac_basi_dakika  = mean(mac_basi_dakika, na.rm = TRUE),
    ort_mac_basi_sut     = mean(mac_basi_sut, na.rm = TRUE)
  )

########## POZİSYONLARA GÖRE MAÇ BAŞINA DAKİKA GÖRSELLEŞTİRMESİ ##########
ggplot(match_stats, aes(x = position_simplified, y = ort_mac_basi_dakika, fill = position_simplified)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = round(ort_mac_basi_dakika, 1)), vjust = -0.5, size = 4) +
  labs(
    title = "Pozisyonlara Göre Maç Başına Ortalama Dakika",
    x     = "Pozisyon",
    y     = "Ortalama Dakika"
  ) +
  scale_fill_manual(values = c("Defans" = "#66c2a5", "Orta Saha" = "#fc8d62", "Forvet" = "#8da0cb")) +
  theme_classic() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x      = element_text(size = 12),
    axis.text.y      = element_text(size = 12),
    axis.title       = element_text(size = 14),
    legend.position  = "none"
  ) +
  coord_cartesian(ylim = c(0, max(match_stats$ort_mac_basi_dakika) * 1.1))


########## MEVCUT VERİLERİN DETAYLI ANALİZİ ##########
detailed_position_stats <- players %>%
  group_by(position_simplified) %>%
  summarize(
    toplam_oyuncu          = n(),
    ortalama_yas           = mean(age, na.rm = TRUE),
    toplam_gol             = sum(goals, na.rm = TRUE),
    toplam_asist           = sum(assists, na.rm = TRUE),
    ortalama_dakika        = mean(minutes_played, na.rm = TRUE),
    sut_konversiyon_orani  = sum(goals, na.rm = TRUE) / sum(shots, na.rm = TRUE) * 100,
    isabetli_sut_orani     = sum(shots_on_target, na.rm = TRUE) / sum(shots, na.rm = TRUE) * 100
  )

# Şut konversiyon oranları görselleştirmesi
ggplot(detailed_position_stats, aes(x = position_simplified, y = sut_konversiyon_orani, fill = position_simplified)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = paste0(round(sut_konversiyon_orani, 1), "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Pozisyonlara Göre Şut Konversiyon Oranları",
    x     = "Pozisyon",
    y     = "Konversiyon Oranı (%)"
  ) +
  scale_fill_manual(values = c("Defans" = "#66c2a5", "Orta Saha" = "#fc8d62", "Forvet" = "#8da0cb")) +
  theme_classic() +
  theme(
    plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title      = element_text(size = 14),
    axis.text.x     = element_text(size = 12),
    axis.text.y     = element_text(size = 12),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, max(detailed_position_stats$sut_konversiyon_orani) * 1.1))


########## YAŞ GRUPLARINA GÖRE PERFORMANS ANALİZİ ##########
age_performance <- players %>%
  group_by(age_group) %>%
  summarize(
    ortalama_gol            = mean(goals, na.rm = TRUE),
    ortalama_asist          = mean(assists, na.rm = TRUE),
    ortalama_dakika         = mean(minutes_played, na.rm = TRUE),
    mac_basi_performans     = sum(goals + assists, na.rm = TRUE) / sum(match_played, na.rm = TRUE)
  )

# Yaş gruplarına göre performans görselleştirmesi
# age_group sıralaması için mantıksal bir sıra belirliyoruz
age_performance$age_group <- factor(
  age_performance$age_group,
  levels = c("21 Yaş ve Altı", "22-25 Yaş", "26-29 Yaş", "30-33 Yaş", "33 Yaş Üstü")
)

ggplot(age_performance, aes(x = age_group, y = mac_basi_performans, group = 1)) +
  geom_line(color = "#2c7fb8", size = 1.5) +
  geom_point(color = "#2c7fb8", size = 4) +
  geom_text(aes(label = round(mac_basi_performans, 2)), vjust = -1, size = 4) +
  labs(
    title = "Yaş Gruplarına Göre Maç Başına Performans",
    x     = "Yaş Grubu",
    y     = "Maç Başına Gol + Asist"
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 12),
    axis.text.x  = element_text(angle = 0),
    axis.ticks.x = element_blank()
  ) +
  ylim(0, max(age_performance$mac_basi_performans) * 1.2)

########## OYUNCULARIN MAÇ İÇİ AKTİVİTELERİNİN DETAYLI ANALİZİ ##########
# Pozisyonlara göre maç içi aktivitelerin analizi
match_activities <- players %>%
  group_by(position_simplified) %>%
  summarize(
    ortalama_faul_yapilan   = mean(fouls_committed, na.rm = TRUE),
    ortalama_faul_kazanilan = mean(fouls_drawn, na.rm = TRUE),
    ortalama_mudahale       = mean(tackles_won, na.rm = TRUE),
    ortalama_top_kesme      = mean(interceptions, na.rm = TRUE),
    mudahale_basari_orani   = sum(tackles_won, na.rm = TRUE) /
      (sum(tackles_won, na.rm = TRUE) + sum(fouls_committed, na.rm = TRUE)) * 100
  )

# Veriyi uzun forma dönüştürüyoruz
match_activities_long <- match_activities %>%
  pivot_longer(
    cols      = c(ortalama_faul_yapilan, ortalama_faul_kazanilan, ortalama_mudahale, ortalama_top_kesme),
    names_to  = "aktivite_tipi",
    values_to = "ortalama_deger"
  )

# Aktivite tiplerini daha anlaşılır hale getiriyoruz
match_activities_long$aktivite_tipi <- recode(match_activities_long$aktivite_tipi,
                                              ortalama_faul_yapilan   = "Faul Yapılan",
                                              ortalama_faul_kazanilan = "Faul Kazanılan",
                                              ortalama_mudahale       = "Başarılı Müdahale",
                                              ortalama_top_kesme      = "Top Kesme")

# Maç içi aktivitelerin görselleştirilmesi
ggplot(match_activities_long, aes(x = position_simplified, y = ortalama_deger, fill = aktivite_tipi)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_text(
    aes(label = round(ortalama_deger, 2)),
    position = position_dodge(width = 0.7),
    vjust    = -0.5,
    size     = 4
  ) +
  labs(
    title = "Pozisyonlara Göre Maç İçi Aktiviteler",
    x     = "Pozisyon",
    y     = "Ortalama Değer",
    fill  = "Aktivite Tipi"
  ) +
  scale_fill_manual(values = c(
    "Faul Yapılan"       = "#66c2a5",
    "Faul Kazanılan"     = "#fc8d62",
    "Başarılı Müdahale"  = "#8da0cb",
    "Top Kesme"          = "#e78ac3"
  )) +
  theme_classic() +
  theme(
    plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 12),
    legend.title    = element_text(size = 12),
    legend.text     = element_text(size = 10),
    legend.position = "top"
  ) +
  coord_cartesian(ylim = c(0, max(match_activities_long$ortalama_deger) * 1.1))

########## SEZON İÇİ FORM GRAFİĞİ ANALİZİ ##########
# Oyuncuların maç başına performans trendini inceleme
player_form <- players %>%
  mutate(
    verimlilik          = (goals + assists + fouls_drawn + 
                            crosses + interceptions + tackles_won + shots_on_target) / 
      match_played,
    dakika_verimlilik   = minutes_played / (match_played * 90)
  ) %>%
  arrange(desc(verimlilik)) %>%
  head(10)  # En verimli 10 oyuncu

# En verimli oyuncuların görselleştirilmesi
ggplot(player_form, aes(x = reorder(player_name, verimlilik), y = verimlilik)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.6, color = "black") +
  geom_text(aes(label = round(verimlilik, 2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "En Verimli 10 Oyuncu",
    x     = "Oyuncu",
    y     = "Verimlilik"
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 12),
    axis.text.x  = element_text(angle = 0),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

########## KALECİ VERİ SETİNİN ANALİZİ ##########
# Kalecilerin temel performans metriklerini hesaplayalım
goalkeeper_stats <- goalkeepers %>%
  mutate(
    # Kurtarış yüzdesi: Toplam kurtarışların kaleye gelen şutlara oranı
    save_percentage = (saves / shots_on_target) * 100,
    # Maç başına kurtarış: Toplam kurtarışların oynanan maç sayısına oranı
    saves_per_match = saves / match_played,
    # Clean sheet oranı: Gol yenilmeyen maçların toplam maçlara oranı
    clean_sheet_ratio = (clean_sheets / match_played) * 100,
    # Penaltı kurtarış yüzdesi: Kurtarılan penaltıların toplam penaltılara oranı
    penalty_save_ratio = ifelse(
      penalty_kicks_attempts > 0,
      (penalty_kicks_saved / penalty_kicks_attempts) * 100,
      NA
    ),
    # Yaş grupları oluşturma
    age_group = cut(
      age,
      breaks = c(15, 20, 25, 30, 35, 40),
      labels = c("15-20", "21-25", "26-30", "31-35", "36+"),
      include.lowest = TRUE
    )
  ) %>%
  # En az 5 maç oynamış kalecileri filtreleyelim
  filter(match_played >= 5) %>%
  arrange(desc(save_percentage))

# Yaş gruplarına göre kaleci performansı
ggplot(goalkeeper_stats, aes(x = age_group, y = save_percentage)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    fill = "#2c7fb8",
    color = "black",
    width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2,
    color = "black"
  ) +
  geom_jitter(
    aes(color = age_group),
    width = 0.2,
    size = 2,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    stat = "summary",
    fun = mean,
    aes(label = round(..y.., 1)),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title    = "Yaş Gruplarına Göre Kaleci Performansı",
    subtitle = "Barlar ortalama kurtarış yüzdesini, noktalar bireysel kalecileri gösterir",
    x        = "Yaş Grubu",
    y        = "Kurtarış Yüzdesi (%)"
  ) +
  theme_classic() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title    = element_text(size = 14),
    axis.text     = element_text(size = 12),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, 100))

########## KALECİLERİN DETAYLI PERFORMANS ANALİZİ ##########
# Clean sheet oranlarını inceleyelim
ggplot(goalkeeper_stats, aes(x = reorder(player_name, clean_sheet_ratio), y = clean_sheet_ratio)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  geom_text(aes(label = match_played), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title    = "Kalecilerin Clean Sheet Oranları",
    subtitle = "En az 5 maç oynamış kaleciler",
    x        = "Kaleci",
    y        = "Maç Başına Clean Sheet Oranı (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  ylim(0, max(goalkeeper_stats$clean_sheet_ratio) * 1.1)

# Kalecilerin penaltı performansını analiz edelim
# En az 2 penaltı karşılaşması olan kalecileri seçelim ve penaltı başarı oranlarını hesaplayalım
goalkeeper_penalty_stats <- goalkeeper_stats %>%
  filter(penalty_kicks_attempts >= 2) %>%  # En az 2 penaltı karşılaşması olanlar
  mutate(
    penalty_success = paste0(penalty_kicks_saved, "/", penalty_kicks_attempts),
    penalty_save_ratio = round(penalty_save_ratio, 1)  # Oranı yuvarlayalım
  ) %>%
  arrange(desc(penalty_save_ratio))  # Penaltı kurtarış oranına göre sıralayalım

# Kaleci isimlerini faktör olarak sıralayalım
goalkeeper_penalty_stats$player_name <- factor(
  goalkeeper_penalty_stats$player_name,
  levels = goalkeeper_penalty_stats$player_name
)

# Grafiği oluşturalım
ggplot(goalkeeper_penalty_stats, aes(x = player_name, y = penalty_save_ratio)) +
  geom_bar(stat = "identity", fill = "#6a51a3", width = 0.6, color = "black") +
  geom_text(aes(label = paste0(penalty_save_ratio, "% (", penalty_success, ")")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title    = "Kalecilerin Penaltı Kurtarış Oranları",
    subtitle = "En az 2 penaltı karşılaşması olan kaleciler",
    x        = "Kaleci",
    y        = "Penaltı Kurtarış Oranı (%)"
  ) +
  theme_classic() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title    = element_text(size = 14),
    axis.text     = element_text(size = 12),
    axis.ticks.y  = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# Kurtarış yüzdesi ve maç sayısı ilişkisi
ggplot(goalkeeper_stats, aes(x = match_played, y = save_percentage)) +
  geom_point(aes(size = saves_per_match), alpha = 0.6, color = "blue") +
  geom_text(
    aes(label = player_name),
    vjust         = -1,
    size          = 3,
    check_overlap = TRUE
  ) +
  labs(
    title    = "Kalecilerin Kurtarış Yüzdesi ve Maç Sayısı İlişkisi",
    subtitle = "Nokta büyüklüğü maç başına kurtarış sayısını gösterir",
    x        = "Oynanan Maç Sayısı",
    y        = "Kurtarış Yüzdesi (%)",
    size     = "Maç Başına Kurtarış"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

########## KALECİ PERFORMANSI VE TAKIM SAVUNMASI İLİŞKİSİ ANALİZİ ##########

# Öncelikle takımların savunma metriklerini hesaplayalım
team_defense_stats <- players %>%
  # Savunma oyuncularını ve defansif orta saha oyuncularını seçelim
  filter(position_simplified == "Defans") %>%
  group_by(squad) %>%
  summarize(
    # Takımın ortalama top kesme sayısı
    takim_top_kesme        = mean(interceptions, na.rm = TRUE),
    # Takımın ortalama başarılı müdahale sayısı
    takim_mudahale         = mean(tackles_won, na.rm = TRUE),
    # Savunma oyuncusu sayısı
    savunma_oyuncu_sayisi  = n(),
    # Toplam savunma dakikası
    toplam_savunma_dakika  = sum(minutes_played, na.rm = TRUE)
  )

# Kaleci ve takım savunma istatistiklerini birleştirelim
goalkeeper_defense_analysis <- goalkeeper_stats %>%
  left_join(team_defense_stats, by = "squad") %>%
  # En az 10 maç oynamış kalecileri filtreleyelim
  filter(match_played >= 10) %>%
  # Savunma etkisi skorunu hesaplayalım
  mutate(
    savunma_etkisi            = (takim_top_kesme + takim_mudahale) / 2,
    # Clean sheet başına düşen savunma aksiyonu
    savunma_clean_sheet_orani = savunma_etkisi / clean_sheet_ratio
  )

# Korelasyon analizi için görselleştirme
ggplot(goalkeeper_defense_analysis, aes(x = savunma_etkisi, y = clean_sheet_ratio)) +
  geom_point(aes(size = match_played, color = squad), alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_text(
    aes(label = player_name),
    vjust         = -1,
    size          = 3,
    check_overlap = TRUE
  ) +
  labs(
    title    = "Kaleci Clean Sheet Oranları ve Takım Savunması İlişkisi",
    subtitle = "Nokta büyüklüğü oynanan maç sayısını gösterir",
    x        = "Takım Savunma Etkisi (Ortalama Top Kesme ve Müdahale)",
    y        = "Clean Sheet Oranı (%)",
    size     = "Maç Sayısı",
    color    = "Takım"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  ) +
  guides(color = guide_legend(title = "Takım"))

########## OYUNCU DEĞERİ VE TAKIM BAŞARISI ANALİZİ ##########

# 'points_per_match' kolonunu sayısal formata çeviriyoruz
players <- players %>%
  mutate(points_per_match = as.numeric(points_per_match))

# Takım başarı metriklerini hesaplıyoruz
takim_basari <- players %>%
  group_by(squad) %>%
  summarize(
    takim_puan           = mean(points_per_match, na.rm = TRUE),
    takim_gol            = sum(goals, na.rm = TRUE),
    takim_asist          = sum(assists, na.rm = TRUE),
    hucum_verimlilik     = takim_gol / n(),
    defansif_aksiyonlar  = mean(tackles_won + interceptions, na.rm = TRUE)
  ) %>%
  arrange(desc(takim_puan))

# Oyuncuların bireysel katkılarını hesaplıyoruz
oyuncu_deger_metrikleri <- players %>%
  mutate(
    mac_katkisi     = (goals + assists) / match_played,
    defansif_katki  = (tackles_won + interceptions) / match_played,
    dakika_katkisi  = minutes_played / (match_played * 90),
    sut_verimlilik  = ifelse(shots > 0, goals / shots, 0)
  )

# Oyuncu katkısı ve takım başarısı ilişkisini görselleştiriyoruz
ggplot(oyuncu_deger_metrikleri, aes(x = mac_katkisi, y = points_per_match)) +
  geom_point(aes(color = position_simplified, size = minutes_played), alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title    = "Oyuncu Katkısı ve Takım Başarısı İlişkisi",
    subtitle = "Maç başına gol+asist katkısının takım puanına etkisi",
    x        = "Maç Katkısı",
    y        = "Takım Puan Ortalaması",
    color    = "Pozisyon",
    size     = "Oynanan Dakika"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

########## KÜMELEME ANALİZİ ##########
# Sayısal değişkenleri seçiyoruz
sayisal_ozellikler <- players %>%
  select(goals, assists, tackles_won, interceptions, shots, minutes_played, age)

# Sayısal özelliklerin normalize edilmesi
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalize_edilmis_ozellikler <- as.data.frame(lapply(sayisal_ozellikler, normalize))

# Elbow yöntemi ile optimal küme sayısını belirliyoruz
wss <- numeric(10)  # Kümelerin WSS değerlerini saklamak için boş bir vektör
for (k in 1:10) {
  set.seed(600)  # Rastgelelik kontrolü için tohum değeri
  kmeans_model <- kmeans(normalize_edilmis_ozellikler, centers = k, nstart = 20)
  wss[k] <- kmeans_model$tot.withinss  # Küme içi toplam kareler
}

# Dirsek grafiği (Elbow)
plot(
  1:10, wss, type = "b", pch = 19, frame = FALSE,
  xlab = "Küme Sayısı (k)",
  ylab = "Toplam Küme İçi Kareler (WSS)",
  main = "Optimal Küme Sayısını Belirlemek İçin Elbow Yöntemi"
)
abline(v = 5, col = "red", lty = 2)  # Optimum k değeri (5)

########## DENDOGRAM ##########
# Mesafe matrisi (öklid mesafesi)
dist_mat <- dist(normalize_edilmis_ozellikler, method = "euclidean")

# Hiyerarşik kümeleme modeli (Ward yöntemini kullanıyoruz)
hclust_model <- hclust(dist_mat, method = "ward.D2")

# Dendogram çizimi
plot(
  hclust_model,
  main = "Hiyerarşik Kümeleme Dendogramı",
  xlab = "",
  sub = "",
  cex = 0.8,
  labels = FALSE
)

# 5 kümeyi vurguluyoruz burada
rect.hclust(hclust_model, k = 5, border = "red")

# Kümeleme ve grafiğin yeniden oluşturulması
set.seed(600)
kmeans_model <- kmeans(normalize_edilmis_ozellikler, centers = 5, nstart = 20)
players$kume <- kmeans_model$cluster

# Küme özetlerini hesaplıyoruz ve grafiği tekrar oluşturuyoruz
kume_ozetleri <- aggregate(normalize_edilmis_ozellikler, by = list(Kume = players$kume), mean)
kume_ozetleri_long <- kume_ozetleri %>%
  pivot_longer(cols = -Kume, names_to = "Metric", values_to = "Mean")

ggplot(kume_ozetleri_long, aes(x = Metric, y = Mean, fill = as.factor(Kume))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Kümelerin Performans Metrikleri Özeti (Normalize Edilmiş)",
    x     = "Metrikler",
    y     = "Ortalama Değer",
    fill  = "Küme"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14)
  )

########## MODEL OLUŞTURMA ##########

# Hedef değişkeni olarak position_simplified kullanıyoruz
players$position_simplified <- as.factor(players$position_simplified)

# Eğitim ve test veri setlerini ayırıyoruz
set.seed(600)
train_index    <- createDataPartition(players$position_simplified, p = 0.8, list = FALSE)
egitim_verisi  <- normalize_edilmis_ozellikler[train_index, ]
test_verisi    <- normalize_edilmis_ozellikler[-train_index, ]

egitim_mevki <- players$position_simplified[train_index]
test_mevki   <- players$position_simplified[-train_index]

# KNN modeli oluşturuyoruz
set.seed(6000)
knn_tahminler <- knn(
  train = egitim_verisi,
  test  = test_verisi,
  cl    = egitim_mevki,
  k     = 5
)

# Tahmin sonuçlarını değerlendiriyoruz
confusion_matrix <- confusionMatrix(knn_tahminler, test_mevki)
print(confusion_matrix)

########## K DEĞERİNE GÖRE DOĞRULUK ANALİZİ ##########

accuracy_results <- c()
for (k in 1:20) {
  knn_tahminler <- knn(
    train = egitim_verisi,
    test  = test_verisi,
    cl    = egitim_mevki,
    k     = k
  )
  accuracy <- sum(knn_tahminler == test_mevki) / length(test_mevki)
  accuracy_results <- c(accuracy_results, accuracy)
}

# Doğruluk oranını görselleştiriyoruz
plot(
  1:20, accuracy_results, type = "b",
  xlab = "k Değeri", ylab = "Doğruluk Oranı",
  main = "K'ye Göre Doğruluk"
)
abline(v = 6, col = "red", lty = 2)  # Max doğruluk değeri

########## MODELE DEĞER VEREREK POZİSYONU TAHMİN EDELİM ##########

# Manuel olarak bir oyuncunun özelliklerini giriyoruz
yeni_oyuncu <- data.frame(
  goals          = 0,     # 5 gol
  assists        = 0,     # 3 asist
  tackles_won    = 20,    # 12 başarılı müdahale
  interceptions  = 15,     # 8 top kesme
  shots          = 10,    # 20 şut
  minutes_played = 2500,   # 1800 dakika
  age = 25                # 25 yaş
)

# Normalize işlemi için fonksiyon tanımlıyoruz
normalize_value <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

# Yeni oyuncunun normalize edilmesi
yeni_oyuncu_normalize <- as.data.frame(t(mapply(
  normalize_value,
  yeni_oyuncu,
  min_val = apply(sayisal_ozellikler, 2, min),
  max_val = apply(sayisal_ozellikler, 2, max)
)))
colnames(yeni_oyuncu_normalize) <- colnames(egitim_verisi)

# KNN modeli kullanarak mevki tahmini yapıyoruz
tahmin_mevki <- knn(
  train = egitim_verisi,
  test  = yeni_oyuncu_normalize,
  cl    = egitim_mevki,
  k     = 6  # grafiğe göre en mantıklısı x yapmak
)

# Tahmin edilen sınıfı ilgili mevkiye dönüştürüyoruz
tahmin_edilen_mevki <- levels(egitim_mevki)[as.numeric(tahmin_mevki)]
cat("Yeni oyuncunun tahmin edilen mevkisi:", tahmin_edilen_mevki, "\n")

