# Oyuncu Analizi Projesi

Bu proje, 2020-2021 Süper Lig sezonundaki futbolcuların ve kalecilerin performansını analiz etmek amacıyla R programlama dili kullanılarak geliştirilmiştir. Proje kapsamında veri temizleme, görselleştirme, korelasyon analizi, kümeleme ve makine öğrenmesi modelleri gibi çeşitli veri bilimi teknikleri uygulanmıştır.

## Projenin Amacı

Bu projenin temel amacı, oyuncuların yaş, pozisyon ve diğer performans metriklerine göre istatistiksel analizlerini yaparak futbolda başarıyı etkileyen faktörleri ortaya çıkarmaktır. Ayrıca, kalecilerin performansları ayrı bir veri seti üzerinden detaylı olarak incelenmiştir.

## Yapılan Analizler

Proje kapsamında aşağıdaki analizler gerçekleştirilmiştir:

* **Veri Temizleme ve Hazırlama:** Ham veriler okunmuş, Türkçe karakter uyumluluğu sağlanmış ve analiz için uygun hale getirilmiştir.
* **Pozisyon ve Yaş Gruplarına Göre Sınıflandırma:** Oyuncular "Defans", "Orta Saha" ve "Forvet" olarak yeniden sınıflandırılmış ve yaşlarına göre "21 Yaş ve Altı", "22-25 Yaş", "26-29 Yaş", "30-33 Yaş" ve "33 Yaş Üstü" gibi gruplara ayrılmıştır.
* **Performans Metrikleri Analizi:** Oyuncuların gol, asist, oynanan dakika, şut isabeti gibi metrikleri pozisyon ve yaş gruplarına göre incelenmiş ve görselleştirilmiştir.
* **Korelasyon Analizi:** Oyuncu performansını etkileyen metrikler arasındaki ilişkiyi anlamak için bir korelasyon matrisi oluşturulmuştur.
* **Kaleci Performans Analizi:** Kalecilerin kurtarış yüzdesi, maç başına kurtarış, penaltı kurtarış oranı gibi istatistikleri analiz edilmiş ve yaş gruplarına göre performansları karşılaştırılmıştır.
* **Kümeleme Analizi (Clustering):** Oyuncular, performans metriklerine göre K-Means ve Hiyerarşik Kümeleme yöntemleri kullanılarak gruplara ayrılmıştır. Optimal küme sayısı "Elbow Yöntemi" ile belirlenmiştir.
* **Makine Öğrenmesi ile Pozisyon Tahmini:** Oyuncuların istatistiklerine dayanarak pozisyonlarını tahmin etmek için bir K-Nearest Neighbors (KNN) modeli oluşturulmuş ve modelin doğruluğu test edilmiştir. Ayrıca, yeni bir oyuncu profili için pozisyon tahmini yapılmıştır.

## Kullanılan Teknolojiler ve Kütüphaneler

Bu proje R programlama dili ile geliştirilmiştir ve aşağıdaki kütüphanelerden yararlanılmıştır:

* `tidyverse`: Veri manipülasyonu ve görselleştirme için.
* `corrplot`: Korelasyon matrisini görselleştirmek için.
* `caret`: Makine öğrenmesi modellemesi için.
* `class`: KNN algoritması için.

## Veri Seti

Projede kullanılan veriler [fbref.com](https://fbref.com/en/comps/26/2020-2021/misc/2020-2021-Super-Lig-Stats) adresinden alınmıştır. Veri seti, oyuncu ve kaleci istatistiklerini içeren iki ayrı CSV dosyasından oluşmaktadır (`player_stats.csv` ve `goalkeepers_stats.csv`).

## Projenin Çalıştırılması

Projeyi kendi bilgisayarınızda çalıştırmak için aşağıdaki adımları izleyebilirsiniz:

1.  Bu repoyu klonlayın veya dosyaları indirin.
2.  R veya RStudio'yu açın.
3.  `R_Project.R` dosyasındaki `setwd()` fonksiyonu ile çalışma dizinini kendi dosya yolunuza göre güncelleyin.
4.  Gerekli kütüphanelerin yüklü olduğundan emin olun. Değilse, `install.packages("paket_adi")` komutu ile yükleyebilirsiniz.
5.  Script'i çalıştırın.

## Katkıda Bulunanlar

* Tuncay EŞSİZ (191005053)
* Betül KIZILOBEK (191005003)

## Lisans

Bu proje MIT Lisansı ile lisanslanmıştır. Detaylı bilgi için `LICENSE` dosyasına bakabilirsiniz.
