
#先取得資料，將兩筆資料合併，然後將複雜的資料名稱簡易化
library(dplyr)
library(ggplot2)
#記得要先設定好工作路徑跟相對檔名(點選 more 裡的 set as working directory)

# Read in data files from `open-data` and `world-happiness` datasets

open_data <- read.csv("countries.csv", stringsAsFactors=F)
happiness <- read.csv("2015.csv", stringsAsFactors=F)
#stringsAsFactors = FALSE tells R to keep character variables as they are rather than convert to factors, which are a little harder to work with.(不讓r將character 轉成 factor)

# Rename from "Country Name" to just "Country" so it's easier to join
colnames(open_data)[2] <- "Country"
#colnames()可以改column的名字

# Join the two dataset files on "Country"
open_data_happiness <- open_data %>%
  left_join(happiness, by = "Country") %>%
  mutate(Country = factor(Country)) %>%
  # Keep only columns I plan to use
  select(Country, Region, X2015.Score, Happiness.Score, Economy..GDP.per.Capita., 
         Family, Health..Life.Expectancy., Freedom, Trust..Government.Corruption., 
         Generosity, Dystopia.Residual)
#left_join可以藉由兩數據中相同的column來做連接，並且是往左增加
# Give the columns nicer names now that our data is in one dataframe
colnames(open_data_happiness) <- c("Country", "Region", "Openness", "Happiness", "GDP", "Family", "Health", "Freedom", "Trust", "Generosity", "DystopiaResidual")
#將數據的名字重新命名以便閱讀
#到此為止，已經將數據初步整理

library(formattable)
#formmatable可將數據格式化，如百分比，逗號，貨幣，會計和科學，並用表格呈現，是一種易於閱讀的方式
#將資料依開放程度排名，並四捨五入資料至小數第二位，再選出前十
open_data_happiness%>%
  arrange(desc(Openness))%>%
  #mutate_each中的計算會套用至每一行column，用round四捨五入至小數點第二位，其中Country,Region,Opennes 不用被計算
  mutate_each(funs(round(.,2)),-c(Country,Region,Openness))%>%
  head(10)%>%
  formattable(list(
    Openness = color_bar("yellow"),
    Happiness = color_bar("pink"),
    GDP = color_bar("deepskyblue"),
    Family = color_bar("deepskyblue"),
    Health = color_bar("deepskyblue"),
    Freedom = color_bar("deepskyblue"),
    Trust = color_bar("deepskyblue"),
    Generosity = color_bar("deepskyblue"),
    DystopiaResidual = color_bar("deepskyblue")
  ), align = "l"
  )
#表格完成

#探索開放程度與幸福程度之間的關係
library(ggplot2)
library(ggthemes)
library(viridis)
ggplot(open_data_happiness, 
       aes(x = Openness, 
           y = Happiness)) +
  geom_point(aes(colour = Region),
             size = 2) +
  #以region為點點顏色的分別基準
  geom_smooth(method="lm") +
  #geom_smooth可以劃出趨勢曲線
  labs(x = "Openness Score",
       y = "Happiness Score",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data openness and happiness by country in 2015") +
  scale_color_viridis(discrete = T) +
  #scale_color_viridis讓顏色漸層有方向
  theme_minimal() +
  theme(text = element_text(size=16))




#慷慨的國家快樂嗎?
#分析happines跟Generosity的相關程度
#先畫圖看看
ggplot(data = open_data_happiness, aes(x = Generosity, y = Happiness)) +
  geom_point()
#好像看不出關係

#比較華人地區跟西方文化地區的慷慨程度
choose<-open_data_happiness%>%filter(Region== "Eastern Asia"|Region=="Western Europe" )
ggplot(choose,aes(x=Region,y=Generosity))+geom_boxplot() +coord_flip()

#以下函式計算%95信賴區間
with(choose, 
     tapply(Generosity, Region,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))

#以下用 t-test 檢驗不同區域是否存在慷慨程度差異
t.test(Generosity ~ Region, data = choose)
t.test(Generosity ~ Region, data = choose, var.equal = TRUE)
#p-value = 0.2234，兩種文化區的慷慨程度分布相似


#課堂筆記:分布一致才能用回歸
#         殘差項很大代表挑選的配對model不對，要再換
#         每個點與估計的點的差異稱作殘差

#以下觀察Generosity是否與地區有關
#先把地區欄位內的順序定下來(order of factors)
open_data_happiness$Region <- factor(open_data_happiness$Region,
                                 levels = c('Eastern Asia',
                                            'Western Europe',
                                            'North America',
                                            'Latin America and Caribbean', 
                                            'Central and Eastern Europe',
                                            'Southern Asia',
                                            'Southeastern Asia',
                                            'Middle East and Northern Africa',
                                            'Sub-Saharan Africa',
                                            'Australia and New Zealand'))

#看不同地區下的慷慨分數平均數
library(Hmisc)
tapply(open_data_happiness$Generosity, open_data_happiness$Region, mean)

#同地區下的慷慨分數平均數，加上信賴區間
ggplot(data = open_data_happiness, 
       aes(x = open_data_happiness$Region, y = open_data_happiness$Generosity)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_hline(yintercept = mean(open_data_happiness$Generosity) , 
             linetype = 'dotted') +
  labs(x = '慷慨程度', y = '區域')+
  coord_flip()

#在這裡我們推測慷慨的程度可能是快樂差距造成的，畫圖觀察看看
anova(m1 <- lm(Generosity ~ Region, data = open_data_happiness))
#畫圖
#如果不分區域來看關係
ggplot(data = open_data_happiness, aes(y = Generosity, x = Happiness)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  labs(x = '快樂指數', y = '慷慨指數')

#如果把區域獨立出來
choose1<-open_data_happiness%>%filter(Region!="NA")
ggplot(data = choose1, 
       aes(group = Region, 
           y = Generosity, x = Happiness)) +
  geom_point() +
  stat_smooth(aes(group = Region, 
                  y = Generosity, x = Happiness), 
              method = 'lm', se = F) + 
  facet_grid( . ~  Region) +
  labs(x = '快樂指數', y = '慷慨指數')


#跟GDP有關嗎
ggplot(data = open_data_happiness, aes(y = Generosity, x = GDP)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  labs(x = 'GDP', y = '慷慨指數')

ggplot(data = choose1, 
       aes(group = Region, 
           y = Generosity, x = GDP)) +
  geom_point() +
  stat_smooth(aes(group = Region, 
                  y = Generosity, x = GDP), 
              method = 'lm', se = F) + 
  facet_grid( . ~  Region) +
  labs(x = 'GDP', y = '慷慨指數')


#跟自由有關嗎
ggplot(data = open_data_happiness, aes(y = Generosity, x = Freedom)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  labs(x = 'Freedom', y = '慷慨指數')

ggplot(data = choose1, 
       aes(group = Region, 
           y = Generosity, x = Freedom)) +
  geom_point() +
  stat_smooth(aes(group = Region, 
                  y = Generosity, x = Freedom), 
              method = 'lm', se = F) + 
  facet_grid( . ~  Region) +
  labs(x = 'Freedom', y = '慷慨指數')

#......好像是跟快樂和自由比較有關

## 利用以下 ANOVA 檢驗假設是否正確
#把快樂加進模型
anova(m2 <- update(m1, . ~ . + 
                     Happiness, data = open_data_happiness))

#或許不是地區影響而是快樂指數造成
anova(m3 <- update(m2, . ~ . - Region,  data = open_data_happiness))


## 將 ANOVA 結果做成圖表輸出，先計算一些需要的數據
#將結果放在一個list中
#m1:只有區域  m2:區域跟快樂  m3:只有快樂
res_lm <- lapply(list(m1, m2, m3), summary)

#比較在控制快樂指數下，地區影響的效果
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)
#差很多

#比較在控制區域下，快樂指數的效果
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)
#差不多

#正式畫圖
require(coefplot)
m2 <- lm(Generosity ~ Region+Happiness- 1, 
         data = open_data_happiness)
coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = 慷慨分數')

#把資料與迴歸分析的預測值、殘差與影響度放進資料
fit_m2 <- data.frame(choose1[, c(10, 2, 4)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )
#依區域疊合真實觀測值與預測值
ggplot(data = fit_m2, aes(x = Generosity, group = Region )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(choose1, tapply(Generosity,Region, mean))), linetype = 'dotted')+
  facet_grid(Region ~ .) +
  scale_x_continuous(breaks = seq(0, 1, by = 100))+
  labs(x = '數學分數', y = '機率密度')


#看殘差分配，依區域，檢視常態與變異數同質假設
ggplot(data = fit_m2, aes(x = scale(resid)), group = Region ) +
  stat_density(geom = 'path', position = 'identity', aes(linetype = Region)) +
  scale_linetype_manual(values = 10:1) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  labs(x = '標準化殘差', y = '機率密度') 

#看看殘差的 Q-Q 圖，依區域。檢視常態假設
require(lattice)
qqmath(~ scale(resid) |Region , data = fit_m2, type = c('p', 'g', 'r'),
       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),
       pch = '.', cex = 2)

#畫預測值與殘差的散佈圖，檢查線性與等分散假設
require(MASS)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = Region )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(Region ~ .) +
  labs(x = '慷慨指數預測值', y = '標準化殘差')


#呈現影響值（影響估計結果過大的值）與標準化殘差
ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group =Region)) +
  geom_text(aes(label = rownames(fit_m2)), cex = 2) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(Region ~ .) +
  labs(x = '影響值', y = '標準化殘差')

#看看影響值
summary(influence(m2)$hat)
#記得改回舊的配色，不然 R 就黑白下去了
#theme_set(old) 出現配色問題 

#Part C. 接下來分析資料集當中的其他變項是否和慷慨程度有關
dta_happy <- choose1[, c('Openness','Happiness','GDP','Family','Health','Freedom','Trust' ,'Generosity', 
                    'DystopiaResidual')]
#看看基本統計量
colMeans(dta_happy)
#呈現兩兩散佈圖

require(heplots)
scatterplotMatrix(~ Generosity + Openness + Happiness + GDP+Family+Health+Freedom+Trust+DystopiaResidual, data= choose1,
                  pch = '.', cex = 3, smooth = FALSE, reg.line = FALSE, ellipse = TRUE,
                  diagonal = 'none', lower.panel = NULL)

#好像有點太小了，捨棄幾個項目好了
dta_happy1 <- choose1[, c('Openness','Happiness','GDP','Family','Freedom','Trust' ,'Generosity')]
require(heplots)
scatterplotMatrix(~ Generosity + Openness + Happiness + GDP+Family+Freedom+Trust, data= choose1,
                  pch = '.', cex = 3, smooth = FALSE, reg.line = FALSE, ellipse = TRUE,
                  diagonal = 'none', lower.panel = NULL)

#畫出各個變量的相關程度(運用corrplot)
require(corrplot)
corrplot(cor(dta_happy), method = 'ellipse', order = 'hclust', addrect = 4,
         type = 'upper', tl.pos = 'tp')
corrplot(cor(dta_happy), add = TRUE, type = 'lower', method = 'number',
         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

#放進三個解釋變項
summary(m4 <- lm(Generosity ~ Freedom + Trust + Happiness, data =dta_happy ))

#來看看效果如何
coefplot(m4, predictors = c('Freedom', 'Trust', 
                            'Happiness'),
         xlab = '估計值', ylab = '迴歸變項(去除截距)', title = '反應變項是慷慨指數')


require(effects)
plot(allEffects(m4), main = '', ylim = c(550, 670), grid = T)
#為何沒有藍線QQ

#看看控制信任與幸福後，自由的效果
summary(m5 <- update(m4, . ~ . - Freedom , data = dta_happy))
anova(m5, m4)


m5 <- lm(Generosity ~ Freedom + Region + Happiness + Trust, data = choose1)
fit_m5 <- data.frame(choose1[, c(10, 8, 2, 4, 9)], fitted = fitted(m5), resid = resid(m5), infl = influence(m5)$hat)

ggplot(data = fit_m5, aes(x = Generosity, group = Region )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(choose1, tapply(Generosity, Region, mean))), linetype = 'dotted')+
  facet_grid(Region ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = '數學分數', y = '機率密度')
