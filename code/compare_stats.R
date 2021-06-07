
require(data.table)
scr_q1_pa = fread('input/JournalHomeGrid (1).csv',skip = 1)
scr_q1_pa$journal_title = toupper(scr_q1_pa$`Full Journal Title`)


require(readxl)
berk = read_excel('input/berkeley_2015_2019.xls')
berk$Q1 <-berk$`Source Title` %in% scr_q1_pa$journal_title

usc = read_excel('input/usc_2015_2019.xls')
usc$Q1 <- usc$`Source Title` %in% scr_q1_pa$journal_title

ucla = read_excel('input/ucla_2015_2019.xls')
ucla$Q1 = ucla$`Source Title` %in% scr_q1_pa$journal_title


davis = read_excel('input/cepb_2015_2019.xls')
davis$Q1 <- davis$`Source Title` %in% scr_q1_pa$journal_title


berk$org = berk$org2 = 'Berkeley'
berk$usnwr = '#4(t)'
berk$fill = '#003262'
berk$col = '#FDB515'
davis$org = davis$org2= 'Davis'
davis$fill = '#022851'
davis$usnwr = 'no program'
davis$col = '#FFBF00'
ucla$org = ucla$org2 = 'UCLA'
ucla$fill = '#2774AE'
ucla$col = '#FFD100'
ucla$usnwr = '#13(t)'
usc$org = usc$org2 = 'USC'
usc$fill = '#990000'
usc$col = '#FFCC00'
usc$usnwr = '#4(t)'

cepb = davis[grepl('Arnold|Scott|Lubell|Vantaggiato',davis$`Author Full Names`),]
cepb$org2 <- 'CEPB'
df = rbindlist(list(berk,davis,cepb,ucla,usc),use.names = T,fill = T)

df_q1 = df[Q1==T,]
df_q1$org <- forcats::fct_rev(forcats::fct_infreq(df_q1$org))

counts =   fct_count(df_q1$org2)


require(tidyverse)
(gg1 = ggplot(data = df_q1) + 
  geom_bar(aes(x = org,fill = fill,col = col,group = org2),lwd = 1,position = position_dodge()) +
  scale_fill_identity() + scale_color_identity() +
  theme_bw() + coord_flip() + theme(axis.text=element_text(size = 12)) +
  scale_y_continuous('# Q1 publications*') +
    annotate(geom = 'text',x = c(as.numeric(df_q1$org)[which(df_q1$org=='Davis')][1]-0.2,as.numeric(df_q1$org)[which(df_q1$org=='Davis')][1]+0.2), label = c('CEPB','all UCD (no PA school)'),
             y = counts$n[counts$f%in%c('CEPB','Davis')]/2,
             col = '#FFBF00')+
    annotate(geom = 'text',x = as.numeric(df_q1$org)[which(df_q1$org=='Berkeley')][1], label = c('Goldman School (#4(t)**)'),
             y = counts$n[counts$f%in%c('Berkeley')]/2,
             col = '#FDB515')+
    annotate(geom = 'text',x = as.numeric(df_q1$org)[which(df_q1$org=='USC')][1], label = c('Price School (#4(t)**)'),
             y = counts$n[counts$f%in%c('USC')]/2,
             col ='#FFCC00')+
    annotate(geom = 'text',x = as.numeric(df_q1$org)[which(df_q1$org=='UCLA')][1], label = c('Luskin School (#13(t)**)'),
             y = counts$n[counts$f%in%c('UCLA')]*4,
             col ='#2774AE')+
  ggtitle('# Q1 publications') +
  theme(axis.title.y = element_blank())+ 
  NULL)


df_med = df[,list(mean_citation = mean(`Times Cited, WoS Core`),
                  median_citation = median(`Times Cited, WoS Core`)),by=.(org,col,fill)]

df_med=rbind(df_med,
df[org2=='CEPB',list(mean_citation = mean(`Times Cited, WoS Core`),
                  median_citation = median(`Times Cited, WoS Core`)),by=.(org,org2,col,fill)],fill = T,use.names=T)


df_med_q1 = df[Q1==T,list(mean_citation = mean(`Times Cited, WoS Core`),
                  median_citation = median(`Times Cited, WoS Core`)),by=.(org,col,fill)]

df_med_q1 = rbind(df_med_q1,
      df[Q1==T&org2=='CEPB',list(mean_citation = mean(`Times Cited, WoS Core`),
                    median_citation = median(`Times Cited, WoS Core`)),by=.(org,org2,col,fill)],fill = T,use.names = T)
      
df_med$org <- forcats::fct_reorder(df_med$org,df_med$median_citation,.fun = max)



(gg2 = ggplot() + 
  geom_point(data = df_med,aes(x = org,y = median_citation,col = col,fill = fill,pch = '22',group = org2),size = 5,position = position_dodge(width= 0.25),lwd = 2) + 
  geom_point(data = df_med_q1,aes(x = org,y = median_citation,col = col,fill = fill,pch = '24',group = org2),size = 5,position = position_dodge(width =0.25),lwd = 2) + 
  coord_flip() + theme_bw() + 
  scale_color_identity() + 
  scale_fill_identity() + 
  annotate(geom = 'text',x = c(as.numeric(df_med$org)[which(df_med$org=='Davis')][1]-0.3,as.numeric(df_med$org)[which(df_med$org=='Davis')][1]+0.3), label = c('CEPB','all UCD'),y = c(17,16),
             col = '#022851')+
  scale_shape_manual(values = c(22,24),labels = c('all pubs.','Q1 pubs.'))+
  scale_y_continuous(name = 'median citations/article*',limits = c(0,20)) + 
  theme(legend.title = element_blank(),axis.title.y= element_blank(),
        legend.position = c(0.8,0.15),axis.text= element_text(size = 12)) + 
  ggtitle('Median citations/article') + 
  NULL)




(grid.arrange(gg1,gg2,ncol = 2,
              top = 'UCD public admn. research compared to leading PA programs in California, 2015-2019',
              bottom='*from Clarivate Web of Science and Journal Citation Reports, 2021 (2015-2019 period)\n**US News & World Report Graduate School Rankings'))

require(gridExtra)

ggsave(plot = (grid.arrange(gg1,gg2,ncol = 2,
             top = 'UCD public admn. research compared to leading PA programs in CA, 2015-2019',
             bottom='*from 2021 Clarivate Web of Science and Journal Citation Reports\n**2021 US News & World Report Graduate School Rankings')),
       filename = 'output/impact_comparison.png',dpi = 300,width = 7,height = 4,units = 'in')



