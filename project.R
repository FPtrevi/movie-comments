#�������� �ҷ�����
library(readxl)
library(stringr)

#cgv(���˵���)
cgv_word1 <- read_excel("cgv_word1.xlsx", col_names = "text")
cgv_word1 <- na.omit(cgv_word1)
cgv_word1 <- cgv_word1$text
cgv_word1 <- gsub("[\\s+]","",cgv_word1)
cgv_word1 <- gsub("[[:punct:]]",'',cgv_word1)
cgv_word1 <- gsub("[[:cntrl:]]",' ', cgv_word1)

#cgv(���ͽ�Ʈ������)
cgv_word2 <- read_excel("cgv_word2.xlsx", col_names = "text")
cgv_word2 <- na.omit(cgv_word2)
cgv_word2 <- cgv_word2$text
cgv_word2 <- gsub("[\\s+]","",cgv_word2)
cgv_word2 <- gsub("[[:punct:]]",'',cgv_word2)
cgv_word2 <- gsub("[[:cntrl:]]",' ', cgv_word2)

#cgv(���ﱫ��)
cgv_word3 <- read_excel("cgv_word3.xlsx", col_names = "text")
cgv_word3 <- na.omit(cgv_word3)
cgv_word3 <- cgv_word3$text
cgv_word3 <- gsub("[\\s+]","",cgv_word3)
cgv_word3 <- gsub("[[:punct:]]",'',cgv_word3)
cgv_word3 <- gsub("[[:cntrl:]]",' ', cgv_word3)

#megabox(���˵���)
megabox_word1 <- read.table("mega_word1.txt", header = FALSE, sep = "\n", quote = "", fileEncoding = "UTF-8",encoding = "CP949", col.names = "text")
megabox_word1 <- na.omit(megabox_word1)
megabox_word1 <- megabox_word1$text
megabox_word1 <- gsub("[\\s+]","",megabox_word1)
megabox_word1 <- gsub("[[:punct:]]",'',megabox_word1)
megabox_word1 <- gsub("[[:cntrl:]]",' ', megabox_word1)

#megabox(���ͽ�Ʈ������)
megabox_word2 <- read.table("mega_word2.txt", header = FALSE, sep = "\n", quote = "", fileEncoding = "UTF-8",encoding = "CP949", col.names = "text")
megabox_word2 <- na.omit(megabox_word2)
megabox_word2 <- megabox_word2$text
megabox_word2 <- gsub("[\\s+]","",megabox_word2)
megabox_word2 <- gsub("[[:punct:]]",'',megabox_word2)
megabox_word2 <- gsub("[[:cntrl:]]",' ', megabox_word2)

#megabox(���ﱫ��)
megabox_word3 <- read.table("mega_word3.txt", header = FALSE, sep = "\n", quote = "", encoding = "UTF-8", col.names = "text")
megabox_word3 <- na.omit(megabox_word3)
megabox_word3 <- megabox_word3$text
megabox_word3 <- gsub("[\\s+]","",megabox_word3)
megabox_word3 <- gsub("[[:punct:]]",'',megabox_word3)
megabox_word3 <- gsub("[[:cntrl:]]",' ', megabox_word3)

#lotte(���˵���)
lotte_word1 <- read_excel("lotte_word1.xlsx", col_names = "text")
lotte_word1 <- na.omit(lotte_word1)
lotte_word1 <- lotte_word1$text
lotte_word1 <- gsub("[\\s+]","",lotte_word1)
lotte_word1 <- gsub("[[:punct:]]",'',lotte_word1)
lotte_word1 <- gsub("[[:cntrl:]]",' ', lotte_word1)

#lotte(���ͽ�Ʈ������)
lotte_word2 <- read_excel("lotte_word2.xlsx", col_names = "text")
lotte_word2 <- na.omit(lotte_word2)
lotte_word2 <- lotte_word2$text
lotte_word2 <- gsub("[\\s+]","",lotte_word2)
lotte_word2 <- gsub("[[:punct:]]",'',lotte_word2)
lotte_word2 <- gsub("[[:cntrl:]]",' ', lotte_word2)

#lotte(���ﱫ��)
lotte_word3 <- read_excel("lotte_word3.xlsx", col_names = "text")
lotte_word3 <- na.omit(lotte_word3)
lotte_word3 <- lotte_word3$text
lotte_word3 <- gsub("[\\s+]","",lotte_word3)
lotte_word3 <- gsub("[[:punct:]]",'',lotte_word3)
lotte_word3 <- gsub("[[:cntrl:]]",' ', lotte_word3)

#����Ŭ����
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

#cgv����Ŭ����
for (doit in 1:3) {
  cgv_word <- extractNoun(get(paste0("cgv_word",doit)))
  pal <- brewer.pal(8,"Dark2")
  noun2 <- unlist(cgv_word)
  wordcount <- table(noun2)
  png(filename = paste0("cgv_wordcloud",doit,".png"), width = 500, height = 500)
  wordcloud(names(wordcount),
            freq = wordcount,
            scale=c(8,0.5),
            min.freq = 3,
            random.order = F,
            rot.per = .1,
            colors = pal)
  dev.off()
}

#�ް��ڽ�����Ŭ����
for(doit in 1:3){
  megabox_word <- extractNoun(get(paste0("megabox_word",doit)))
  pal <- brewer.pal(8,"Dark2")
  noun2 <- unlist(megabox_word)
  wordcount <- table(noun2)
  png(filename = paste0("megabox_wordcloud",doit,".png"), width = 500, height = 500)
  wordcloud(names(wordcount),
            freq = wordcount,
            scale=c(10,0.5),
            min.freq = 3,
            random.order = F,
            rot.per = .1,
            colors = pal)
  dev.off()
}

#�Ե��ó׸�����Ŭ����
for(doit in 1:3){
  lotte_word <- extractNoun(get(paste0("lotte_word",doit)))
  pal <- brewer.pal(8,"Dark2")
  noun2 <- unlist(lotte_word)
  wordcount <- table(noun2)
  png(filename = paste0("lotte_wordcloud",doit,".png"), width = 500, height = 500)
  wordcloud(names(wordcount),
            freq = wordcount,
            scale=c(8,0.5),
            min.freq = 3,
            random.order = F,
            rot.per = .1,
            colors = pal)
  dev.off()
}

#cgv ����� ����
for(doit in 1:3){
  react <- c("���", "�׷�����", "���","��ſ�","����","���","����","����",
             "����","��","���","����","�ְ�","���","��","���","���ٸ�",
             "����","����","���","����","����","��Ծ�","������","¯")
  #������
  good <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("cgv_word",doit)),j))
    good <- good+count
  }
  
  #������
  react <- c("�Ǹ�","����","��Ȳ","����","����","��̾�","�ƽ���","�ƽ�",
             "�׸�","��ġ","�ƽ�","���","���","�Ƿ�","���","����","����","¡�׷���",
             "�����")
  nogood <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("cgv_word",doit)),j))
    nogood <- nogood+count
  }
  
  per_good <- round(good/(good+nogood),2)*100
  per_noGood <- round(nogood/(good+nogood),2)*100
  
  part <- c("good", "nogood")
  score <- c(per_good,per_noGood)
  
  data <- data.frame(part, score)
  
  png(filename = paste0("cgv_gra",doit,".png"), width = 400, height = 400)
  myplot <- barplot(data$score, names=c("����","����"), col=c("green","yellow"))
  text(x=myplot,y=data$score-2,labels=paste0(data$score,"%"), col="black", cex = 1)
  dev.off()
}

#�ް��ڽ� ����� ����
for(doit in 1:3){
  react <- c("���", "�׷�����", "���","��ſ�","����","���","����","����",
             "����","��","���","����","�ְ�","���","��","���","���ٸ�",
             "����","����","���","����","����","��Ծ�","������","¯")
  #������
  good <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("megabox_word",doit)),j))
    good <- good+count
  }
  
  #������
  react <- c("�Ǹ�","����","��Ȳ","����","����","��̾�","�ƽ���","�ƽ�",
             "�׸�","��ġ","�ƽ�","���","���","�Ƿ�","���","����","����","¡�׷���",
             "�����")
  nogood <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("megabox_word",doit)),j))
    nogood <- nogood+count
  }
  
  per_good <- round(good/(good+nogood),2)*100
  per_noGood <- round(nogood/(good+nogood),2)*100
  
  part <- c("good", "nogood")
  score <- c(per_good,per_noGood)
  
  data <- data.frame(part, score)
  
  png(filename = paste0("megabox_gra",doit,".png"), width = 400, height = 400)
  barplot(data$score, names=c("����","����"), col=c("green","yellow"))
  text(x=myplot,y=data$score-2,labels=paste0(data$score,"%"), col="black", cex = 1)
  dev.off()
}

#�Ե��ó׸� ����� ����
for(doit in 1:3){
  react <- c("���", "�׷�����", "���","��ſ�","����","���","����","����",
             "����","��","���","����","�ְ�","���","��","���","���ٸ�",
             "����","����","���","����","����","��Ծ�","������","¯")
  #������
  good <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("lotte_word",doit)),j))
    good <- good+count
  }
  
  #������
  react <- c("�Ǹ�","����","��Ȳ","����","����","��̾�","�ƽ���","�ƽ�",
             "�׸�","��ġ","�ƽ�","���","���","�Ƿ�","���","����","����","¡�׷���",
             "�����")
  nogood <- 0
  for(j in react){
    count <- sum(str_count(get(paste0("lotte_word",doit)),j))
    nogood <- nogood+count
  }
  
  per_good <- round(good/(good+nogood),2)*100
  per_noGood <- round(nogood/(good+nogood),2)*100
  
  part <- c("good", "nogood")
  score <- c(per_good,per_noGood)
  
  data <- data.frame(part, score)
  
  png(filename = paste0("lotte_gra",doit,".png"), width = 400, height = 400)
  barplot(data$score, names=c("����","����"), col=c("green","yellow"))
  text(x=myplot,y=data$score-2,labels=paste0(data$score,"%"), col="black", cex = 1)
  dev.off()
}
