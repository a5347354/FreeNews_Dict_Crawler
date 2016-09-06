library(rvest)
# 擷取該篇關鍵字
grep_keyword = function(url){
  free_url = read_html(url) 
  free_keyword = free_url %>% html_nodes(".con_keyword") %>% html_nodes("a") %>% html_text()
  return(free_keyword)
}
# 抓取某日期後所有頁面的關鍵字
# 今日日期
start_date = Sys.Date() 
i = 1
keyword = ""
repeat{
  news_url = read_html(paste0("http://news.ltn.com.tw/list/BreakingNews?page=",i))
  # 分析html萃取時間
  times = news_url %>% html_nodes(".lipic span") %>% html_text()
  # 轉換成日期格式
  times = as.character.Date(times) %>% as.POSIXct(., format="%Y-%m-%d %H:%M")
  
  # 分析html萃取子url
  sub_news_url = news_url %>% html_nodes(".picword") %>% html_attr("href")
  df = data.frame(times = times,sub_news_url = sub_news_url)
  df = subset(df,as.Date(df$times) >= start_date)
  print(paste0("第",i,"頁完成"))
  if(length(df$sub_news_url) != 0){
      for(j in 1:length(df$sub_news_url)){
        # 抓取子頁keyword
        keyword = c(keyword,grep_keyword(sub_news_url[j]))
      }
      i = i+1
       
  }else{
      break
  }
}
#不重複，並把第一個空字元刪除
keyword = unique(keyword[-1])

#寫入txt裡
fileConn<-file("direc.txt")
writeLines(keyword, fileConn)
close(fileConn)