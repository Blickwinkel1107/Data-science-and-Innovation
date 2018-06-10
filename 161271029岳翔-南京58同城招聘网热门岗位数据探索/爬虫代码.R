library(rvest)
library(mongolite)
library(stringr)
library(RJSONIO)

prefix = 'http://nj.58.com/job/'
url = paste(prefix, 'pn1', sep='')
maxPage = url %>% read_html() %>% html_nodes('i.total_page') %>% html_text() %>% as.numeric()
address = name = company = salary = type = edubg = experience = welfareNum = NULL

# main loop
for(i in 1:maxPage){
  url = paste(prefix, 'pn', i, sep='')
  html = read_html(url)
  address = c(address, html %>% html_nodes('span.address') %>% html_text())
  name = c(name, html %>% html_nodes('span.name') %>% html_text())
  requires = html %>% html_nodes('div.item_con.job_comp')
  company = c(company, requires %>% html_nodes('div.comp_name') %>% html_nodes('a.fl') %>% html_text())
  salary = c(salary, html %>% html_nodes('p.job_salary') %>% html_text())
  type = c(type, requires %>% html_nodes('span.cate') %>% html_text())
  edubg = c(edubg, requires %>% html_nodes('span.xueli') %>% html_text())
  experience = c(experience, requires %>% html_nodes('span.jingyan') %>% html_text())
  welfareNodes = html %>% html_nodes('li.job_item.clearfix')
  vec = NULL
  for(node in welfareNodes){
    vec = c(vec, node %>% html_nodes('div.job_wel.clearfix') %>% html_nodes('span') %>% length())
  }
  welfareNum = c(welfareNum, vec)
}
df = data.frame(address, name, company, salary, type, edubg, experience, welfareNum)
json_data = toJSON(df)
write.table(df, '58同城热门岗位信息.txt', row.names = F, quote = F)
write.csv(df, '58同城热门岗位信息.csv')
conn = mongo(collection = 'jobs', db = 'test', url = 'mongodb://localhost')
conn$insert(json_data)
# 注：导出数据库文件是通过mongodump.exe执行的