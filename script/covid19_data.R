covid19_data <- function(covid.path.data = "~/git/covid19_dashboard/data/"){
  
  url <- 'https://www.data.gouv.fr/fr/datasets/cas-confirmes-dinfection-au-covid-19-par-region/'
  url <- url %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="resource-fa9b8fc8-35d5-4e24-90eb-9abe586b0fa5"]/footer/div[2]/a[2]') %>%
    as.character() %>%
    ex_between('href="', '" class')
  url <- url[[1]]
  data <- fread(url, fill = T, encoding = 'UTF-8', verbose = F)
  
  data[, Date := as.IDate(as.character(Date), '%Y/%m/%d')]
  data <- melt(data, id = 'Date')
  setnames(data, colnames(data), c('date', 'lib_reg', 'cas'))
  
  data.list <- split(data, by = 'lib_reg')
  for(i in 1:length(data.list)){
    data.list[[i]]$var <- as.numeric()
    data.list[[i]]$new_case <- as.numeric()
    for(j in 2:nrow(data.list[[i]])){
      data.list[[i]]$var[j] <- (data.list[[i]]$cas[j] - data.list[[i]]$cas[j-1]) / data.list[[i]]$cas[j-1]
      data.list[[i]]$new_cas[j] <- (data.list[[i]]$cas[j] - data.list[[i]]$cas[j-1]) }}
  data <- do.call(rbind, data.list)
  data[is.na(data)] <- 0
  data[var == Inf, var := 0]
  data[, var := round(var, 6)]

  tr_geocodage <- fread(paste0(covid.path.data, 'ref/tr_geo_reg.csv'))
  data <- merge(data, tr_geocodage, by = 'lib_reg')
  data[, id_reg := .GRP, by = "lib_reg"]
  
  histo <- merge(data[, sum(cas), by = 'date'], data[, sum(new_cas), by = 'date'], by = 'date')
  setnames(histo, c('date', 'cas', 'new_cas'))
  histo <- histo[order(-date)]
  today <- as.Date(unique(data[date == max(date), date]), '%Y-%m-%d')
  var2 <- data[date %in% c(today-1, today), sum(cas), by = 'date']
  var <- data[date %in% c(today-1, today), sum(new_cas), by = 'date']
  
  covid19 <- list('data' = data, 
                  'today' = today, 
                  'histo' = histo,
                  'var_cas' = var2,
                  'var_newcas' = var)
  
  save(covid19, file = paste0(covid.path.data, 'covid19.rdata')) }