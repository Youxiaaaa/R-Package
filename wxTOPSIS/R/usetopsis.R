# 标准化变量值
z_value <- function(x){
  x / sqrt(sum(x^2))
}

# 加权标准化变量值
w_value <- function(x,y){
  for(i in 1:ncol(x)) {
    x[,i]=x[,i]*y[i]
  }
  return (x)
}

#根据属性得到正理想型(1是成本型，0是效益型)
en_positive <- function(x,y) {
  z <- c()
  for(i in 1:ncol(x)) {
    if(y[i]==1) {
      z[i]=min(x[,i])
    }
    else {
      z[i]=max(x[,i])
    }
  }
  return (z)
}

#根据属性得到负理想型(1是成本型，0是效益型)
en_negative <- function(x,y) {
  z <- c()
  for(i in 1:ncol(x)) {
    if(y[i]==1) {
      z[i]=max(x[,i])
    }
    else {
      z[i]=min(x[,i])
    }
  }
  return (z)
}

# 计算最优距离
dist <-function(x, std){
  res <- c()
  for ( i in 1 : nrow(x)) {
    res[i] = sqrt(sum((unlist(x[i,-1])-std)^2))
  }
  return(res)
}

usetopsis <- function(content, weight, attribute) {
  # load sample data
  dat <- read_csv(content)

  # 按列对数据进行规范化
  dat_f <- dat %>% mutate(across(c(2:ncol(dat)), z_value))

  # 按列对数据进行加权规范化
  dat_z <-mutate(dat_f, w_value(dat_f[,c(2:ncol(dat))],weight))

  # unlist 转换tibble为vector
  z_positive <- en_positive(dat_z[,c(2:ncol(dat_z))],attribute)
  z_negative <- en_negative(dat_z[,c(2:ncol(dat_z))],attribute)

  # dat_z %>% select(2:ncol(dat_z))) %>% rowwise() %>% mutate(du = dist(., z_max), dn= dist(., z_min))
  du <- dist(dat_z, z_positive)
  dn <- dist(dat_z, z_negative)

  # 计算RI并按照降序排序
  dat_z %>% add_column(du = du, dn = dn) %>%
    mutate(Ri= dn/(du+dn)) %>%
    arrange(-Ri)
  return (dat_z %>% add_column(du = du, dn = dn) %>%
            mutate(Ri= dn/(du+dn)) %>%
            arrange(-Ri))
}

