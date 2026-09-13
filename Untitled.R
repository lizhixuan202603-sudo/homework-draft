Q1
# 读取文件
# Mac/Linux 路径示例
hw1data <- read.csv("/Users/li-zhixuan/Desktop/R-script in Umich/draft for homework/forty+soybean+cultivars+from+subsequent+harvests/data.csv")

# 查看前几行
head(hw1data)

#b
# 1. 验证总观测数是否为 320
nrow(hw1data)

# 2. 验证不同品种 (Cultivar) 的数量是否为 40
length(unique(hw1data$Cultivar))

# 3. 验证季节 (Season) 的数量，并生成频数表
table(hw1data$Season)

library(dplyr)
install.packages("dplyr")

hw1data %>%
  count(Season)

#c
# 计算 PH 和 GY 的整体相关系数
cor(hw1data$PH, hw1data$GY)

# 按 Season 分组计算相关系数
tapply(hw1data$PH, hw1data$Season, function(x) cor(x, hw1data$GY[hw1data$Season == hw1data$Season[1]]))

hw1data %>%
  group_by(Season) %>%
  summarise(correlation = cor(PH, GY))

#3
hw1data$GY[which.max(hw1data$PH)]

# 计算 MHG 的平均值
mean_mhg <- mean(hw1data$MHG, na.rm = TRUE)

# 计算大于平均值的比例（百分比）
mean(hw1data$MHG > mean_mhg, na.rm = TRUE) * 100

#5. 平均籽粒产量（GY）最高的品种
hw1data %>%
  group_by(Cultivar) %>%
  summarise(avg_GY = mean(GY, na.rm = TRUE)) %>%
  arrange(desc(avg_GY)) %>%
  slice(1)


#d

# 计算整体和分季节的平均值

# 1. 分季节平均值（先把 Season 转成字符，方便和 Overall 合并）
season_means <- hw1data %>%
  group_by(Season) %>%
  summarise(across(c(PH, IFP, NLP, NGP, NGL, NS, MHG, GY), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(Season = as.character(Season))

# 2. 整体平均值
overall_means <- hw1data %>%
  summarise(across(c(PH, IFP, NLP, NGP, NGL, NS, MHG, GY), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(Season = "Overall")

# 3. 合并
bind_rows(season_means, overall_means)

#e
# 使用 R 自带的 t.test 函数
t.test(GY ~ Season, data = hw1data)


Q2.
# 从 UCI 直接读取数据
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
# 注意：adult.data 没有表头，需要手动指定列名
adult <- read.csv(url, header = FALSE, stringsAsFactors = FALSE)

colnames(adult) <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
)

# 查看数据
head(adult)
str(adult)

# 简化变量名（用下划线，去掉不必要的复杂名称）
colnames(adult) <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
)

# 检查变量名
names(adult)

# 注意：adult.data 中的缺失值通常编码为 " ?"，需要先处理
# 把所有 " ?" 替换成 NA
adult[adult == " ?"] <- NA

# 统计每个变量的缺失值数量
missing_count <- colSums(is.na(adult))
missing_count

# 过滤前样本量
n_before <- nrow(adult)
n_before

# 过滤：每周工作至少 10 小时
adult <- adult[adult$hours_per_week >= 10, ]

# 过滤后样本量
n_after <- nrow(adult)
n_after

# 删除这三个变量有缺失值的观测
adult <- adult[!is.na(adult$occupation) & !is.na(adult$workclass) & !is.na(adult$native_country), ]

# 确认样本量
nrow(adult)

#为什么要删除？
#occupation、workclass 和 native_country 是分类变量，缺失值无法用均值或中位数合理填补。
#这三个变量在后续分析（如收入与职业、工作类型、国籍的关系）中很重要。
#缺失比例较小（合计约 7%），删除后不会严重损失样本量。
#删除可以避免在建模时因缺失值导致错误或偏差。

# 删除年龄小于 18、大于 90 的观测
adult <- adult[adult$age >= 18 & adult$age <= 90, ]

# 删除 education 为 Preschool 的观测
adult <- adult[adult$education != " Preschool", ]

# 最终样本量
n_final <- nrow(adult)
n_final

Q3

#a.
nextHappy <- function(n){
  if(!is.numeric(n) || length(n) != 1 || is.na(n)){
    stop("Input must be a single numeric value.")
  }
  if(n != as.integer(n)){
    stop("Input must be an integer.")
  }
  if(n <= 0){
    stop("Input must be a positive integer.")
  }
  
  digits <- as.integer(strsplit(as.character(n), "")[[1]])
  
  return(sum(digits^2))
}
nextHappy(19)   # 1^2 + 9^2 = 1 + 81 = 82
nextHappy(82)   # 8^2 + 2^2 = 64 + 4 = 68


#b
happySequence <- function(n) {
  # 输入验证
  if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
    stop("Input must be a single numeric value.")
  }
  if (n != as.integer(n)) {
    stop("Input must be an integer.")
  }
  if (n <= 0) {
    stop("Input must be a positive integer.")
  }
  
  seq_vals <- c(n)   # 存储序列
  seen <- c(n)       # 用于检测循环
  
  # 如果起始值就是 1，直接返回
  if (n == 1) {
    return(list(sequence = 1, isHappy = TRUE))
  }
  
  repeat {
    next_val <- nextHappy(seq_vals[length(seq_vals)])
    seq_vals <- c(seq_vals, next_val)
    
    # 到达 1，happy number
    if (next_val == 1) {
      return(list(sequence = seq_vals, isHappy = TRUE))
    }
    
    # 检测到循环，unhappy number
    if (next_val %in% seen) {
      return(list(sequence = seq_vals, isHappy = FALSE))
    }
    
    seen <- c(seen, next_val)
  }
}

happySequence(19)
happySequence(4)


#c
# 对 1 到 1000 每个数判断是否 happy
results <- sapply(1:1000, function(x) happySequence(x)$isHappy)

# 有多少个 happy numbers
num_happy <- sum(results)
num_happy

# 百分比
pct_happy <- mean(results) * 100
pct_happy

# 哪个起始值的序列最长（包括 happy 和 unhappy）
seq_lengths <- sapply(1:1000, function(x) length(happySequence(x)$sequence))
max_length <- max(seq_lengths)
start_with_max <- which(seq_lengths == max_length)
max_length
start_with_max

