library(tidyverse)

df1 <- read.csv('data/earphenotypesformatted.csv')
df2 <- read.csv('data/earphenotypesformatted_part2.csv') %>%
  mutate(`Ear..` = as.numeric(`Ear..`))

# Concatenate the two dataframes
ears <- bind_rows(df1, df2)
# Group by plot
plots <- ears %>%
  mutate(QR.Code = str_to_upper(QR.Code)) %>%
  group_by(QR.Code) %>%
  mutate(earNum = 1:n()) %>%
  filter(earNum <= 4) %>%
  pivot_wider(id_cols = QR.Code, names_from = earNum, 
               values_from = c(starts_with('Kernel'), starts_with('Cob'), Ear.Width, Ear.Weight, X100.Kernel.weight)) %>%
  select(!starts_with('Kernel.Color'))
setsOf3 <- matrix(data = c(2, 3, 4, 1, 3, 4, 1, 2, 3), nrow = 3, byrow = TRUE)
setsOf2 <- matrix(data = c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4), nrow = 6, byrow = TRUE)

traits <- c('Ear.Width', 'Kernel.Fill.Length', 'Kernel.Row.Number', 'Kernels.per.Row', 'Ear.Weight', 'Kernel.Count', 'Cob.Width', 'Cob.Length', 'Cob.Weight', 
            'X100.Kernel.weight')

result <- c()
for(i in traits)
{
  for(j in 1:length(plots$QR.Code))
  {
    x3 <- 1:3
    x2 <- 1:6
    samp3 <- sample(x3, 3)
    samp2 <- sample(x2, 6)
    
    for (k in 1:2)
    {
      vec <- samp3[k, ]
      vars <- paste0(i, vec)
      mu <- paste0(i, 'mu', k, sep = '.')
      se <- paste0(i, 'se', k, sep = '.')
      'result3{i}':= plots %>%
        
    }
  }
  
}