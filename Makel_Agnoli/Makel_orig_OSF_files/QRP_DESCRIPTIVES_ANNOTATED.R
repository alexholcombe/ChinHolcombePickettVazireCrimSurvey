#For this code, only "dplyr" is needed. 

data <- read.csv ("Full_Responses.csv")
library ("dplyr")

#This script will return descriptive statistics for all items. Note, there
#is forcing within this script that might not be necessary in future updates 
#to R or the dplyr package. The forcing used is through the use of the code
#"dplyr::"

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R3_2, na.rm = TRUE),
    sd = sd(R3_2, na.rm = TRUE)
  )


 dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R4_2, na.rm = TRUE),
    sd = sd(R4_2, na.rm = TRUE)
  )


  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R5_2, na.rm = TRUE),
    sd = sd(R5_2, na.rm = TRUE)
  )


  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R6_2, na.rm = TRUE),
    sd = sd(R6_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R7_2, na.rm = TRUE),
    sd = sd(R7_2, na.rm = TRUE)
  )


  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R8_2, na.rm = TRUE),
    sd = sd(R8_2, na.rm = TRUE)
  )


  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R9_2, na.rm = TRUE),
    sd = sd(R9_2, na.rm = TRUE)
  )


  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R10_2, na.rm = TRUE),
    sd = sd(R10_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R11_2, na.rm = TRUE),
    sd = sd(R11_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R12_2, na.rm = TRUE),
    sd = sd(R12_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R13_2, na.rm = TRUE),
    sd = sd(R13_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R14_2, na.rm = TRUE),
    sd = sd(R14_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R15_2, na.rm = TRUE),
    sd = sd(R15_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R16_2, na.rm = TRUE),
    sd = sd(R16_2, na.rm = TRUE)
  )

  dplyr::summarise(data,
    count = dplyr::n(),
    mean = mean(R17_2, na.rm = TRUE),
    sd = sd(R17_2, na.rm = TRUE)
  )

