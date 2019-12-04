library('readxl');
library('EnvStats');
library('stats');

#  1 - Praha	
#  2 - Jihocesky	
#  3 - Jihomoravsky	
#  4 - Karlovarsky	
#  5 - Vysocina	
#  6 - Kralovehradecky	
#  7 - Liberecky	
#  8 - Moravskoslezky	
#  9 - Olomoucky	
# 10 - Pardubicky	
# 11 - Plzensky	
# 12 - Stredocesky	
# 13 - Ustecky	
# 14 - Zlinsky

my_data <- read_excel("data.xls", col_names = TRUE)

pha <- my_data$Praha;
vys <- my_data$Vysocina;
pce <- my_data$Pardubicky;


#aritmetický průměr
mean(pha);
mean(vys);
mean(pce);

#geometrický průměr
geoMean(pha, na.rm = FALSE);
geoMean(vys, na.rm = FALSE);
geoMean(pce, na.rm = FALSE);

#modus
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(pha);
getmode(vys);
getmode(pce);

#medián
median(pha, na.rm = FALSE);
median(vys, na.rm = FALSE);
median(pce, na.rm = FALSE);

#kvantily
quantile(pha);
quantile(vys);
quantile(pce);

#percentily
quantile(pha, probs = c(1, 5, 10, 90, 95, 99)/100);
quantile(vys, probs = c(1, 5, 10, 90, 95, 99)/100);
quantile(pce, probs = c(1, 5, 10, 90, 95, 99)/100);

#sešikmenost
skewness(pha);
skewness(vys);
skewness(pce);

#normální rozdělení
kurtosis(pha);
kurtosis(vys);
kurtosis(pce);

#GRAFY

#historogram


#bodovy
plot(pha);

#boxplot

boxplot(my_data,data=mtcars, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

summary(my_data);

