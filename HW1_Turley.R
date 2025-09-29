# ------------------------------------------------------------
# HW1 – CSC-587-W1 Advanced Data Mining
# Student: Mike Turley

# Install once if needed, I already did this
# install.packages(c("tidyverse","ggplot2"))
library(tidyverse)
#library(ggplot2)

# Set a working directory :
setwd("~/Downloads/DATA/")

# 1) Su_raw_matrix.txt (30 pts)  -----------------------------
# Tasks: read.delim; mean/sd for Liver_2.CEL; colMeans/colSums
# (a) create variable su
su <- read.delim("~/Downloads/DATA/Su_raw_matrix.txt", header = TRUE, sep = "\t", check.names = TRUE)
str(su)

# (b) Mean/SD for Liver_2.CEL
# Use na.rm=TRUE for any missing values.
m_L2  <- mean(su$Liver_2.CEL, na.rm = TRUE)
sd_L2 <- sd(su$Liver_2.CEL,   na.rm = TRUE)
m_L2; sd_L2

# (c) Column-wise means/sums
cm <- colMeans(su[ , sapply(su, is.numeric)], na.rm = TRUE)
cs <- colSums (su[ , sapply(su, is.numeric)], na.rm = TRUE)
cm; cs

# ------------------------------------------------------------
# 2) rnorm histograms (20 pts) -------------------------------
# Generate n=10,000 for each (mean, sd), plot histograms, and
# briefly comment on the differences.  

set.seed(42)

n <- 10000

x1 <- rnorm(n, mean = 0, sd = 0.2)
x2 <- rnorm(n, mean = 0, sd = 0.5)

# Base R histograms with same xlim to visualize spread difference
png("hist_rnorm_sd0_2.png", width=900, height=600)
hist(x1, breaks=50, xlab="Value", main="N(0, 0.2^2) – n=10,000", xlim=c(-5,5))
dev.off()

png("hist_rnorm_sd0_5.png", width=900, height=600)
hist(x2, breaks=50, xlab="Value", main="N(0, 0.5^2) – n=10,000", xlim=c(-5,5))
dev.off()

# ------------------------------------------------------------
# 3) ggplot demos + diabetes mass by class (20 pts) ----------


# 3a) Sample dataframe
dat <- data.frame(
  cond   = factor(rep(c("A","B"), each=200)),
  rating = c(rnorm(200), rnorm(200, mean=.8))
)

# 3b) Overlaid histograms
png("gg_overlaid_hist_demo.png", width=900, height=600)
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  labs(title="Overlaid histograms (demo)")
dev.off()

# 3c) Interleaved histograms
png("gg_interleaved_hist_demo.png", width=900, height=600)
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, position="dodge") +
  labs(title="Interleaved histograms (demo)")
dev.off()

# 3d) Density plots (lines)
png("gg_density_lines_demo.png", width=900, height=600)
ggplot(dat, aes(x=rating, colour=cond)) + 
  geom_density() +
  labs(title="Density (lines) (demo)")
dev.off()

# 3e) Density plots with semi-transparent fill
png("gg_density_fill_demo.png", width=900, height=600)
ggplot(dat, aes(x=rating, fill=cond)) + 
  geom_density(alpha=.3) +
  labs(title="Density (fill) (demo)")
dev.off()

# 3f) Apply to diabetes_train.csv – mass by class
# Use the provided file; keep column names as-is.
diabetes <- read.csv("~/Downloads/DATA/diabetes_train.csv")
str(diabetes)

# Overlaid histograms: mass by class
png("diab_mass_overlaid_hist.png", width=900, height=600)
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") +
  labs(title="Diabetes: mass by class (overlaid)")
dev.off()

# Interleaved histograms
png("diab_mass_interleaved_hist.png", width=900, height=600)
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=1, position="dodge") +
  labs(title="Diabetes: mass by class (interleaved)")
dev.off()

# Density (lines)
png("diab_mass_density_lines.png", width=900, height=600)
ggplot(diabetes, aes(x=mass, colour=class)) + 
  geom_density() +
  labs(title="Diabetes: mass by class (density lines)")
dev.off()

# Density (fill)
png("diab_mass_density_fill.png", width=900, height=600)
ggplot(diabetes, aes(x=mass, fill=class)) + 
  geom_density(alpha=.3) +
  labs(title="Diabetes: mass by class (density fill)")
dev.off()

# ------------------------------------------------------------
# 4) titanic.csv piping tasks (20 pts) -----------------------
# Read titanic; run the five tidyverse operations and briefly describe

passengers <- read.csv("~/Downloads/DATA/titanic.csv")
str(passengers)

# 4a) Drop rows with any NA, then summary()
res_4a <- passengers %>% drop_na() %>% summary()
res_4a

# 4b) Filter males
res_4b <- passengers %>% filter(Sex == "male")
head(res_4b)

# 4c) Arrange by Fare descending
res_4c <- passengers %>% arrange(desc(Fare))
head(res_4c)

# 4d) New column: family size
res_4d <- passengers %>% mutate(FamSize = Parch + SibSp)
head(res_4d)

# 4e) Group by Sex; summarize mean Fare & number survived
res_4e <- passengers %>%
  group_by(Sex) %>%
  summarise(meanFare = mean(Fare, na.rm = TRUE),
            numSurv  = sum(Survived, na.rm = TRUE))
res_4e


write.csv(res_4e, "titanic_by_sex_summary.csv", row.names=FALSE)

# ------------------------------------------------------------
# 5) Percentiles of diabetes$skin (10 pts) -------------------
#  -wasn't sure if you wanted plots so experimented writing out files/plots  

q_skin <- quantile(diabetes$skin, probs = c(0.10, 0.30, 0.50, 0.60), na.rm = TRUE)
q_skin
write.csv(data.frame(percentile=c(10,30,50,60), value=as.numeric(q_skin)), "diabetes_skin_percentiles.csv", row.names=TRUE)


# Density (lines)
png("diabetes_skin_percentiles.png", width=900, height=600)
ggplot(diabetes, aes(x=mass, colour=class)) + 
  geom_density() +
  labs(title="Diabetes: mass by skin (density lines)")
dev.off()
