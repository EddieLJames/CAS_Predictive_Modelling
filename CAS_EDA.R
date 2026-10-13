# CAS Case EDA


raw <- read.csv("C:\\Users\\ELITEBOOK 840 G5\\OneDrive - The University of Texas at El Paso\\CAS Predictive Modelling\\Data\\06 - CAS Predictive Modeling Case Competition- Dataset.csv")
raw$cov <- c("Personal Property" = "PP", "Additional Living Expense" = "ALE",
             "Guest Medical" = "GM", "Liability" = "LIAB")[raw$coverage]


##some summary statistics
summary(raw)
summary(raw[raw$distance_to_campus!=0,])

raw$claim <- raw$claim_id > 0
head(raw)

# One row per student x coverage
md <- aggregate(cbind(n = claim, loss = amount) ~ student_id + cov + class + gpa + greek + study +
                  gender + distance_to_campus + sprinklered + risk_tier, data = raw, FUN = sum)
md$dist_band <- cut(md$distance_to_campus, c(-Inf, 0, 1, 3, 6, Inf),
                    labels = c("On campus", "Off 0-1mi", "Off 1-3mi", "Off 3-6mi", "Off 6+mi"))
md$off <- ifelse(md$dist_band == "On campus", "On campus", "Off campus")
md$gpa_band <- cut(md$gpa, c(0, 1, 2, 3, 4.01), right = FALSE,
                   labels = c("<1.0", "1.0-2.0", "2.0-3.0", "3.0+"))
summary(md)


# Total loss per student across all coverages
stu <- aggregate(loss ~ student_id + class + greek + off + sprinklered + risk_tier, data = md, FUN = sum)
head(stu)

claims <- raw[raw$claim, ]
head(claims)

# ---- Some Data facts  ----
length(unique(raw$student_id))              # 10,000 students
nrow(claims)                            # 1,819 claims
sum(claims$amount)                        # $8.51 M paid
sum(claims$amount) / 10000                    # $851 per student
mean(md$n == 0)                        # 96% of student-coverage rows have no claim

by_cov <- data.frame(
  claims  = tapply(md$n, md$cov, sum),
  freq    = tapply(md$n, md$cov, mean),
  avg_sev = tapply(claims$amount, claims$cov, mean),
  max     = tapply(claims$amount, claims$cov, max),
  pp      = tapply(md$loss, md$cov, sum) / 10000
)
by_cov$share <- by_cov$pp / sum(by_cov$pp)
round(by_cov, 3)                        

ggplot(data=by_cov, aes(x=rownames(by_cov), y=pp)) +
  geom_bar(stat="identity",fill="steelblue") +labs(title="Claims Cost per Student by Coverage",x="Coverage Type", y="Claims Cost per Student ($)") +
  geom_text(aes(label = round(pp,2)), vjust = -0.3, size = 4)+ theme_minimal()


# ---- 2. One policy year per student ----
# Class and GPA never change within a student, so each student is one year of exposure
max(tapply(raw$class, raw$student_id, function(x) length(unique(x)))) 
max(tapply(raw$gpa,   raw$student_id, function(x) length(unique(x))))   


# ---- 3. Large losses and the $50k cap ----
boxplot(amount ~ cov, data = claims, main = "Claim size by coverage ($)")
abline(h = 50000, col = "red", lty = 2)

table(claims$cov[claims$amount > 50000])  


# Excess load = uncapped / capped loss
sapply(split(claims$amount, claims$cov), function(x) sum(x) / sum(pmin(x, 50000)))


# ---- 4. Current tiers vs Greek x Distance since they had largest effect size----
tier_stu<-aggregate(loss ~ risk_tier, data = stu, FUN = function(x) c(mean = mean(x), n = length(x)))
tier_stu
greek_off<-aggregate(loss ~ greek + off, data = stu, FUN = function(x) c(mean = mean(x), n = length(x)))
greek_off$category<-paste(greek_off$greek, greek_off$off, sep = "\n")
greek_off

par(mfrow = c(1, 2))
barplot(tapply(stu$loss, stu$risk_tier, mean), ylim = c(0, 3600), main = "By tier (1.2x)")

barplot(sort(tapply(stu$loss, paste(stu$greek, stu$off, sep = "\n"), mean)), ylim = c(0, 3600),
       cex.names = 0.6, main = "Greek x location (10.7x)")
par(mfrow = c(1, 1))


# ---- 5. One-way view of each variable ----
# Frequency (claims per student-coverage), severity (avg claim) and loss per student-coverage
one_way <- function(v) {
  data.frame(rows = tapply(md$n, md[[v]], length),
             freq = tapply(md$n, md[[v]], mean),
             sev  = tapply(md$loss, md[[v]], sum) / tapply(md$n, md[[v]], sum),
             loss = tapply(md$loss, md[[v]], mean))
}
one_way("greek")
one_way("dist_band")
one_way("sprinklered")      
one_way("gpa_band")         
one_way("class")
one_way("study")            
one_way("gender")         


