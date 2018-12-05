# Read all datasets from local computer
Claims_cleaned <- read.csv("G:/My Drive/DSBA 6100 Big Data/Group Project/Midterm Due 11-4/Claims_cleaned.csv", stringsAsFactors=FALSE)
Trans.Part.1 <- read.csv("G:/My Drive/DSBA 6100 Big Data/Group Project/Midterm Due 11-4/Trans Part 1.csv", stringsAsFactors=FALSE)
Trans.Part.2 <- read.csv("G:/My Drive/DSBA 6100 Big Data/Group Project/Midterm Due 11-4/Trans Part 2.csv", stringsAsFactors=FALSE)

# Comine both transaction files and megrge with claim dataset
combined_trans <- rbind(Trans.Part.1,Trans.Part.2)
extended_frame<-merge(combined_trans, Claims_cleaned, by="ClaimIdentifier")

# Reformat Dateformate 
opendate <- as.Date(extended_frame$ClaimantOpenedDate,format = "%m/%d/%Y")
paymetndate <- as.Date(extended_frame$PaymentDate, "%d%b%y:%H:%M:%S")

# Get date processing days, month and day
extended_frame$Payment_Processing <- paymetndate - opendate
extended_frame$month_payment <- month(paymetndate)
extended_frame$year_payment <- year(paymetndate)

# Aggreate payment processing days and claim pending days 
aggraeted_pay_day <- aggregate(extended_frame$Payment_Processing, by=list(extended_frame$ClaimIdentifier), mean)
aggraeted_claim_day <- aggregate(extended_frame$Claim.Pending.Days, by=list(extended_frame$ClaimIdentifier), mean)

# Plot averaged payment processing days and claim pending days
plot(aggraeted_pay_day$x,type="l",col="red", main = "Payment Processing", xlab="ID", ylab="Payment Processing Days")

# Determine critical or non-critical preliminary results 
payamount_avg<-mean(extended_frame$PaymentAmount)
payprocess_avg<-mean(extended_frame$Payment_Processing)
extended_frame$criti_noncrite[extended_frame$PaymentAmount >= payamount_avg ] <- 1
extended_frame$criti_noncrite[extended_frame$PaymentAmount < payamount_avg ] <- 0

# Export into a csv file
write.csv(extended_frame, "G:/My Drive/DSBA 6100 Big Data/Group Project/Final Due 12-4/extended_claim.csv")
