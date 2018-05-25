Emails <- read.csv("C:/Users/subha/Documents/Spring Semester/Analytics Edge/EnronEmails.csv", stringsAsFactors = FALSE) 




EmailsShort = gsub("Subject:.*$", "", Emails$Text)

EmailsShort


EmailsShort = gsub("Subject:.*$", "", Emails$Text)
mean(nchar(EmailsShort))

matches = regexpr("Date:.*From:", EmailsShort)

subStrings = regmatches(EmailsShort, matches)
subStrings
mean(nchar(subStrings))

pattern = "Date: |\nFrom:"
matches = regexpr(pattern, EmailsShort)
matches
subStrings = regmatches(EmailsShort, matches)
subStrings
