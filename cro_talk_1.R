
a.traffic = 10000
b.traffic = 10000

a.conversions = 1000
b.conversions = 1200

prepared.conversion.data = rbind(c(a.conversions, b.conversions), c(a.traffic, b.traffic))

p.value = chisq.test(prepared.conversion.data, correct=FALSE)$p.value

print(p.value)
