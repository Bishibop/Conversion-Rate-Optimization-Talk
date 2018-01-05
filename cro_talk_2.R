
CalculateTestSignificance <- function(total.traffic, base.conversion.rate, lift) {
  a.traffic = total.traffic / 2
  b.traffic = total.traffic / 2

  a.conversions = a.traffic * base.conversion.rate
  b.conversions = b.traffic * base.conversion.rate * (1 + lift)

  prepared.conversion.data = rbind(c(a.conversions, b.conversions), c(a.traffic, b.traffic))

  return(chisq.test(prepared.conversion.data, correct=FALSE)$p.value)
}

print(CalculateTestSignificance(1000000, 0.02, 0.1))
