CalculateTestSignificance <- function(total.traffic, base.conversion.rate, lift) {
  a.traffic = total.traffic / 2
  b.traffic = total.traffic / 2

  a.conversions = a.traffic * base.conversion.rate
  b.conversions = b.traffic * base.conversion.rate * (1 + lift)

  prepared.conversion.data = rbind(c(a.conversions, b.conversions), c(a.traffic, b.traffic))

  return(chisq.test(prepared.conversion.data, correct=FALSE)$p.value)
}

CalculateRequiredTraffic <- function(base.conversion.rate, lift) {
  initial.lower.traffic.bound = 0
  initial.upper.traffic.bound = 5000000000

  BinarySearch <- function(lower.traffic.bound, upper.traffic.bound) {
    new.traffic.guess = round((lower.traffic.bound + upper.traffic.bound) / 2)

    p.value = CalculateTestSignificance(new.traffic.guess, base.conversion.rate, lift)

    if ( p.value < 0.0495 ) {
      return(BinarySearch(lower.traffic.bound, new.traffic.guess))
    } else if ( p.value > 0.0505 ) (
      return(BinarySearch(new.traffic.guess, upper.traffic.bound))
    ) else if ( p.value >= 0.0495 && p.value <= 0.0505 ) {
      return(new.traffic.guess)
    } else {
      print("Something horrible went wrong.")
    }
  }

  return(BinarySearch(initial.lower.traffic.bound, initial.upper.traffic.bound))
}

lift.values = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2)
plot(lift.values, sapply(lift.values, CalculateRequiredTraffic, base.conversion.rate = 0.02))
