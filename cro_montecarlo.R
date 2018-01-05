GenerateConversionPairs <- function(samples, traffic, conversion.rate, lift){
  rbind(
    rbinom(samples, traffic, conversion.rate),
    rbinom(samples, traffic, conversion.rate * (1 + lift))
  )
}

CalculatePValue <- function(conversion.pair, traffic){
  preparedMatrix = rbind(conversion.pair, c(traffic - conversion.pair[1], traffic - conversion.pair[2]))
  return(chisq.test(preparedMatrix, correct=FALSE)$p.value)
}

CalculateTrafficRequirement <- function(conversion.rate,
                                        lift,
                                        p.value.threshold=0.05,
                                        samples=5000,
                                        lower.traffic.bound=50
                                       ) {

  print(paste("Calculating traffic requirements: Conversion Rate =", conversion.rate, ", Lift =", lift))
  CalculatePercentBelowPValueThreshold <- function(traffic) {
    conversion.pairs = GenerateConversionPairs(samples, traffic, conversion.rate, lift)
    sample.p.values = apply(conversion.pairs, 2, CalculatePValue, traffic)
    percent.below.threshold = sum(sample.p.values < p.value.threshold) / samples
  }

  OneSidedBinarySearch <- function(lower.traffic.bound) {
    percent.below.threshold = CalculatePercentBelowPValueThreshold(lower.traffic.bound * 2)
    print(paste("Percent of samples below threshold:", percent.below.threshold, ",", lower.traffic.bound * 2))
    if (percent.below.threshold <= (1 - p.value.threshold)) {
      print("Upper bound not detected. Doubling traffic threshold.")
      OneSidedBinarySearch(lower.traffic.bound * 2)
    } else if (percent.below.threshold > (1 - p.value.threshold)) {
      print(paste("Upper bound detected. Initiating two-sided binary search between", lower.traffic.bound, "and", lower.traffic.bound * 2))
      TwoSidedBinarySearch(lower.traffic.bound, lower.traffic.bound * 2)
    } else {
      stop("One-sided binary search boundry calculations failed.")
    }
  }

  TwoSidedBinarySearch <- function(lower.traffic.bound, upper.traffic.bound) {
    halfway.traffic.bound = round((lower.traffic.bound + upper.traffic.bound) / 2)
    percent.below.threshold = CalculatePercentBelowPValueThreshold(halfway.traffic.bound)
    print(paste("Executing two-sided binary search iteration. Traffic bounds between", lower.traffic.bound, "and", upper.traffic.bound))
    print(paste("Percent of samples below threshold:", percent.below.threshold, halfway.traffic.bound))
    if (percent.below.threshold > (1 - (p.value.threshold * 1.05)) & percent.below.threshold < (1 - (p.value.threshold * 0.95))) {
      print(paste("Traffic requirement detected:", halfway.traffic.bound))
      return(halfway.traffic.bound)
    } else if (percent.below.threshold < (1 - p.value.threshold)) {
      print("UP")
      TwoSidedBinarySearch(halfway.traffic.bound, upper.traffic.bound)
    } else if (percent.below.threshold > (1 - p.value.threshold)) {
      print("DOWN")
      TwoSidedBinarySearch(lower.traffic.bound, halfway.traffic.bound)
    } else {
      stop("Two-sided binary search boundry calculations failed.")
    }
  }

  print("Initiating one-sided binary search.")
  OneSidedBinarySearch(lower.traffic.bound)
}

lift.values = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2)
lift.traffic.requirements = sapply(lift.values, CalculateTrafficRequirement, conversion.rate = 0.1)
plot(lift.values, lift.traffic.requirements)
