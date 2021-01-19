# Conversion Rate Optimization Talk

These are the live code samples I used for a talk titled "Using Code to Beat Marketers at Their Own Game".

The subject of the talk was how I wrote a piece of code to demonstrate a very non-intuitive pair of principles A/B split testing:
* to detect increasingly smaller effect sizes you need quadratically larger traffic volumes
* if your traffic volume grows large enough, standard chi-squared statistical testing will increasingly give positive results for economically insignificant improvements

The code I wrote was a Monte Carlo simulation used to rapidly generate the distribution of traffic volume to minimum detectable effect sizes, and make the non-linear realationship therein transparently obvious.

[View the slides](https://docs.google.com/presentation/d/1lf1tHU4J2LVUj_AMmPqo-qEvq-iIXNjLyBUQhdrpCdg)
