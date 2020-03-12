### LaunchDarkly Sample Haskell Server-side Application
We've built a simple console application that demonstrates how LaunchDarkly's SDK works. Below, you'll find the basic build procedure, but for more comprehensive instructions, you can visit your [Quickstart page](https://app.launchdarkly.com/quickstart).

##### Build instructions

The sample application is built using [stack](https://docs.haskellstack.org/en/stable/README/).

1. Copy your SDK key and feature flag key from your LaunchDarkly dashboard into `app/Main.hs`
2. Run `stack build`
3. Run `stack exec hello`
