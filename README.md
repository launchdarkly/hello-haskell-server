# LaunchDarkly sample Haskell application

We've built a simple console application that demonstrates how LaunchDarkly's SDK works.

Below, you'll find the build procedure. For more comprehensive instructions, you can visit your [Quickstart page](https://app.launchdarkly.com/quickstart#/) or the [Haskell reference guide](https://docs.launchdarkly.com/sdk/server-side/haskell).

## Build instructions

1. Edit Main.hs and set the value of sdkKey to your LaunchDarkly SDK key. If there is an existing boolean feature flag in your LaunchDarkly project that you want to evaluate, set featureFlagKey to the flag key.

``` haskell
sdkKey :: Text
sdkKey = "1234567890abcdef"

featureFlagKey :: Text
featureFlagKey = "my-flag"
```

2. On the command line, run `stack run`

You should receive the message "Feature flag '<flag key>' is <true/false> for this user".
