-- state ~ Initializing SetupState
setup ...
-> NextTrial (Time.now)
-> TimeAndRandomProfile (Random.generate `distModel`)
-> TimeAndRandomProfileAndX
    (state = initExperiment `curState` time profile x)
-- state ~ Running ExperimentState
-> SlashOrZPressed
-> InitTrial (Time.now)
    ( initTime = Time.now
    )
-> RandomDelay (Random.generate (Random.float 150.0 1000.0))
    ( initialDelay = delay
    )
-> Process.sleep
-> StartTrial (Time.now)
    ( currentTrial = startTrial time
    , cue = True
    )
-> Process.sleep (cueDuration)
-> HideCue (Time.now)
    ( cue  = False
    , cueHideTime = Time.now
    )
    -> Process.sleep (`trialDuration` - cueDuration)
-> ShowTarget (Time.now)
    ( targetVisible = Time.now
    )

-> SpacePressed
    -> LaunchRocket (Time.now)
        ( launchTime = Time.now
        )

-> NextTrial (Time.now)
-> TimeAndRandomProfile (time, Random.generate profile)
-> TimeAndRandomProfileAndX (time, Random.generate profile, Random.generate Random.float 0 1)

-- SPEEDRUN VERSION

-- state ~ Initializing SetupState
setup ...
-> NextTrial (Time.now)
-> TimeAndRandomProfile (Random.generate `distModel`)
-> TimeAndRandomProfileAndX
    (state = initExperiment `curState` time profile x)
-- state ~ Running ExperimentState
-> SlashOrZPressed
-> InitTrial (Time.now)
    ( initTime = Time.now
    )
-> RandomDelay (Random.generate (Random.float 150.0 1000.0))
    ( initialDelay = delay
    )
-> Process.sleep
-> StartTrial (Time.now)
    ( currentTrial = startTrial time
    , cue = True
    )
-> Process.sleep (cueDuration)
-> HideCue (Time.now)
    ( cue  = False
    , cueHideTime = Time.now
    )
    -> Process.sleep (`trialDuration` - cueDuration)
-> ShowTarget (Time.now)
    ( targetVisible = Time.now
    )

-> SpacePressed
    -> LaunchRocket (Time.now)
        ( launchTime = Time.now
        )
