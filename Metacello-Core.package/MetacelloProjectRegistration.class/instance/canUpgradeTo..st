testing
canUpgradeTo: aProjectRegistration
  "true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and an upgrade is allowed"

  (self hasLoadConflicts: aProjectRegistration)
    ifFalse: [ ^ true ].
  configurationProjectSpec
    ifNotNil: [ 
      aProjectRegistration configurationProjectSpec
        ifNotNil: [ 
          configurationProjectSpec copy ensureProjectLoaded.
          ^ configurationProjectSpec
            canUpgradeTo: aProjectRegistration configurationProjectSpec ] ].
  baselineProjectSpec
    ifNotNil: [ 
      aProjectRegistration baselineProjectSpec
        ifNotNil: [ 
          baselineProjectSpec copy ensureProjectLoaded.
          ^ baselineProjectSpec
            canUpgradeTo: aProjectRegistration baselineProjectSpec ] ].
  ^ false