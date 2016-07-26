scripting
canUpgradeTo: aProjectSpec
  "cannot upgrade between baselines unless repositories can be upgraded"

  (self repositories canUpgradeTo: aProjectSpec repositories)
    ifTrue: [ ^ true ].
  ^ false