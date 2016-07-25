scripting
asProjectRegistration
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ 
      ^ MetacelloProjectRegistration
        fromMCBaselineProjectSpec: self asBaselineProjectSpec ].
  ^ MetacelloProjectRegistration
    fromMCConfigurationProjectSpec: self asConfigurationProjectSpec