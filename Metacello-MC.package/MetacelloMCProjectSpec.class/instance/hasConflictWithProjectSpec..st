testing
hasConflictWithProjectSpec: projectSpec
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ ^ projectSpec hasConflictWithBaselineSpec: self asBaselineProjectSpec ].
  ^ projectSpec
    hasConflictWithConfigurationSpec: self asConfigurationProjectSpec