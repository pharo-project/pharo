handlers
handleNotificationsForAction: actionBlock
  [ 
  [ 
  [ 
  actionBlock
    on:
      MetacelloLookupProjectSpec , MetacelloLookupProjectSpecForLoad
        , MetacelloProjectSpecLoadedNotification
        , MetacelloScriptEnsureProjectLoadedForDevelopment
        , MetacelloLookupBaselineSpecForEnsureLoad
    do: [ :ex | 
      "lookup and registration handlers need to be innermost set of handlers ...they may throw option notifications"
      ex handleResolutionFor: self ] ]
    on:
      MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
        , MetacelloAllowConflictingProjectUpgrade
    do: [ :ex | 
      "option handlers need to be outermost set of handlers ... last line of defense before users are involved"
      ex handleResolutionFor: self ] ]
    on: MetacelloAllowLockedProjectChange
    do: [ :ex | 
      "MetacelloAllowLockedProjectChange need to be outermost handler ... since it is signaled from second line of handlers"
      ex handleResolutionFor: self ] ]
    on: Warning
    do: [ :ex | 
      "Warning is absolute outermost handler"
      self handleWarning: ex ]