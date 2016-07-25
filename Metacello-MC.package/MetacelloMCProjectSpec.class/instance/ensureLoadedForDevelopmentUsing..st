loading
ensureLoadedForDevelopmentUsing: mcLoader
  "for #development projects, always need latest version of package when contemplating a load"

  | ensured |
  ensured := mcLoader ensuredMap at: self name ifAbsent: [ nil ].
  self projectClass ~~ nil
    ifTrue: [ 
      | vrsn |
      vrsn := self versionOrNil.
      vrsn ~~ nil
        ifTrue: [ 
          (self ensureConfigurationLoaded: vrsn ensured: ensured)
            ifTrue: [ 
              mcLoader ensureForDevelopment
                ifTrue: [ 
                  | pc |
                  (pc := self projectClass) ~~ nil
                    ifTrue: [ 
                      MetacelloClearStackCacheNotification
                        signal:
                          #(#'currentVersion' #'currentVersionAgainst:' #'currentVersionInfo' #'versionConstructor' #'loadableSpecNames')
                            , {pc} ].
                  self ensureLoadUsing: mcLoader ]
                ifFalse: [ self projectPackage fetchUsing: mcLoader ].
              mcLoader ensuredMap at: self name put: #'latest' ].
          ^ self ] ].
  ensured == nil
    ifTrue: [ 
      "projectClass == nil or version == nil"
      mcLoader ensureForDevelopment
        ifTrue: [ 
          | pc |
          (pc := self projectClass) ~~ nil
            ifTrue: [ 
              MetacelloClearStackCacheNotification
                signal:
                  #(#'currentVersion' #'currentVersionAgainst:' #'currentVersionInfo' #'versionConstructor' #'loadableSpecNames')
                    , {pc} ].
          self ensureLoadUsing: mcLoader ]
        ifFalse: [ self fetchUsing: mcLoader ].
      mcLoader ensuredMap at: self name put: #'present' ]