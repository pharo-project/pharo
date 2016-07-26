api options
onDowngradeUseIncoming: projectNames
  self
    onDowngrade: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex useIncoming ]
        ifFalse: [ ex useLoaded ] ]