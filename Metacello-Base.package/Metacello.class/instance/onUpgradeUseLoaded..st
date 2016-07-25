api options
onUpgradeUseLoaded: projectNames
  self
    onUpgrade: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex useLoaded ]
        ifFalse: [ ex useIncoming ] ]