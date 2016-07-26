api options
onLockBreak: projectNames
  self
    onLock: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex break ]
        ifFalse: [ ex honor ] ]