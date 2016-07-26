api options
onConflictUseIncoming: incomingProjects useLoaded: loadedProjects
  self
    onConflict: [ :ex :loaded :incoming | 
      (incomingProjects includes: incoming baseName)
        ifTrue: [ ex useIncoming ]
        ifFalse: [ 
          (loadedProjects includes: incoming baseName)
            ifTrue: [ ex useLoaded ] ].
      ex pass ]