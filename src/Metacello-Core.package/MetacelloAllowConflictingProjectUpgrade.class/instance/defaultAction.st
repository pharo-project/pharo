exception handling
defaultAction
  self existingProjectRegistration locked
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/263"
      ^ self useIncoming ].
  ^ MetacelloConflictingProjectError
    signal:
      'Load Conflict between existing ' , self existingProjectRegistration printString
        , ' and ' , self newProjectRegistration printString