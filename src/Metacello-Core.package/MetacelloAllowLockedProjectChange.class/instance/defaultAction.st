exception handling
defaultAction
  Warning
    signal:
      'LOCK ENFORCED: Attempt to ' , self operationString printString
        , ' new project: ' , self newProjectRegistration printString printString
        , ' when existing project: '
        , self existingProjectRegistration printString printString
        , ' is locked. New project not loaded. Use #onLock: to intercept.'.
  ^ self disallow