initialization
primeRegisterLoadedProjectSpec: projectSpec
  MetacelloProjectRegistration
    registrationForProjectSpec: projectSpec
    ifAbsent: [ :new | 
      new
        loadedInImage: true;
        registerProject ]
    ifPresent: [ :existing :new | 
      existing
        copyOnWrite: [ :existingCopy | 
          existingCopy loadedInImage: true.
          existing locked
            ifFalse: [ 
              "lock takes precedence when priming from image"
              existingCopy merge: new ] ] ]