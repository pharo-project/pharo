initialization
primeRegistryProjectSpecRegistered: projectSpec
  MetacelloProjectRegistration
    registrationForProjectSpec: projectSpec
    ifAbsent: [ :new | ^ false ]
    ifPresent: [ :existing :new | ^ true ]