mutability
copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock
  | copy |
  aMetacelloProjectRegistration
    configurationProjectSpecIfPresent: [ :spec | 
      copy := spec copy.
      aBlock value: copy.
      aMetacelloProjectRegistration configurationProjectSpec: copy ]
    ifAbsent: [ 
      aMetacelloProjectRegistration
        baselineProjectSpecIfPresent: [ :spec | 
          copy := spec copy.
          aBlock value: copy.
          aMetacelloProjectRegistration baselineProjectSpec: copy ]
        ifAbsent: [ aBlock value: nil ] ]