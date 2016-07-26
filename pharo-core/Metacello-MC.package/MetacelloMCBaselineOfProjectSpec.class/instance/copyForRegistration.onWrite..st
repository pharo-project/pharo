mutability
copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock
  | copy |
  aMetacelloProjectRegistration
    baselineProjectSpecIfPresent: [ :spec | 
      copy := spec copy.
      aBlock value: copy.
      aMetacelloProjectRegistration baselineProjectSpec: copy ]
    ifAbsent: [ 
      aMetacelloProjectRegistration
        configurationProjectSpecIfPresent: [ :spec | 
          copy := spec copy.
          aBlock value: copy.
          aMetacelloProjectRegistration configurationProjectSpec: copy ]
        ifAbsent: [ aBlock value: nil ] ]