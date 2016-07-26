*metacello-mc
matchesMetacelloGoferPackage: aMetacelloGoferPackage
  | ref |
  (super matchesMetacelloGoferPackage: aMetacelloGoferPackage)
    ifFalse: [ ^ false ].
  ref := self class name: aMetacelloGoferPackage packageFilename.
  ref versionNumber = 0
    ifTrue: [ ^ true ].
  ^ self versionNumber = ref versionNumber