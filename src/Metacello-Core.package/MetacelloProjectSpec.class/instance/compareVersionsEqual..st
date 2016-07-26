scripting
compareVersionsEqual: aMetacelloProjectSpec
  | vrsn otherVrsn |
  vrsn := self versionOrNil.
  otherVrsn := aMetacelloProjectSpec versionOrNil.
  vrsn ifNil: [ ^ vrsn = otherVrsn ].
  otherVrsn ifNil: [ ^ false ].
  ^ vrsn versionNumber = otherVrsn versionNumber