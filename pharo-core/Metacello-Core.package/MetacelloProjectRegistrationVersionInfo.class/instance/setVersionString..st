accessing
setVersionString: anObject
  "set versionString, only if it does not already have avalue"

  versionString ifNil: [ self versionString: anObject ]