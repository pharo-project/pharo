accessing
printOn: aStream
  aStream
    nextPutAll: self class name asString;
    nextPut: $(.
  self versionSpec printOn: aStream.
  aStream nextPut: $)