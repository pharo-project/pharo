accessing
setImport: anArray
  importArray := importArray
    ifNil: [ anArray ]
    ifNotNil: [ importArray , anArray ]