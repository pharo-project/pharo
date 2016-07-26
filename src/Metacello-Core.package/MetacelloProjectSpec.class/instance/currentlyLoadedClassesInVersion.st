querying
currentlyLoadedClassesInVersion
    self versionOrNil ifNotNil: [ :vrsn | ^ vrsn currentlyLoadedClassesInVersion ].
    ^ #()