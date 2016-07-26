importing
mergeImportLoads: aLoadList
    aLoadList
        ifNotNil: [ :otherLoads | self loads ifNil: [ loads := otherLoads ] ifNotNil: [ loads := loads , otherLoads ] ]