query
allStoresInto: instVarName of: aClass
    | coll |
    coll := OrderedCollection new.
    aClass
        withAllSubAndSuperclassesDo: [ :class | (class whichSelectorsStoreInto: instVarName) do: [ :sel | coll add: (self createMethodNamed: sel realParent: class) ] ].
    ^ coll