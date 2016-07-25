scripting
compareEqual: aMetacelloProjectSpec
    | repositorySpecs anotherRepositorySpecs |
    repositorySpecs := (self map values sort: [ :a :b | a description <= b description ])
        collect: [ :each | each description ].
    anotherRepositorySpecs := (aMetacelloProjectSpec map values sort: [ :a :b | a description <= b description ])
        collect: [ :each | each description ].
    ^ repositorySpecs = anotherRepositorySpecs