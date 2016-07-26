utilities
loadDefinitions
    | entries |
    definitions := OrderedCollection new.
    entries := packageDirectory entries.
    self
        addDefinitionFromFile: (entries detect: [ :entry | entry name beginsWith: 'categories' ] ifNone: [  ])
            inDirectory: packageDirectory;
        addClassAndMethodDefinitionsFromDirectory: packageDirectory;
        addDefinitionFromFile: (entries detect: [ :entry | entry name beginsWith: 'initializers' ] ifNone: [  ])
            inDirectory: packageDirectory