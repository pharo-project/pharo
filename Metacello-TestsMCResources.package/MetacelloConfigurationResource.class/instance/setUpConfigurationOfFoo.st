ConfigurationOfFoo
setUpConfigurationOfFoo
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFoo-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'bleedingEdgeVersion:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'bleedingEdgeVersion:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline30Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline30MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline35Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline35MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline40Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline40MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline50Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline50MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: self preloadDoItMethodSource)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())