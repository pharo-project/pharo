ConfigurationOfAtomicFoo
setUpConfigurationOfLinearFoo
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfLinearFoo-dkh.1'.
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
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self linearProjectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version50Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version50MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version51Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version51MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline60Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline60MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline61Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline61MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline62Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline62MethodSourceFoo)}.
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