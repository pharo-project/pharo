ConfigurationOfAtomicFoo
setUpConfigurationOfAtomicFoo
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfAtomicFoo-dkh.1'.
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
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline25Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline25MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version20MethodSourceFoo)}.
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