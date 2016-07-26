ConfigurationOfAtomicFan
setUpConfigurationOfAtomicFan
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfAtomicFan-dkh.1'.
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
        selector: 'baseline20Fan:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFan).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline31Fan:'
        category: 'cat'
        timeStamp: ''
        source: self baseline31MethodSourceFan).
    (MCMethodDefinition
        className: className asString
        selector: 'postLoad31baseline'
        category: 'cat'
        timeStamp: ''
        source: self postLoad31baselineMethodSource)}.
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