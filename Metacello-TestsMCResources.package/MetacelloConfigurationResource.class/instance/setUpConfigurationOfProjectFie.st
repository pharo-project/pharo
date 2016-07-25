ConfigurationOfProjectFie
setUpConfigurationOfProjectFie
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectFie-dkh.1'.
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
        selector: 'baseline15ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline15ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline25ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline25ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline35ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline35ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline40ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline40ProjectFie:') asString)}.
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