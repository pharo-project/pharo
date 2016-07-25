invalid configurations
setUpInvalidConfigurations
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfInvalidConfigurations-dkh.1'.
    className := #'ConfigurationOfInvalidConfigurations'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration10:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration10:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration20:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration20:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration30:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration30:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo