issue 32
setUpConfigurationIssue32
    "see https://github.com/dalehenrich/metacello-work/issues/32"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue32-dkh.1'.
    className := #'ConfigurationOfIssue32'.
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
        selector: 'version10Issue47:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue47:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version09Issue32:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version09Issue32:') asString).
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