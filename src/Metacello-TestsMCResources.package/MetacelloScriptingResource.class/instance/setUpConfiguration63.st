issue 63
setUpConfiguration63
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue63-dkh.1'.
    className := #'ConfigurationOfIssue63'.
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
        selector: 'configuration091Issue63:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configuration091Issue63:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configuration092Issue63:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configuration092Issue63:') asString)}.
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