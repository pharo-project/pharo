issue 84
setUpConfigurationNextedIssue84dkh2: ancestors
    "see https://github.com/dalehenrich/metacello-work/issues/84"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfNestedIssue84-dkh.2'.
    className := #'ConfigurationOfNestedIssue84'.
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
        selector: 'version10NestedIssue84:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10NestedIssue84:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionNumberClass'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionNumberClass') asString).
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
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo