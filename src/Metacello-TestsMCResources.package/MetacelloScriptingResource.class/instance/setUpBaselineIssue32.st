issue 32
setUpBaselineIssue32
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfIssue32-dkh.1'.
    className := #'BaselineOfIssue32'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'postloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'postloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'preloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineIssue32:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineIssue32:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    externalRepository
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