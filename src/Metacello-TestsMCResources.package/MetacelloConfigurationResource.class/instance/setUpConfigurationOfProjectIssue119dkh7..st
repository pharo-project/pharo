Issue 119
setUpConfigurationOfProjectIssue119dkh7: ancestors
    "versions 5.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.7'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version50Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version50Issue119:') asString)}.
    monticelloRepository
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