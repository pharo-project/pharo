Issue 154
setUpConfigurationOfMetacelloExampledkh1
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfMetacelloExample-dkh.1'.
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
        selector: 'baselineVersion10MetacelloExample:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10MetacelloExample:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version10MetacelloExample:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10MetacelloExample:') asString)}.
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
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo