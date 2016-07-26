Issue 154
setUpConfigurationOfProjectIssue154dkh1
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfMetacelloProjectIssue154-dkh.1'.
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
        selector: 'baselineVersion10Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion11Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion11Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion12Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion12Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion13Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion13Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion20Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion30Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue154:') asString)}.
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