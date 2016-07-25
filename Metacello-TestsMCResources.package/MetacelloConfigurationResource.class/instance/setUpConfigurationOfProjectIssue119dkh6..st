Issue 119
setUpConfigurationOfProjectIssue119dkh6: ancestors
    "versions 1.0, 2.0. 3.0,4.0, 4.1, 4.2, 4.3, 4.4"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.6'.
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
        selector: 'baselineVersion10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version20Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version30Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version41Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version41Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version42Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version42Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version43Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version43Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version44Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version44Issue119:') asString)}.
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