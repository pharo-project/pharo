Issue 86
setUpConfigurationOfProjectIssue86
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue86-dkh.1'.
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
        source: self linearProjectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline60ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline60MethodSourceProjectIssue86).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline61ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline61MethodSourceProjectIssue86).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline62ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline62MethodSourceProjectIssue86)}.
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