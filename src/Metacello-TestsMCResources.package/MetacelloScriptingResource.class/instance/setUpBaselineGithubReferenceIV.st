baseline pragma - github reference
setUpBaselineGithubReferenceIV
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefIV-dkh.1'.
    className := #'BaselineOfGithubRefIV'.
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
        selector: 'configurationGithubReferenceX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceX:') asString)}.
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