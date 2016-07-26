baseline pragma - github reference
setUpBaselineGithubReferenceVI
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefVI-dkh.1'.
    className := #'BaselineOfGithubRefVI'.
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
        selector: 'configurationGithubReferenceXXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceXXX:') asString)}.
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