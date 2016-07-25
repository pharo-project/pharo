ConfigurationOfProjectToolBox
setUpConfigurationOfProjectToolBox
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectToolBox-dkh.1'.
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
        selector: 'postloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'postloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'preloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'symbolicVersionExplicitlyDoesNotExistProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'symbolicVersionExplicitlyDoesNotExistProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version100ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version100ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version101ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version101ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version102ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version102ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version103ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version103ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version104ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version104ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version105ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version105ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version106ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version106ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version107ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version107ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version108ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version108ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version109ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version109ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version110ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version110ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version111ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version111ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version112ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version112ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion113ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion113ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version113ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version113ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version114ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version114ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version115ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version115ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version116ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version116ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version117ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version117ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version118ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version118ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version119ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version119ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version120ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version120ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version121ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version121ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version122ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version122ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version123ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version123ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion124ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion124ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version124ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version124ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version125ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version125ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version126ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version126ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version127ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version127ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version128ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version128ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version129ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version129ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version130ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version130ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version131ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version131ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version132ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version132ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version133ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version133ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version134ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version134ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version135ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version135ProjectToolBox:') asString)}.
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