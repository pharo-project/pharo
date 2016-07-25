baseline:with:
setUpConfigurationOfConflict
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfConflict-dkh.1'.
  className := #'ConfigurationOfConflict'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf20:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf20:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf21:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf21:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf10:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf10:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf11:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf11:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf12:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf12:') asString).
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
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo