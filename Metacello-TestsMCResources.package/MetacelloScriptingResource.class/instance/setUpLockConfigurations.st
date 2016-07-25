locking
setUpLockConfigurations
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfLockConfigurations-dkh.1'.
  className := #'ConfigurationOfLockConfigurations'.
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
    selector: 'lockConfiguration10:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration10:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration11:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration11:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration12:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration12:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration13:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration13:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration14:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration14:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration15:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration15:') asString).
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