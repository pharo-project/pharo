external configurations
setUpConfigurationOfExternalXdkh2: ancestors
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalX-dkh.2'.
  className := #'ConfigurationOfExternalX'.
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
    selector: 'stableVersionOfX:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersionOfX:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'unstableVersionOfX:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'unstableVersionOfX:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfX090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfX090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfX091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfX091:') asString).
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
            ancestors: ancestors)
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo