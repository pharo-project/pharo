issue 63
setUpConfiguration181
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfIssue181-dkh.1'.
  className := #'ConfigurationOfIssue181'.
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
    selector: 'configuration091Issue181:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration091Issue181:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration092Issue181:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration092Issue181:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration093Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration093Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration094Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration094Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration095Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration095Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration096Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration096Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration097Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration097Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration098Issue215:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration098Issue215:') asString)}.
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