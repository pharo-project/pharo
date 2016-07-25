issue 283
setUpConfigurationOfProjectIssue283dkh2: ancestors
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfProjectIssue283-dkh.2'.
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
    selector: 'stableVersion283B:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersion283B:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'bleedingEdge206B:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'bleedingEdge206B:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue283:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version21Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version21Issue283:') asString)}.
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