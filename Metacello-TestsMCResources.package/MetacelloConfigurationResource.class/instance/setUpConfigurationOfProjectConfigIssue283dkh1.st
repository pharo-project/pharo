issue 283
setUpConfigurationOfProjectConfigIssue283dkh1
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfProjectConfigIssue283-dkh.1'.
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
    selector: 'projectVersion10Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectVersion10Issue283:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'projectVersion20Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectVersion20Issue283:') asString)}.
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
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo