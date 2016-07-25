issue 215
setUpBaselineIssue215
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'BaselineOfIssue215-dkh.1'.
  className := #'BaselineOfIssue215'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'BaselineOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'postloadDoIt'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'postloadDoIt') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'preloadDoIt'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'preloadDoIt') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'baselineIssue215:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineIssue215:') asString).
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