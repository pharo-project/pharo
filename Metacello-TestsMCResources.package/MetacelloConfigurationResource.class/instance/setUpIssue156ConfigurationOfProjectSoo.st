ConfigurationOfFoo
setUpIssue156ConfigurationOfProjectSoo
  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'ConfigurationOfProjectSoo-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: className)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: className
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    selector: 'version10Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version10Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version11Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version11Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version30Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version30Issue156:') asString)}.
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