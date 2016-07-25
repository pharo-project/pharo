ConfigurationOfFoo
setUpIssue156ConfigurationOfGoo
  "https://github.com/dalehenrich/metacello-work/issues/156"

  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'ConfigurationOfGoo-dkh.1'.
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
    selector: 'baselineGoo300Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo300Issue156Configuration:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGoo400Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo400Issue156Configuration:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGoo500Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo500Issue156Configuration:') asString)}.
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