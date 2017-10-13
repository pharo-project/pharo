issue 399
setUpBaselineIssue399Cypress
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'BaselineOfIssue399Cypress-dkh.1'.
  className := #'BaselineOfIssue399Cypress'.
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
    selector: 'baselineIssue399:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineIssue399:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'projectClass'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectClass') asString)}.
  externalRepository
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