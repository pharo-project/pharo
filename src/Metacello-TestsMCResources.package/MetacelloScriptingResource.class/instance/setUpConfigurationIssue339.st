issue 339
setUpConfigurationIssue339
  "see https://github.com/dalehenrich/metacello-work/issues/339"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfIssue339-dkh.1'.
  className := #'ConfigurationOfIssue339'.
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
    selector: 'version100Issue339:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version100Issue339:') asString)}.
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