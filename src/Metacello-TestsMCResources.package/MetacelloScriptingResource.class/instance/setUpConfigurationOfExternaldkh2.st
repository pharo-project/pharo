external configurations
setUpConfigurationOfExternaldkh2
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternal-dkh.2'.
  className := #'ConfigurationOfExternal'.
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
    selector: 'versionOfExternal090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfExternal090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfExternal091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfExternal091:') asString)}.
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