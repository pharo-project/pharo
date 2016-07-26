baseline pragma - github reference
setUpConfigurationExternalRefdkh1
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalRef-dkh.1'.
  className := #'ConfigurationOfExternalRef'.
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
    selector: 'configurationExternalRef090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef091:') asString)}.
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