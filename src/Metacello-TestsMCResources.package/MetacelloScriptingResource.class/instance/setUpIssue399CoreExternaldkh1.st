issue 399
setUpIssue399CoreExternaldkh1
  "two packages with same name and version number, but a different class created."

  "this on goes into the external repository"

  "add first package to the package cache, then load a baseline in repo with other package"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'Issue399-Core-dkh.1'.
  className := #'Issue399External'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '')}.
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