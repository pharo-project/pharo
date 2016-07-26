api-configuration
configurationNamed: baseName
  | configurationName |
  "Check if the class does not exist"
  configurationName := self configurationNameFrom: baseName.
  (Smalltalk includesKey: configurationName asSymbol)
    ifFalse: [ 
      self ensureMetacello.	"Create the configuration class"
      MetacelloPlatform current
        copyClass:
          (MetacelloPlatform current globalNamed: #'MetacelloConfigTemplate')
        as: configurationName asSymbol
        inCategory: configurationName asString.	"Create the package that has the same name"
      MetacelloPlatform current registerPackageNamed: configurationName ].
  project := (MetacelloPlatform current globalNamed: configurationName asSymbol)
    project