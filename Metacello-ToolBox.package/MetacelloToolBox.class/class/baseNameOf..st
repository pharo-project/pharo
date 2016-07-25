utility
baseNameOf: configurationClassName
  "Return the baseName for the given configuration class name."

  <apiDocumentation>
  ^ (configurationClassName beginsWith: 'ConfigurationOf')
    ifTrue: [ 
      configurationClassName
        copyFrom: 'ConfigurationOf' size + 1
        to: configurationClassName size ]
    ifFalse: [ configurationClassName ]