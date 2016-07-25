querying
registrationForClassNamed: aClassName ifAbsent: absentBlock
  ^ self
    registrationForExactClassNamed: aClassName
    ifAbsent: [ 
      | baseName |
      baseName := MetacelloScriptEngine baseNameOf: aClassName.
      self configurationRegistry
        at: 'ConfigurationOf' , baseName
        ifPresent: [ :registration | ^ registration ].
      self baselineRegistry
        at: 'BaselineOf' , baseName
        ifPresent: [ :registration | ^ registration ].
      absentBlock value ]