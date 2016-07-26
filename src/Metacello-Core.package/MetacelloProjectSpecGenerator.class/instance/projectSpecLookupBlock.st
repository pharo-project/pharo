accessing
projectSpecLookupBlock
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine baselineNameFrom: projectName)
        ifAbsent: [  ]).
    (MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine configurationNameFrom: projectName)
        ifAbsent: [  ])} ]