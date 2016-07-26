accessing
projectSpecLookupBlock
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine baselineNameFrom: projectName)
        ifAbsent: [  ])} ]