accessing
projectSpecLookupBlock
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine configurationNameFrom: projectName)
        ifAbsent: [  ])} ]