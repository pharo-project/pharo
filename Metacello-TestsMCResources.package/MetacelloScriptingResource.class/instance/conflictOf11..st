baseline:with:
conflictOf11: spec
    <version: '1.1.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>conflictOf11:'.
            spec author: 'dkh'.
            spec timestamp: '6/1/2012 14:46' ].
    spec
        for: #'custom'
        do: [ 
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]