baseline:with:
conflictOf12: spec
    <version: '1.2.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>conflictOf12:'.
            spec author: 'dkh'.
            spec timestamp: '6/1/2012 14:46' ].
    spec
        for: #'custom'
        do: [ 
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        operator: #'=';
                        version: '0.9.2';
                        repository: 'dictionary://Metacello_Conflict_Test_Repository'	"#= forces conflict" ] ]