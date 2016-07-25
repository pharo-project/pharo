issue 84
version10Issue84: spec
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>version10Issue84:'.
            spec author: 'dkh'.
            spec timestamp: '7/24/2012 19:23'.
            spec
                configuration: 'NestedIssue84'
                with: [ 
                    spec
                        version: '1.0.0';
                        repository: 'dictionary://Metacello_Config_Test_Repository' ] ]