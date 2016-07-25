issue 32
version10Issue59: spec
    <version: '1.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>version10Issue59:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34'.
            spec baseline: 'Issue32' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]