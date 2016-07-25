issue 32
version10Issue47: spec
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>version10Issue47:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34'.
            spec baseline: 'Issue32' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]