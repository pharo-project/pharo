issue 32
version09Issue32: spec
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>baselineOfIssue32:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]