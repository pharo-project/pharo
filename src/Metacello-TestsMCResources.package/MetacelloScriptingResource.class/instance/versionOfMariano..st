baseline:with:
versionOfMariano: spec
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfMariano:'.
            spec author: 'dkh'.
            spec timestamp: '5/31/2012 16:04' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]