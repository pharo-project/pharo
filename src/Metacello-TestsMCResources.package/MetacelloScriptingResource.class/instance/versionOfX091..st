external configurations
versionOfX091: spec
    <version: '0.9.1'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfX091:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]