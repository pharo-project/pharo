external configurations
versionOfXX: spec
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfXX:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ 
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]