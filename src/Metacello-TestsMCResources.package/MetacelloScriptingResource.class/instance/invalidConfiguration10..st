invalid configurations
invalidConfiguration10: spec
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration10: ... missing className:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]