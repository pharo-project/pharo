invalid configurations
invalidConfiguration20: spec
    <version: '2.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration20: ... missing version:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]