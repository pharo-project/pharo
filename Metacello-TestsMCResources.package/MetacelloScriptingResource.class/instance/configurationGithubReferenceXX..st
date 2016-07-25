baseline pragma - github reference
configurationGithubReferenceXX: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceXX:'.
            spec
                configuration: 'External'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]