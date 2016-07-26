baseline pragma - github reference
configurationGithubReferenceIV: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceIV:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]