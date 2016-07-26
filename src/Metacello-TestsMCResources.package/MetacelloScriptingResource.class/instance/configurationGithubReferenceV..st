baseline pragma - github reference
configurationGithubReferenceV: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceV:'.
            spec
                project: 'External'
                    with: [ 
                            spec
                                className: 'ConfigurationOfExternal';
                                version: '0.9.0';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ];
                project: 'External Tests' copyFrom: 'External' with: [ spec loads: 'Tests' ] ]