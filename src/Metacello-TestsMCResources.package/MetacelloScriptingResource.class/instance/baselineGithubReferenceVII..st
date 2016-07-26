baseline pragma - github reference
baselineGithubReferenceVII: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceVII:'.
            spec
                baseline: 'External Core'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ];
                project: 'External Tests' copyFrom: 'External Core' with: [ spec loads: 'Tests' ] ]