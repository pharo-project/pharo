baseline pragma - github reference
baselineGithubReferenceVI: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceVI:'.
            spec
                baseline: 'External Core'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ];
                baseline: 'External Tests'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Tests';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]