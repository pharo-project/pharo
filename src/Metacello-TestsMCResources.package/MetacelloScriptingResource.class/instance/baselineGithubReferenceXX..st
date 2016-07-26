baseline pragma - github reference
baselineGithubReferenceXX: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceXX:'.
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]