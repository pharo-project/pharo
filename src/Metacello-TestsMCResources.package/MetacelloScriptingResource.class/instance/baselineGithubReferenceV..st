baseline pragma - github reference
baselineGithubReferenceV: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceV:'.
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]