invalid configurations
invalidConfiguration30: spec
    <version: '3.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration30: ... missing repository:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        version: '0.9.0' ] ]