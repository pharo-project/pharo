issue 63
configuration095Issue185: spec
  <version: '0.9.5'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration095Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA2
                    , '/repository' ];
        import: 'External' ]