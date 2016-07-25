issue 63
configuration096Issue185: spec
  <version: '0.9.6'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration0956Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA3
                    , '/repository' ];
        import: 'External' ]