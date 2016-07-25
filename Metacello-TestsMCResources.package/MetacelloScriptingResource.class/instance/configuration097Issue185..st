issue 63
configuration097Issue185: spec
  <version: '0.9.7'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration0957Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA4
                    , '/repository' ];
        import: 'External' ]