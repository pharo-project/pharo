issue 63
configuration094Issue185: spec
  <version: '0.9.4'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration094Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA1
                    , '/repository' ];
        import: 'External' ]