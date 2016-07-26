issue 63
configuration092Issue181: spec
  <version: '0.9.2'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration092Issue181:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue181SHA
                    , '/repository' ];
        import: 'External' ]