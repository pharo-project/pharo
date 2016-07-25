issue 63
configuration091Issue181: spec
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration091Issue181:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        import: 'External' ]