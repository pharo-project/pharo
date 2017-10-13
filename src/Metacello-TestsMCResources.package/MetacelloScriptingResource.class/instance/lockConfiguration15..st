locking
lockConfiguration15: spec
  <version: '1.5.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: self className , '>>lockConfiguration15: '.
      spec
        baseline: 'External'
        with: [ 
          spec
            repository:
              'github://dalehenrich/external:'
                , MetacelloScriptingResource externalCustomIssue185SHA
                , '/repository' ] ]