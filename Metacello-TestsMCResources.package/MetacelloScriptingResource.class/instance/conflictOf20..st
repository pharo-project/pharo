baseline:with:
conflictOf20: spec
  <version: '2.0.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>conflictOf20:'.
      spec author: 'dkh'.
      spec timestamp: '6/1/2012 14:46' ].
  spec
    for: #'custom'
    do: [ 
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: '0.9.0';
            repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]