baseline:with:
conflictOf21: spec
  <version: '2.1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>conflictOf21:'.
      spec author: 'dkh'.
      spec timestamp: '6/1/2012 14:46' ].
  spec
    for: #'custom'
    do: [ 
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            operator: #'=';
            version: '0.9.1';
            repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]