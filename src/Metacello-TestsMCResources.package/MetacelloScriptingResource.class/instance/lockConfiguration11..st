locking
lockConfiguration11: spec
  <version: '1.1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration11: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: '0.9.1';
            repository: 'dictionary://Metacello_Config_Test_Repository' ] ]