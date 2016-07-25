locking
lockConfiguration12: spec
  <version: '1.2.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration12: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          "bleedingEdge version"
          spec repository: 'dictionary://Metacello_Config_Test_Repository' ] ]