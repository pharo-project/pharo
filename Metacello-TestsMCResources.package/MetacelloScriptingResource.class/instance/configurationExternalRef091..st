baseline pragma - github reference
configurationExternalRef091: spec
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'version'.
      spec
        description: 'MetacelloScriptingResource>>configurationExternalRef090:'.
      spec
        project: 'External'
        with: [ 
          spec
            className: 'ConfigurationOfExternal';
            version: '0.9.1';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]