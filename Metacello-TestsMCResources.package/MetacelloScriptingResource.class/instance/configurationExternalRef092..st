baseline pragma - github reference
configurationExternalRef092: spec
  <version: '0.9.2'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'version'.
      spec
        description: 'MetacelloScriptingResource>>configurationExternalRef092:'.
      spec
        project: 'External'
        with: [ 
          "ConfigurationOfExternal 0.9.2 should not exist"
          spec
            className: 'ConfigurationOfExternal';
            version: '0.9.2';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]