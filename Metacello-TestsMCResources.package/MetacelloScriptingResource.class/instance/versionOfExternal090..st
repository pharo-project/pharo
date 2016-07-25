external configurations
versionOfExternal090: spec
  <version: '0.9.0'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>versionOfExternal090:'.
      spec repository: 'dictionary://Metacello_Configuration_Test_Repository'.
      spec
        package: 'External-Core' with: 'External-Core-dkh.1';
        yourself ]