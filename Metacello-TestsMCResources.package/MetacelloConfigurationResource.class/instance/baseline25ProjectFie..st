ConfigurationOfProjectFie
baseline25ProjectFie: spec
  "self reset"

  <version: '2.5'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Foo'
        with: [ 
          spec
            className: 'MetacelloTestConfigurationOfFoo';
            versionString: '3.5';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]