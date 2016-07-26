ConfigurationOfProjectFum
baseline10ProjectFum: spec
    "self reset"

    <version: '1.0'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfFoo';
                                versionString: '5.0';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                project: 'Fie'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfProjectFie';
                                versionString: '2.5';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself ]