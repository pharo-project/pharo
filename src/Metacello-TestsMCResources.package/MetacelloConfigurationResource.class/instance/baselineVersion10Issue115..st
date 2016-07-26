Issue 115
baselineVersion10Issue115: spec
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                versionString: #'bleedingEdge';
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GeauxFoo' ]