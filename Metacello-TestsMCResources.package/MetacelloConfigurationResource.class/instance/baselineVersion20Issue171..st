Issue 171
baselineVersion20Issue171: spec
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GeauxFoo' ]