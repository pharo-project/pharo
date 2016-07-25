Issue 171
baselineVersion21Issue171: spec
    <version: '2.1-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo' with: [ spec className: 'MetacelloTestConfigurationOfFoo' ];
                yourself.
            spec package: 'GeauxFoo' ]