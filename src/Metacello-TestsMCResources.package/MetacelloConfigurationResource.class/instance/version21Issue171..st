Issue 171
version21Issue171: spec
    <version: '2.1' imports: #('2.1-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1' ]