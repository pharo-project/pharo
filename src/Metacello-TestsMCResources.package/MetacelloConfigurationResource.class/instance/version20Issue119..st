Issue 119
version20Issue119: spec
    <version: '2.0' imports: #('2.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]