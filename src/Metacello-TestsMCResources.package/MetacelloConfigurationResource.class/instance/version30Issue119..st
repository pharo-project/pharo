Issue 119
version30Issue119: spec
    <version: '3.0' imports: #('3.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]