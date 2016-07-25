Issue 119
version44Issue119: spec
    <version: '4.4' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: 2.0.
            spec package: 'GoferBar ' with: 'GoferBar-jf.1' ]