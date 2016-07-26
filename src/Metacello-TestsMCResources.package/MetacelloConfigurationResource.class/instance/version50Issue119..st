Issue 119
version50Issue119: spec
    <version: '5.0' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '1.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]