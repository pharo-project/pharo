Issue 154
version10MetacelloExample: spec
    <version: '1.0' imports: #('1.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec package: 'GoferFoo' with: 'GoferFoo-lr.2' ]