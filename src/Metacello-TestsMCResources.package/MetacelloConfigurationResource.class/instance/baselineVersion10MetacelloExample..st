Issue 154
baselineVersion10MetacelloExample: spec
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec package: 'GoferFoo' ]