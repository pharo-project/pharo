Issue 125
baselineVersion20Issue125: spec
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec description: 'MetacelloConfigurationResource>>baselineVersion20Issue125:'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                package: 'GoferFoo';
                package: 'GoferBar' with: [ spec requires: 'GoferFoo' ];
                yourself ]