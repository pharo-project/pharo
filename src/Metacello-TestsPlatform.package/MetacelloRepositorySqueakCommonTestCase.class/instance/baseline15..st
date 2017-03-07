baselines
baseline15: spec
    <version: '1.5-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'dictionary://Metacello_Platform_Test_GlobalDictionary' ] ]