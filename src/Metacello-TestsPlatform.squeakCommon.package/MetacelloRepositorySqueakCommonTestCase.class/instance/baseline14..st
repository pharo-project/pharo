baselines
baseline14: spec
    <version: '1.4-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'http://example.com/examples' ] ]