baselines
baseline13: spec
    <version: '1.3-baseline'>
    spec
        for: #'squeakCommon'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'ftp://ftp.example.com/examples' ] ]