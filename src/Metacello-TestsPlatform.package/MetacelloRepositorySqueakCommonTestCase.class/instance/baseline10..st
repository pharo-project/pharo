baselines
baseline10: spec
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: self directoryForPlatform ] ]