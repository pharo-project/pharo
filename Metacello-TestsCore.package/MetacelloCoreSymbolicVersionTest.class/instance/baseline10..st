baselines
baseline10: spec
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'Example Project'
                with: [ 
                    spec
                        className: 'MetacelloExampleProjectConfig';
                        versionString: '1.0-baseline';
                        preLoadDoIt: #'preloadForProject';
                        postLoadDoIt: #'postloadForProject';
                        yourself ] ]