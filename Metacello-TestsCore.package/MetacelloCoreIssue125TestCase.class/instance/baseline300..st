baselines
baseline300: spec
    <version: '3.0-baseline' imports: #('2.0-baseline')>
    spec
        for: #(#'attribute1' #'attribute2')
        do: [ 
            spec
                project: 'Extra Project'
                    with: [ 
                            spec
                                className: 'ConfigurationOfExtraProject';
                                versionString: '1.0-baseline';
                                preLoadDoIt: #'preloadForProject';
                                postLoadDoIt: #'postloadForProject';
                                loads: #('core');
                                yourself ];
                yourself.
            spec group: 'Core' with: #('Extra Project') ]