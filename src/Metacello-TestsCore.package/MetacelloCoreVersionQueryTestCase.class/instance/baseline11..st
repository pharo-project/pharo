baselines
baseline11: spec
    <version: '1.1-baseline' imports: #('1.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF7'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF7') ].
            spec
                group: 'Core' with: #('UTF7');
                group: 'Tests' with: #('UTF7');
                yourself ]