baselines
baseline10: spec
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF8'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF8') ].
            spec
                group: 'Core' with: #('UTF8');
                group: 'Tests' with: #('UTF8');
                group: 'Recursive' with: #('Core' 'Recursive')	"recursive group definition" ]