baselines
baseline20: spec
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF8'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: #'stable';
                        loads: #('Misc-UTF8') ].
            spec
                project: 'UTF9'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryOtherConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF8') ].
            spec
                group: 'Core' with: #('UTF8' 'UTF9');
                group: 'Tests' with: #('UTF8');
                group: 'Recursive' with: #('Core' 'Recursive')	"recursive group definition" ]