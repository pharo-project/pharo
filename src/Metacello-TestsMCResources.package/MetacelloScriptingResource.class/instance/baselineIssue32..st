issue 32
baselineIssue32: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineIssue32:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                preLoadDoIt: #'preloadDoIt';
                postLoadDoIt: #'postloadDoIt';
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ]