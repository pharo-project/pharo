baseline pragma - external
externalBaselineXX: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>externalBaselineXX:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ]