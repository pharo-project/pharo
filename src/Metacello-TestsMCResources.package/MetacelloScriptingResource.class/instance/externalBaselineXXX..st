baseline pragma - external
externalBaselineXXX: spec
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>externalBaselineXXX:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ].
    spec
        for: #'custom'
        do: [ 
            spec package: 'External-UIX'.
            spec group: 'UI' with: #('External-UIX') ]