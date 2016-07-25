baseline pragma - external
externalBaselineX: spec
    <baseline>
    spec description: 'MetacelloScriptingResource>>externalBaselineX:'.
    spec
        package: 'External-CoreX';
        package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
        yourself.
    spec
        group: 'Core' with: #('External-CoreX');
        group: 'default' with: #('Core');
        group: 'Tests' with: #('External-TestsX');
        yourself