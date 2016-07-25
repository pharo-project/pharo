The **MetacelloMCBaselineProject**  is a wrapper for the **BaselineOf** version specification for file-based repositories.

There is a single version in a **MetacelloMCBaselineProject**, named *'baseline'*.

A typical **BaselineOf** is specification:

```Smalltalk
baseline: spec
    <baseline>
    spec
        package: 'External-Core';
        package: 'External-Tests' with: [ spec requires: 'External-Core' ];
        yourself.
    spec
        group: 'Core' with: #('External-Core');
        group: 'default' with: #('Core');
        group: 'Tests' with: #('External-Tests');
        yourself
```

The `<baseline>` pragma marks the method containing the baseline specification.