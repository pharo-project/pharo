Issue 125
baselineVersion30Issue125: spec
    <version: '3.0-baseline' imports: #('2.0-baseline')>
    spec
        for: #(#'attribute1' #'attribute2')
        do: [ 
            spec description: 'MetacelloConfigurationResource>>baselineVersion30Issue125:'.
            spec
                package: 'GoferFaux' with: [ spec requires: 'GoferFoo' ];
                package: 'GoferBeau' with: [ spec requires: 'GoferFaux' ];
                yourself ]