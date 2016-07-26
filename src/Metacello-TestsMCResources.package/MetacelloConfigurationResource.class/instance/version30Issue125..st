Issue 125
version30Issue125: spec
    <version: '3.0' imports: #('3.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloConfigurationResource>>version30Issue125:'.
            spec
                package: 'GoferFoo' with: 'GoferFoo-lr.4';
                package: 'GoferBar' with: 'GoferBar-jf.1';
                yourself ].
    spec
        for: #'attribute1'
        do: [ 
            spec
                package: 'GoferFaux' with: 'GoferFaux-tg.32';
                package: 'GoferBeau' with: 'GoferBeau-dkh.55';
                yourself ].
    spec
        for: #'attribute2'
        do: [ 
            spec
                package: 'GoferFaux' with: 'GoferFaux-tg.33';
                package: 'GoferBeau' with: 'GoferBeau-dkh.56';
                yourself ]