querying
difference: otherVersionSpec
    "Return a dictionary of additions, removals and modifications"

    | report myProjectSpecs myPackageSpecs otherProjectSpecs otherPackageSpecs |
    report := MetacelloVersionDiffReport new.
    myProjectSpecs := Dictionary new.
    myPackageSpecs := Dictionary new.
    self
        projectDo: [ :projectSpec | myProjectSpecs at: projectSpec name put: projectSpec ]
        packageDo: [ :packageSpec | myPackageSpecs at: packageSpec name put: packageSpec ]
        groupDo: [ :ignored |  ].
    otherProjectSpecs := Dictionary new.
    otherPackageSpecs := Dictionary new.
    otherVersionSpec
        projectDo: [ :projectSpec | otherProjectSpecs at: projectSpec name put: projectSpec ]
        packageDo: [ :packageSpec | otherPackageSpecs at: packageSpec name put: packageSpec ]
        groupDo: [ :ignored |  ].
    myProjectSpecs
        valuesDo: [ :myProjectSpec | 
            | otherProjectSpec |
            otherProjectSpec := otherProjectSpecs at: myProjectSpec name ifAbsent: [  ].
            otherProjectSpec == nil
                ifTrue: [ 
                    report removals
                        at: myProjectSpec name
                        put:
                            {(myProjectSpec versionString).
                            ''} ]
                ifFalse: [ 
                    myProjectSpec versionString = otherProjectSpec versionString
                        ifFalse: [ 
                            report modifications
                                at: myProjectSpec name
                                put:
                                    {(myProjectSpec versionString).
                                    (otherProjectSpec versionString)} ] ] ].
    otherProjectSpecs
        valuesDo: [ :otherProjectSpec | 
            (myProjectSpecs at: otherProjectSpec name ifAbsent: [  ]) == nil
                ifTrue: [ 
                    report additions
                        at: otherProjectSpec name
                        put:
                            {''.
                            (otherProjectSpec versionString)} ] ].
    myPackageSpecs
        valuesDo: [ :myPackageSpec | 
            | otherPackageSpec |
            otherPackageSpec := otherPackageSpecs at: myPackageSpec name ifAbsent: [  ].
            otherPackageSpec == nil
                ifTrue: [ 
                    report removals
                        at: myPackageSpec name
                        put:
                            {(myPackageSpec file).
                            ''} ]
                ifFalse: [ 
                    myPackageSpec file = otherPackageSpec file
                        ifFalse: [ 
                            report modifications
                                at: myPackageSpec name
                                put:
                                    {(myPackageSpec file).
                                    (otherPackageSpec file)} ] ] ].
    otherPackageSpecs
        valuesDo: [ :otherPackageSpec | 
            (myPackageSpecs at: otherPackageSpec name ifAbsent: [  ]) == nil
                ifTrue: [ 
                    report additions
                        at: otherPackageSpec name
                        put:
                            {''.
                            (otherPackageSpec file)} ] ].
    ^ report