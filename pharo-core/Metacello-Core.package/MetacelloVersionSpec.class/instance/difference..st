querying
difference: otherVersionSpec
    "Return a dictionary of additions, removals and modifications"

    | report myProjectSpecs otherProjectSpecs |
    report := MetacelloVersionDiffReport new.
    myProjectSpecs := Dictionary new.
    self
        projectDo: [ :projectSpec | myProjectSpecs at: projectSpec name put: projectSpec ]
        packageDo: [ :ignored |  ]
        groupDo: [ :ignored |  ].
    otherProjectSpecs := Dictionary new.
    otherVersionSpec
        projectDo: [ :projectSpec | otherProjectSpecs at: projectSpec name put: projectSpec ]
        packageDo: [ :ignored |  ]
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
    ^ report