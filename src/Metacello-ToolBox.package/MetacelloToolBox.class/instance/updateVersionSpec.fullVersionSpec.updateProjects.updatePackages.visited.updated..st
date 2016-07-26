private
updateVersionSpec: versionSpec fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages visited: visitedSpecs updated: updatedSpecs
    versionSpec
        specListProjectDo: [ :spec | 
            (updateProjects and: [ (visitedSpecs includes: spec name) not ])
                ifTrue: [ 
                    | fullProjectSpec |
                    fullProjectSpec := (fullVersionSpec packageNamed: spec name) projectReference.
                    (fullProjectSpec versionString ~~ nil
                        and: [ fullProjectSpec versionString ~~ #'stable' and: [ fullProjectSpec projectClass ~~ nil ] ])
                        ifTrue: [ 
                            | cv |
                            cv := fullProjectSpec projectClassProject currentVersion.
                            cv ~~ nil
                                ifTrue: [ 
                                    cv versionString ~= fullProjectSpec versionString
                                        ifTrue: [ 
                                            spec projectReference versionString: cv versionString.
                                            updatedSpecs add: spec name ] ]
                                ifFalse: [ 
                                    "set to an empty string as a place holder, since we know we should have a version, but we are unable to figure out the version"
                                    spec projectReference versionString: ''.
                                    updatedSpecs add: spec name ] ] ] ]
        packageDo: [ :spec | 
            (updatePackages and: [ (visitedSpecs includes: spec name) not ])
                ifTrue: [ 
                    spec getFile ~~ nil
                        ifTrue: [ 
                            | viName |
                            spec file = ''
                                ifTrue: [ spec file: spec name ].
                            (viName := spec workingCopyNameFor: spec loader) ~~ nil
                                ifTrue: [ 
                                    viName ~= spec file
                                        ifTrue: [ 
                                            spec file: viName.
                                            updatedSpecs add: spec name ].
                                    visitedSpecs add: spec name ] ] ] ]
        groupDo: [ :ignored |  ]