private
editVersionSpecsForImport: constructor projectAttributes: projectAttributes active: activeSection fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages visited: visitedSpecs updated: updatedSpecs
    | fileSpecified encounteredPackageSpecs |
    fileSpecified := Bag new.
    encounteredPackageSpecs := Bag new.
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            "collect packageSpec names ... interested in singletons"
            methodSection versionSpec
                specListProjectDo: [ :spec |  ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs add: spec name.
                    (spec copy updateForSpawnMethod: spec)
                        ifFalse: [ fileSpecified add: spec name ] ]
                groupDo: [ :spec |  ] ].
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            | versionSpec attributePath packageSpecsToDelete |
            versionSpec := methodSection versionSpec.
            attributePath := methodSection attributePath.
            versionSpec updateForSpawnMethod: versionSpec.
            packageSpecsToDelete := OrderedCollection new.
            versionSpec
                specListProjectDo: [ :spec | spec updateForSpawnMethod: spec ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs remove: spec name.
                    (spec updateForSpawnMethod: spec)
                        ifTrue: [ 
                            "no file speced"
                            ((fileSpecified includes: spec name) or: [ encounteredPackageSpecs includes: spec name ])
                                ifTrue: [ 
                                    "already specified somewhere else, need to delete this spec from version spec OR we're deleting all but first occurence"
                                    packageSpecsToDelete add: spec ]
                                ifFalse: [ 
                                    "file hasn't been specified, so specify now"
                                    spec file: spec name.
                                    fileSpecified add: spec name ] ]
                        ifFalse: [ 
                            "file specified"
                            (fileSpecified occurrencesOf: spec name) > 1
                                ifTrue: [ 
                                    "multiple specifications for file ... delete all but first occurence"
                                    packageSpecsToDelete add: spec.
                                    fileSpecified remove: spec name ] ] ]
                groupDo: [ :spec | spec updateForSpawnMethod: spec ].
            packageSpecsToDelete do: [ :spec | versionSpec deleteSpec: spec ].
            activeSection
                ifTrue: [ 
                    self
                        updateVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages
                        visited: visitedSpecs
                        updated: updatedSpecs ]
                ifFalse: [ 
                    self
                        clearVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages ] ]